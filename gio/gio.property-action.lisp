;;; ----------------------------------------------------------------------------
;;; gio.property-action.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.64 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;; GPropertyAction
;;;
;;;     A GAction reflecting a GObject property
;;;
;;; Types and Values
;;;
;;;     GPropertyAction
;;;
;;; Functions
;;;
;;;     g_property_action_new
;;;
;;; Properties
;;;
;;;        gboolean    enabled           Read
;;;        gboolean    invert-boolean    Read / Write / Construct Only
;;;           gchar*   name              Read / Write / Construct Only
;;;         GObject*   object            Write / Construct Only
;;;    GVariantType*   parameter-type    Read
;;;           gchar*   property-name     Write / Construct Only
;;;        GVariant*   state             Read
;;;    GVariantType*   state-type        Read
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GPropertyAction
;;;
;;; Implemented Interfaces
;;;
;;;     GPropertyAction implements GAction.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GPropertyAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GPropertyAction" g-property-action
  (:superclass g-object
   :export t
   :interfaces ("GAction")
   :type-initializer "g_property_action_get_type")
  ((enabled
    g-property-action-enabled
    "enabled" "gboolean" t nil)
   (invert-boolean
    g-property-action-invert-boolean
    "invert-boolean" "gboolean" t nil)
   (name
    g-property-action-name
    "name" "gchararray" t nil)
   (object
    g-property-action-object
    "object" "GObject" nil nil)
   (parameter-type
    g-property-action-parameter-type
    "parameter-type" "GVariantType" t nil)
   (property-name
    g-property-action-property-name
    "property-name" "gchararray" nil nil)
   (state
    g-property-action-state
    "state" "GVariant" t nil)
   (state-type
    g-property-action-state-type
    "state-type" "GVariantType" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-property-action 'type)
 "@version{2020-4-26}
  @begin{short}
    A @sym{g-property-action} is a way to get a @class{g-action} with a state
    value reflecting and controlling the value of a GObject property.
  @end{short}

  The state of the action will correspond to the value of the property.
  Changing it will change the property, assuming the requested value matches
  the requirements as specified in the GParamSpec.

  Only the most common types are presently supported. Booleans are mapped to
  booleans, strings to strings, signed/unsigned integers to int32/uint32 and
  floats and doubles to doubles.

  If the property is an enum then the state will be string-typed and conversion
  will automatically be performed between the enum value and \"nick\" string as
  per the GEnumValue table.

  Flags types are not currently supported.

  Properties of object types, boxed types and pointer types are not supported
  and probably never will be.

  Properties of GVariant types are not currently supported.

  If the property is boolean-valued then the action will have a NULL parameter
  type, and activating the action (with no parameter) will toggle the value of
  the property.

  In all other cases, the parameter type will correspond to the type of the
  property.

  The general idea here is to reduce the number of locations where a particular
  piece of state is kept and therefore has to be synchronised between.
  @sym{g-property-action} does not have a separate state that is kept in sync
  with the property value -- its state is the property value.

  For example, it might be useful to create a @class{g-action} corresponding to
  the \"visible-child-name\" property of a @class{gtk-stack} so that the current
  page can be switched from a menu. The active radio indication in the menu is
  then directly determined from the active page of the @class{gtk-stack}.

  An anti-example would be binding the \"active-id\" property on a
  @class{gtk-combo-box}. This is because the state of the combobox itself is
  probably uninteresting and is actually being used to control something else.

  Another anti-example would be to bind to the \"visible-child-name\" property
  of a @class{gtk-stack} if this value is actually stored in GSettings. In that
  case, the real source of the value is GSettings. If you want a
  @class{g-action} to control a setting stored in GSettings, see
  @code{g_settings_create_action()} instead, and possibly combine its use with
  @code{g_settings_bind()}.
  @see-slot{g-property-action-enabled}
  @see-slot{g-property-action-invert-boolean}
  @see-slot{g-property-action-name}
  @see-slot{g-property-action-object}
  @see-slot{g-property-action-parameter-type}
  @see-slot{g-property-action-property-name}
  @see-slot{g-property-action-state}
  @see-slot{g-property-action-state-type}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-property-action-enabled ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enabled" 'g-property-action) 't)
 "The @code{enabled} property of type @code{:boolean} (Read) @br{}
  If the action is currently enabled. If the action is disabled then calls to
  the functions @fun{g-action-activate} and @fun{g-action-change-state} have no
  effect. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-enabled 'function)
 "@version{2020-4-26}
  @syntax[]{(g-property-action-enabled object) => enabled}
  @argument[object]{a @class{g-simple-action} object}
  @argument[enabled]{a boolean whether the action is enabled}
  @begin{short}
    Accessor of the @slot[g-property-action]{enabled} slot of the
    @class{g-property-action} class.
  @end{short}

  If the action is currently enabled. If the action is disabled then calls to
  the functions @fun{g-action-activate} and @fun{g-action-change-state} have
  no effect. @br{}
  @see-class{g-property-action}
  @see-function{g-action-activate}
  @see-function{g-action-change-state}")

;;; --- g-property-action-invert-boolean ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invert-boolean"
                                               'g-property-action) 't)
 "The @code{invert-boolean} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  If @em{true}, the state of the action will be the negation of the property
  value, provided the property is boolean. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-invert-boolean atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-invert-boolean 'function)
 "@version{2020-4-27}
  @syntax[]{(g-property-action-invert-boolean object) => invert}
  @argument[object]{a @class{g-property-action} object}
  @argument[invert]{a boolean whether the state of the action will be the
    negation}
  @begin{short}
    If @em{true}, the state of the action will be the negation of the property
    value, provided the property is boolean.
  @end{short}
  @see-class{g-property-action}")

;;;--- g-property-action-name --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'g-property-action) 't)
 "The @code{name} property of type @code{g-string}
  (Read / Write / Construct Only) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g-action-map} object. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-name 'function)
 "@version{2020-4-27}
  @syntax[]{(g-property-action-name object) => name}
  @argument[object]{a @class{g-property-action} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    The name of the action. This is mostly meaningful for identifying the
    action once it has been added to a @class{g-action-map} object.
  @end{short}
  @see-class{g-property-action}
  @see-class{g-action-map}")

;;; --- g-property-action-object -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "object" 'g-property-action) 't)
 "The @code{object} property of type @class{g-object}
  (Write / Construct Only) @br{}
  The object to wrap a property on. The object must be a GObject with
  properties. @br{}
  @em{Note:} In the Lisp binding this property is not readable and not
  writeable.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-object atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-object 'function)
 "@version{2020-4-27}
  @begin{short}
    The @slot[g-property-action]{object} slot of the @class{g-property-action}
    class is not readable and not writable.
  @end{short}
  @see-class{g-property-action}")

;;; --- g-property-action-parameter-type ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "parameter-type"
                                               'g-property-action) 't)
 "The @code{parameter-type} property of type @class{g-variant-type} (Read) @br{}
  The type of the parameter that must be given when activating the action.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-parameter-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-parameter-type 'function)
 "@version{2020-4-28}
  @syntax[]{(g-property-action-parameter-type object) => type}
  @argument[object]{a @class{g-property-action} object}
  @argument[type]{the @class{g-variant-type} of the parameter}
  @begin{short}
    The type of the parameter that must be given when activating the action.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
  (defvar button (make-instance 'gtk-button))
=> BUTTON
  (defvar action (g-property-action-new \"action\" button \"xalign\"))
=> ACTION
  (g-property-action-parameter-type action)
=> #<G-VARIANT-TYPE {10023AE493@}>
    @end{pre}
  @end{dictionary}
  @see-class{g-property-action}
  @see-class{g-variant-type}")

;;; --- g-property-action-property-name ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "property-name"
                                               'g-property-action) 't)
 "The @code{property-name} property of type @code{g-string}
  (Write / Construct only) @br{}
  The name of the property to wrap on the object. The property must exist on
  the passed-in object and it must be readable and writable and not
  construct-only. @br{}
  @em{Note:} In the Lisp binding this property is not readable und not
  writeable.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-property-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-property-name 'function)
 "@version{2020-4-28}
  @begin{short}
    The @slot[g-property-action]{property-name} slot of the
    @class{g-property-action} class is not readable and not writable.
  @end{short}
  @see-class{g-property-action}")

;;; --- g-property-action-state ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state"
                                               'g-property-action) 't)
 "The @code{state} property of type @type{g-variant} (Read) @br{}
  The state of the action, or @code{nil} if the action is stateless. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-state 'function)
 "@version{2020-4-28}
  @syntax[]{(g-property-action-state object) => state}
  @argument[object]{a @class{g-property-action} object}
  @argument[state]{the @type{g-variant} state of the action}
  @begin{short}
    The state of the action, or @code{nil} if the action is stateless.
  @end{short}
  @see-class{g-property-action}
  @see-type{g-variant}")

;;; --- g-property-action-state-type -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-type"
                                               'g-property-action) 't)
 "The @code{state-type} property of type @class{g-variant-type} (Read) @br{}
  The variant type of the state that the action has, or @code{nil} if the
  action is stateless.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-property-action-state-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-property-action-state-type 'function)
 "@version{2020-4-28}
  @syntax[]{(g-property-action-state-type object) => type}
  @argument[object]{a @class{g-property-action} object}
  @argument[type]{the @class{g-variant-type} state type of the action}
  @begin{short}
    The variant type of the state that the action has, or @code{nil} if the
    action is stateless.
  @end{short}
  @see-class{g-property-action}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; g_property_action_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_property_action_new" g-property-action-new)
    (g-object g-property-action)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-28}
  @argument[name]{a string with the name of the action to create}
  @argument[object]{a @class{g-oject} that has the property to wrap}
  @argument[property-name]{a string with the name of the property}
  @return{a new @class{g-property-action} object}
  @begin{short}
    Creates an action corresponding to the value of property @arg{property-name}
    on the object.
  @end{short}

  The property must be existent and readable and writable and not
  construct-only.
  @see-class{g-property-action}"
  (name g-string)
  (object g-object)
  (property-name g-string))

(export 'g-property-action-new)

;;; --- End of file gio.property-action.lisp -----------------------------------
