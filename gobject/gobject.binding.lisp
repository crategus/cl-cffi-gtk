;;; ----------------------------------------------------------------------------
;;; gobject.binding.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;; GBinding
;;;
;;;     Bind two object properties
;;;
;;; Types and Values
;;;
;;;     GBinding
;;;     GBindingFlags
;;;
;;; Functions
;;;
;;;     g_binding_get_source
;;;     g_binding_get_source_property
;;;     g_binding_get_target
;;;     g_binding_get_target_property
;;;     g_binding_get_flags
;;;     g_binding_unbind
;;;     g_object_bind_property
;;;     GBindingTransformFunc
;;;     g_object_bind_property_full
;;;     g_object_bind_property_with_closures
;;;
;;; Properties
;;;
;;;   GBindingFlags    flags              Read / Write / Construct Only
;;;         GObject*   source             Read / Write / Construct Only
;;;           gchar*   source-property    Read / Write / Construct Only
;;;         GObject*   target             Read / Write / Construct Only
;;;           gchar*   target-property    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GFlags
;;;     ╰── GBindingFlags
;;;
;;;     GObject
;;;     ╰── GBinding
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; --- g-binding-flags --------------------------------------------------------

(define-g-flags "GBindingFlags" g-binding-flags
  (:export t
   :type-initializer "g_binding_flags_get_type")
  (:default 0)
  (:bidirectional 1)
  (:sync-create 2)
  (:invert-boolean 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-binding-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'g-binding-flags atdoc:*external-symbols*)
 "@version{2020-10-18}
  @begin{short}
    Flags to be passed to the functions @fun{g-object-bind-property} or
    @fun{g-object-bind-property-full}.
  @end{short}

  This enumeration can be extended at later date.
  @begin{pre}
(define-g-flags \"GBindingFlags\" g-binding-flags
  (:export t
   :type-initializer \"g_binding_flags_get_type\")
  (:default 0)
  (:bidirectional 1)
  (:sync-create 2)
  (:invert-boolean 4))
  @end{pre}
  @begin[code]{table}
    @entry[:default]{The default binding. If the source property changes, the
      target property is updated with its value.}
    @entry[:bidirectional]{Bidirectional binding. If either the property of the
      source or the property of the target changes, the other is updated.}
    @entry[:sync-create]{Synchronize the values of the source and target
      properties when creating the binding. The direction of the synchronization
      is always from the source to the target.}
    @entry[:invert-bool]{If the two properties being bound are booleans, setting
      one to @em{true} will result in the other being set to @code{false} and
      vice versa. This flag will only work for boolean properties, and cannot be
      used when passing custom transformation functions to the function
      @fun{g-object-bind-property-full}.}
  @end{table}
  @see-class{g-binding}
  @see-function{g-object-bind-property}
  @see-function{g-object-bind-property-full}")

;;; --- g-binding --------------------------------------------------------------

(define-g-object-class "GBinding" g-binding
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_binding_get_type")
  ((flags
    g-binding-flags
    "flags" "GBindingFlags" t t)
   (source
    g-binding-source
    "source" "GObject" t t)
   (source-property
    g-binding-source-property
    "source-property" "gchararray" t t)
   (target
    g-binding-target
    "target" "GObject" t t)
   (target-property
    g-binding-target-property
    "target-property" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-binding 'type)
 "@version{2020-10-18}
  @begin{short}
    @sym{g-binding} is the representation of a binding between a property on a
    @class{g-object} instance, or source, and another property on another
    @class{g-object} instance, or target.
  @end{short}
  Whenever the source property changes, the same value is applied to the target
  property. For instance, the following binding:
  @begin{pre}
g_object_bind_property (object1, \"property-a\",
                        object2, \"property-b\",
                        G_BINDING_DEFAULT);
  @end{pre}
  will cause the property named @code{property-b} of @code{object2} to be
  updated every time the @fun{g-object-set} function or the specific accessor
  changes the value of the property @code{property-a} of @code{object1}.

  It is possible to create a bidirectional binding between two properties of two
  @class{g-object} instances, so that if either property changes, the other is
  updated as well, for instance:
  @begin{pre}
g_object_bind_property (object1, \"property-a\",
                        object2, \"property-b\",
                        G_BINDING_BIDIRECTIONAL);
  @end{pre}
  will keep the two properties in sync.

  It is also possible to set a custom transformation function, in both
  directions, in case of a bidirectional binding, to apply a custom
  transformation from the source value to the target value before applying it.
  For instance, the following binding:
  @begin{pre}
g_object_bind_property_full (adjustment1, \"value\",
                             adjustment2, \"value\",
                             G_BINDING_BIDIRECTIONAL,
                             celsius_to_fahrenheit,
                             fahrenheit_to_celsius,
                             NULL, NULL);
  @end{pre}
  will keep the @code{value} property of the two adjustments in sync. The
  function @code{celsius_to_fahrenheit} will be called whenever the @code{value}
  property of @code{adjustment1} changes and will transform the current value of
  the property before applying it to the @code{value} property of
  @code{adjustment2}.

  Vice versa, the function @code{fahrenheit_to_celsius} will be called whenever
  the @code{value} property of @code{adjustment2} changes, and will transform
  the current value of the property before applying it to the @code{value}
  property of @code{adjustment1}.

  Note that @sym{g-binding} does not resolve cycles by itself. A cycle like
  @begin{pre}
object1:propertyA -> object2:propertyB
object2:propertyB -> object3:propertyC
object3:propertyC -> object1:propertyA
  @end{pre}
  might lead to an infinite loop. The loop, in this particular case, can be
  avoided if the objects emit the \"notify\" signal only if the value has
  effectively been changed. A binding is implemented using the \"notify\"
  signal, so it is susceptible to all the various ways of blocking a signal
  emission, like the functions @fun{g-signal-stop-emission} or
  @fun{g-signal-handler-block}.

  A binding will be severed, and the resources it allocates freed, whenever
  either one of the @class{g-object} instances it refers to are finalized, or
  when the @sym{g-binding} instance loses its last reference.

  Bindings for languages with garbage collection can use the function
  @fun{g-binding-unbind} to explicitly release a binding between the source and
  target properties, instead of relying on the last reference on the binding,
  source, and target instances to drop.
  @see-slot{g-binding-flags}
  @see-slot{g-binding-source}
  @see-slot{g-binding-source-property}
  @see-slot{g-binding-target}
  @see-slot{g-binding-target-poperty}")

;;; ----------------------------------------------------------------------------
;;; Propery and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-binding-flags --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "flags" 'g-binding) 't)
 "The @code{flags} property of type @symbol{g-binding-flags}
  (Read / Write / Construct Only) @br{}
  Flags to be used to control the binding.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-binding-flags atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-binding-flags 'function)
 "@version{2020-10-18}
  @syntax[]{(g-binding-flags object) => flags}
  @argument[object]{a @class{g-binding} object}
  @argument[flags]{the @symbol{g-binding-flags} value used by the binding}
  @begin{short}
    Accessor of the @slot[g-binding]{flags} slot of the @class{g-binding} class.
  @end{short}

  Retrieves the flags passed when constructing the @class{g-binding} object.
  @see-class{g-binding}")

;;; --- g-binding-source -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "source" 'g-binding) 't)
 "The @code{source} property of type @class{g-object}
  (Read / Write / Construct Only) @br{}
  The object that should be used as the source of the binding.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-binding-source atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-binding-source 'function)
 "@version{2020-10-18}
  @syntax[]{(g-binding-source object) => source}
  @argument[object]{a @class{g-binding} object}
  @argument[source]{the @class{g-object} source}
  @begin{short}
    Accessor of the @slot[g-binding]{source} slot of the @class{g-binding}
    class.
  @end{short}

  Retrieves the @class{g-object} instance used as the source of the binding.
  @see-class{g-binding}")

;;; --- g-binding-source-property ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "source-property" 'g-binding) 't)
 "The @code{source-property} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the property of the source that should be used as the source of
  the binding.
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-binding-source-property atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-binding-source-property 'function)
 "@version{2020-10-18}
  @syntax[]{(g-binding-source-property object) => source-property}
  @argument[object]{a @class{g-binding} object}
  @argument[source-property]{a string with the name of the source property}
  @begin{short}
    Accessor of the @slot[g-binding]{source-property} slot of the
    @class{g-binding} class.
  @end{short}

  Retrieves the name of the property of the source used for the binding.
  @see-class{g-binding}")

;;; --- g-binding-target -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "target" 'g-binding) 't)
 "The @code{target} property of type @class{g-object}
  (Read / Write / Construct Only) @br{}
  The object that should be used as the target of the binding.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-binding-target atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-binding-target 'function)
 "@version{2020-10-18}
  @syntax[]{(g-binding-target object) => target}
  @argument[object]{a @class{g-binding} object}
  @argument[target]{the @class{g-object} target}
  @begin{short}
    Accessor of the @slot[g-binding]{target} slot of the @class{g-binding}
    class.
  @end{short}

  Retrieves the @class{g-object} instance used as the target of the binding.
  @see-class{g-binding}")

;;; --- g-binding-target-property ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "target-property" 'g-binding) 't)
 "The @code{target-property} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the property of the target that should be used for the binding.
  @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-binding-target-property atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-binding-target-property 'function)
 "@version{2020-10-18}
  @syntax[]{(g-binding-target-property object) => target}
  @argument[object]{a @class{g-binding} object}
  @argument[target-property]{a string with the name of the target property}
  @begin{short}
    Accessor of the @slot[g-binding]{target-property} slot of the
    @class{g-binding} class.
  @end{short}

  Retrieves the name of the property of the target used for the binding.
  @see-class{g-binding}")

;;; ----------------------------------------------------------------------------
;;; g_binding_unbind ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_binding_unbind" g-binding-unbind) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-10-18}
  @argument[binding]{a @class{g-binding} object}
  @begin{short}
    Explicitly releases the binding between the source and the target property
    expressed by binding .
  @end{short}

  This function will release the reference that is being held on the binding
  instance. If you want to hold on to the @class{g-binding} instance after
  calling the function @sym{g-binding-unbind}, you will need to hold a reference
  to it.
  @see-class{g-binding}"
  (binding (g-object g-binding)))

(export 'g-binding-unbind)

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_bind_property" g-object-bind-property) (g-object g-binding)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-18}
  @argument[source]{the source @class{g-object} instance}
  @argument[source-property]{a string with the property on @arg{source} to bind}
  @argument[target]{the target @class{g-object} instance}
  @argument[target-property]{a string with the property on @arg{target} to bind}
  @argument[flags]{a @symbol{g-binding-flags} value to pass to the binding}
  @begin{return}
    The @class{g-binding} instance representing the binding between the two
    @class{g-object} instances.
  @end{return}
  @begin{short}
    Creates a binding between @arg{source-property} on @arg{source} and
    @arg{target-property} on @arg{target}.
  @end{short}
  Whenever the @arg{source-property} is changed the @arg{target-property} is
  updated using the same value. For instance:
  @begin{pre}
(g-object-bind-property action \"active\" widget \"sensitive\" :default)
  @end{pre}
  Will result in the @code{sensitive} property of the widget @class{g-object}
  instance to be updated with the same value of the @code{active} property of
  the action @class{g-object} instance.

  If @arg{flags} contains @code{:bidirectional} then the binding will be
  mutual. If @arg{target-property} on @arg{target} changes then the
  @arg{source-property} on @arg{source} will be updated as well.

  The binding will automatically be removed when either the source or the
  target instances are finalized. To remove the binding without affecting the
  source and the target you can just call @code{g_object_unref()} on the
  returned @class{g-binding} instance.

  A @class{g-object} can have multiple bindings.
  @see-class{g-binding}
  @see-class{g-object}"
  (source g-object)
  (source-property :string)
  (target g-object)
  (target-property :string)
  (flags g-binding-flags))

(export 'g-object-bind-property)

;;; ----------------------------------------------------------------------------
;;; GBindingTransformFunc ()
;;;
;;; gboolean (*GBindingTransformFunc) (GBinding *binding,
;;;                                    const GValue *from_value,
;;;                                    GValue *to_value,
;;;                                    gpointer user_data);
;;;
;;; A function to be called to transform from_value to to_value . If this is the
;;; transform_to function of a binding, then from_value is the source_property
;;; on the source object, and to_value is the target_property on the target
;;; object. If this is the transform_from function of a G_BINDING_BIDIRECTIONAL
;;; binding, then those roles are reversed.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; from_value :
;;;     the GValue containing the value to transform
;;;
;;; to_value :
;;;     the GValue in which to store the transformed value
;;;
;;; user_data :
;;;     data passed to the transform function
;;;
;;; Returns :
;;;     TRUE if the transformation was successful, and FALSE otherwise
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property_full ()
;;;
;;; GBinding *
;;; g_object_bind_property_full (gpointer source,
;;;                              const gchar *source_property,
;;;                              gpointer target,
;;;                              const gchar *target_property,
;;;                              GBindingFlags flags,
;;;                              GBindingTransformFunc transform_to,
;;;                              GBindingTransformFunc transform_from,
;;;                              gpointer user_data,
;;;                              GDestroyNotify notify);
;;;
;;; Complete version of g_object_bind_property().
;;;
;;; Creates a binding between source_property on source and target_property on
;;; target , allowing you to set the transformation functions to be used by the
;;; binding.
;;;
;;; If flags contains G_BINDING_BIDIRECTIONAL then the binding will be mutual:
;;; if target_property on target changes then the source_property on source will
;;; be updated as well. The transform_from function is only used in case of
;;; bidirectional bindings, otherwise it will be ignored
;;;
;;; The binding will automatically be removed when either the source or the
;;; target instances are finalized. This will release the reference that is
;;; being held on the GBinding instance; if you want to hold on to the GBinding
;;; instance, you will need to hold a reference to it.
;;;
;;; To remove the binding, call g_binding_unbind().
;;;
;;; A GObject can have multiple bindings.
;;;
;;; The same user_data parameter will be used for both transform_to and
;;; transform_from transformation functions; the notify function will be called
;;; once, when the binding is removed. If you need different data for each
;;; transformation function, please use g_object_bind_property_with_closures()
;;; instead.
;;;
;;; source :
;;;     the source GObject.
;;;
;;; source_property :
;;;     the property on source to bind
;;;
;;; target :
;;;     the target GObject.
;;;
;;; target_property :
;;;     the property on target to bind
;;;
;;; flags :
;;;     flags to pass to GBinding
;;;
;;; transform_to :
;;;     the transformation function from the source to the target , or NULL to
;;;     use the default.
;;;
;;; transform_from :
;;;     the transformation function from the target to the source , or NULL to
;;;     use the default.
;;;
;;; user_data :
;;;     custom data to be passed to the transformation functions, or NULL
;;;
;;; notify :
;;;     a function to call when disposing the binding, to free resources used by
;;;     the transformation functions, or NULL if not required.
;;;
;;; Returns :
;;;     the GBinding instance representing the binding between the two GObject
;;;     instances. The binding is released whenever the GBinding reference count
;;;     reaches zero.
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property_with_closures ()
;;;
;;; GBinding *
;;; g_object_bind_property_with_closures (gpointer source,
;;;                                       const gchar *source_property,
;;;                                       gpointer target,
;;;                                       const gchar *target_property,
;;;                                       GBindingFlags flags,
;;;                                       GClosure *transform_to,
;;;                                       GClosure *transform_from);
;;;
;;; Creates a binding between source_property on source and target_property on
;;; target , allowing you to set the transformation functions to be used by the
;;; binding.
;;;
;;; This function is the language bindings friendly version o
;;; g_object_bind_property_full(), using GClosures instead of function pointers.
;;;
;;; source :
;;;     the source GObject.
;;;
;;; source_property :
;;;     the property on source to bind
;;;
;;; target :
;;;     the target GObject.
;;;
;;; target_property :
;;;     the property on target to bind
;;;
;;; flags :
;;;     flags to pass to GBinding
;;;
;;; transform_to :
;;;     a GClosure wrapping the transformation function from the source to the
;;;     target , or NULL to use the default
;;;
;;; transform_from :
;;;     a GClosure wrapping the transformation function from the target to the
;;;     source , or NULL to use the default
;;;
;;; Returns :
;;;     the GBinding instance representing the binding between the two GObject
;;;     instances. The binding is released whenever the GBinding reference count
;;;     reaches zero.
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; --- gobject.binding.lisp ---------------------------------------------------
