;;; ----------------------------------------------------------------------------
;;; gobject.binding.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.60 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; Functions
;;;
;;;     g_binding_get_source 
;;;     g_binding_get_source_property 
;;;     g_binding_get_target 
;;;     g_binding_get_target_property 
;;;     g_binding_get_flags 
;;;     g_binding_unbind 
;;;     g_object_bind_property 
;;;     (*GBindingTransformFunc) 
;;;     g_object_bind_property_full 
;;;     g_object_bind_property_with_closures 
;;;
;;; Properties
;;;
;;;     GBindingFlags  flags            Read / Write / Construct Only
;;;     GObject *      source           Read / Write / Construct Only
;;;     gchar *        source-property  Read / Write / Construct Only
;;;     GObject *      target           Read / Write / Construct Only
;;;     gchar *        target-property  Read / Write / Construct Only
;;;
;;; Types and Values
;;;
;;;     GBinding
;;;     GBindingFlags
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
 "@version{2019-3-12}
  @begin{short}
    Flags to be passed to @fun{g-object-bind-property} or
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
    @entry[:default]{The default binding; if the source property changes, the
      target property is updated with its value.}
    @entry[:bidirectional]{Bidirectional binding; if either the property of the
      source or the property of the target changes, the other is updated.}
    @entry[:sync-create]{Synchronize the values of the source and target
      properties when creating the binding; the direction of the synchronization
      is always from the source to the target.}
    @entry[:invert-bool]{If the two properties being bound are booleans, setting
      one to @em{true} will result in the other being set to @code{nil} and vice
      versa.
      This flag will only work for boolean properties, and cannot be used when 
      passing custom transformation functions to
      @fun{g-object-bind-property-full}.}
  @end{table}
  Since 2.26
  @see-class{g-binding}
  @see-function{g-object-bind-property}
  @see-function{g-object-bind-property-full}")

;;; --- g-binding --------------------------------------------------------------

(define-g-object-class "GBinding" g-binding
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_binding_get_type")
  ((flag
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
    "target-property" "gchararray" t t)
    ))

#|








Description

GBinding is the representation of a binding between a property on a GObject instance (or source) and another property on another GObject instance (or target). Whenever the source property changes, the same value is applied to the target property; for instance, the following binding:

g_object_bind_property (object1, "property-a",
                        object2, "property-b",
                        G_BINDING_DEFAULT);
will cause the property named "property-b" of object2 to be updated every time g_object_set() or the specific accessor changes the value of the property "property-a" of object1 .

It is possible to create a bidirectional binding between two properties of two GObject instances, so that if either property changes, the other is updated as well, for instance:

g_object_bind_property (object1, "property-a",
                        object2, "property-b",
                        G_BINDING_BIDIRECTIONAL);
will keep the two properties in sync.

It is also possible to set a custom transformation function (in both directions, in case of a bidirectional binding) to apply a custom transformation from the source value to the target value before applying it; for instance, the following binding:

g_object_bind_property_full (adjustment1, "value",
                             adjustment2, "value",
                             G_BINDING_BIDIRECTIONAL,
                             celsius_to_fahrenheit,
                             fahrenheit_to_celsius,
                             NULL, NULL);
will keep the "value" property of the two adjustments in sync; the celsius_to_fahrenheit function will be called whenever the "value" property of adjustment1 changes and will transform the current value of the property before applying it to the "value" property of adjustment2 .

Vice versa, the fahrenheit_to_celsius function will be called whenever the "value" property of adjustment2 changes, and will transform the current value of the property before applying it to the "value" property of adjustment1 .

Note that GBinding does not resolve cycles by itself; a cycle like

object1:propertyA -> object2:propertyB
object2:propertyB -> object3:propertyC
object3:propertyC -> object1:propertyA
might lead to an infinite loop. The loop, in this particular case, can be avoided if the objects emit the “notify” signal only if the value has effectively been changed. A binding is implemented using the “notify” signal, so it is susceptible to all the various ways of blocking a signal emission, like g_signal_stop_emission() or g_signal_handler_block().

A binding will be severed, and the resources it allocates freed, whenever either one of the GObject instances it refers to are finalized, or when the GBinding instance loses its last reference.

Bindings for languages with garbage collection can use g_binding_unbind() to explicitly release a binding between the source and target properties, instead of relying on the last reference on the binding, source, and target instances to drop.

GBinding is available since GObject 2.26

Since 2.26





Property Details

The “flags” property
  “flags”                    GBindingFlags
Flags to be used to control the GBinding

Flags: Read / Write / Construct Only

Since: 2.26

The “source” property
  “source”                   GObject *
The GObject that should be used as the source of the binding

Flags: Read / Write / Construct Only

Since: 2.26

The “source-property” property
  “source-property”          gchar *
The name of the property of “source” that should be used as the source of the binding

Flags: Read / Write / Construct Only

Default value: NULL

Since: 2.26

The “target” property
  “target”                   GObject *
The GObject that should be used as the target of the binding

Flags: Read / Write / Construct Only

Since: 2.26

The “target-property” property
  “target-property”          gchar *
The name of the property of “target” that should be used as the target of the binding

Flags: Read / Write / Construct Only

Default value: NULL

Since: 2.26


|#




;;; ----------------------------------------------------------------------------
;;; g_binding_get_source ()
;;;
;;; GObject * g_binding_get_source (GBinding *binding);
;;;
;;; Retrieves the GObject instance used as the source of the binding.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; Returns :
;;;     the source GObject.
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_binding_get_source_property ()
;;;
;;; const gchar * g_binding_get_source_property (GBinding *binding);
;;;
;;; Retrieves the name of the property of “source” used as the source of the
;;; binding.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; Returns :
;;;     the name of the source property
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_binding_get_target ()
;;;
;;; GObject * g_binding_get_target (GBinding *binding);
;;;
;;; Retrieves the GObject instance used as the target of the binding.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; Returns :
;;;     the target GObject.
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_binding_get_target_property ()
;;;
;;; const gchar * g_binding_get_target_property (GBinding *binding);
;;;
;;; Retrieves the name of the property of “target” used as the target of the
;;; binding.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; Returns :
;;;     the name of the target property
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_binding_get_flags ()
;;;
;;; GBindingFlags g_binding_get_flags (GBinding *binding);
;;;
;;; Retrieves the flags passed when constructing the GBinding.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; Returns :
;;;     the GBindingFlags used by the GBinding
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_binding_unbind ()
;;;
;;; void g_binding_unbind (GBinding *binding);
;;;
;;; Explicitly releases the binding between the source and the target property
;;; expressed by binding .
;;;
;;; This function will release the reference that is being held on the binding
;;; instance; if you want to hold on to the GBinding instance after calling
;;; g_binding_unbind(), you will need to hold a reference to it.
;;;
;;; binding :
;;;     a GBinding
;;;
;;; Since: 2.38
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_bind_property" g-object-bind-property) (g-object g-binding)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-12}
  @argument[source]{The source @class{g-object}.}
  @argument[source-property]{The property on source to bind.}
  @argument[target]{The target @class{g-object}.}
  @argument[target-property]{The property on target to bind.}
  @argument[flags]{Flags to pass to @class{g-binding}.}
  @begin{return}
    The @class{g-binding} instance representing the binding between the two
    @class{g-object} instances. The binding is released whenever the
    @class{g-binding} reference count reaches zero.
  @end{return}
  @begin{short}
    Creates a binding between @arg{source_property} on @arg{source} and 
    @arg{target_property} on @arg{target}.
  @end{short}
  Whenever the @arg{source-property} is changed the @arg{target-property} is
  updated using the same value. For instance:  
  @begin{pre}
    (g-object-bind-property action \"active\" widget \"sensitive\" :default)
  @end{pre}
  Will result in the @code{\"sensitive\"} property of the widget 
  @class{g-object} instance to be updated with the same value of the
  @code{\"active\"} property of the action @class{g-object} instance.

  If @arg{flags} contains @code{:bidirectional} then the binding will be mutual:
  if @arg{target_property} on @arg{target} changes then the 
  @arg{source_property} on @arg{source} will be updated as well.

  The binding will automatically be removed when either the source or the
  target instances are finalized. To remove the binding without affecting the
  source and the target you can just call @fun{g-object-unref} on the returned
  @class{g-binding} instance.

  A @class{g-object} can have multiple bindings.  

  Since 2.26
  @see-class{g-binding}
  @see-class{g-object}
  @see-function{g-object-unref}"
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
