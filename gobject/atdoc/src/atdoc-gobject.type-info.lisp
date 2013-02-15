;;; ----------------------------------------------------------------------------
;;; atdoc-gobject.type-info.lisp
;;;
;;; Documentation strings for the library GObject.
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.32.4. See http://www.gtk.org
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

(in-package :gobject)

;;; ----------------------------------------------------------------------------




(defcfun ("g_type_default_interface_ref" g-type-default-interface-ref) :pointer
  (gtype g-type))

(export 'g-type-default-interface-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_peek ()
;;;
;;; gpointer g_type_default_interface_peek (GType g_type);
;;;
;;; If the interface type g_type is currently in use, returns its default
;;; interface vtable.
;;;
;;; g_type :
;;;     an interface type
;;;
;;; Returns :
;;;     the default vtable for the interface, or NULL if the type is not
;;;     currently in use
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_unref ()
;;;
;;; void g_type_default_interface_unref (gpointer g_iface);
;;;
;;; Decrements the reference count for the type corresponding to the interface
;;; default vtable g_iface. If the type is dynamic, then when no one is using
;;; the interface and all references have been released, the finalize function
;;; for the interface's default vtable (the class_finalize member of GTypeInfo)
;;; will be called.
;;;
;;; g_iface :
;;;     the default vtable structure for a interface, as returned by
;;;     g_type_default_interface_ref()
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_default_interface_unref" g-type-default-interface-unref) :void
  (interface :pointer))

(export 'g-type-default-interface-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_children ()
;;;
;;; GType * g_type_children (GType type, guint *n_children);
;;;
;;; Return a newly allocated and 0-terminated array of type IDs, listing the
;;; child types of type. The return value has to be g_free()ed after use.
;;;
;;; type :
;;;     The parent type.
;;;
;;; n_children :
;;;     Optional guint pointer to contain the number of child types.
;;;
;;; Returns :
;;;     Newly allocated and 0-terminated array of child types.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_children" %g-type-children) (:pointer %g-type)
  (gtype g-type)
  (n-children (:pointer :uint)))

(defun g-type-children (type)
  (with-foreign-object (n-children :uint)
    (let ((g-types-ptr (%g-type-children type n-children)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-children :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(export 'g-type-children)

;;; ----------------------------------------------------------------------------
;;; g_type_interfaces ()
;;;
;;; GType * g_type_interfaces (GType type, guint *n_interfaces);
;;;
;;; Return a newly allocated and 0-terminated array of type IDs, listing the
;;; interface types that type conforms to. The return value has to be g_free()ed
;;; after use.
;;;
;;; type :
;;;     The type to list interface types for.
;;;
;;; n_interfaces :
;;;     Optional guint pointer to contain the number of interface types.
;;;
;;; Returns :
;;;     Newly allocated and 0-terminated array of interface types.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interfaces" %g-type-interfaces) (:pointer %g-type)
  (type g-type)
  (n-interfaces (:pointer :uint)))

(defun g-type-interfaces (type)
  (with-foreign-object (n-interfaces :uint)
    (let ((g-types-ptr (%g-type-interfaces type n-interfaces)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interfaces :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(export 'g-type-interfaces)

;;; ----------------------------------------------------------------------------
;;; g_type_interface_prerequisites ()
;;;
;;; GType * g_type_interface_prerequisites (GType interface_type,
;;;                                         guint *n_prerequisites);
;;;
;;; Returns the prerequisites of an interfaces type.
;;;
;;; interface_type :
;;;     an interface type
;;;
;;; n_prerequisites :
;;;     location to return the number of prerequisites, or NULL
;;;
;;; Returns :
;;;     a newly-allocated zero-terminated array of GType containing the
;;;     prerequisites of interface_type
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_prerequisites" %g-type-interface-prerequisites)
    (:pointer %g-type)
  (interface-type g-type)
  (n-prerequisites (:pointer :uint)))

(defun g-type-interface-prerequisites (interface-type)
  (with-foreign-object (n-prerequisites :uint)
    (let ((g-types-ptr (%g-type-interface-prerequisites interface-type
                                                        n-prerequisites)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-prerequisites :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(export 'g-type-interface-prerequisites)

;;; ----------------------------------------------------------------------------
;;; g_type_set_qdata ()
;;;
;;; void g_type_set_qdata (GType type, GQuark quark, gpointer data);
;;;
;;; Attaches arbitrary data to a type.
;;;
;;; type :
;;;     a GType
;;;
;;; quark :
;;;     a GQuark id to identify the data
;;;
;;; data :
;;;     the data
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_set_qdata" g-type-set-qdata) :void
  (gtype g-type)
  (quark g-quark)
  (data :pointer))

(export 'g-type-set-qdata)

;;; ----------------------------------------------------------------------------
;;; g_type_get_qdata ()
;;;
;;; gpointer g_type_get_qdata (GType type, GQuark quark);
;;;
;;; Obtains data which has previously been attached to type with
;;; g_type_set_qdata().
;;;
;;; Note that this does not take subtyping into account; data attached to one
;;; type with g_type_set_qdata() cannot be retrieved from a subtype using
;;; g_type_get_qdata().
;;;
;;; type :
;;;     a GType
;;;
;;; quark :
;;;     a GQuark id to identify the data
;;;
;;; Returns :
;;;     the data, or NULL if no data was found
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_get_qdata" g-type-get-qdata) :pointer
  (gtype g-type)
  (quark g-quark))

(export 'g-type-get-qdata)

|#


;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBaseInitFunc ()
;;;
;;; void (*GBaseInitFunc) (gpointer g_class);
;;;
;;; A callback function used by the type system to do base initialization of the
;;; class structures of derived types. It is called as part of the
;;; initialization process of all derived classes and should reallocate or reset
;;; all dynamic class members copied over from the parent class. For example,
;;; class members (such as strings) that are not sufficiently handled by a plain
;;; memory copy of the parent class into the derived class have to be altered.
;;; See GClassInitFunc() for a discussion of the class intialization process.
;;;
;;; g_class :
;;;     The GTypeClass structure to initialize.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBaseFinalizeFunc ()
;;;
;;; void (*GBaseFinalizeFunc) (gpointer g_class);
;;;
;;; A callback function used by the type system to finalize those portions of a
;;; derived types class structure that were setup from the corresponding
;;; GBaseInitFunc() function. Class finalization basically works the inverse way
;;; in which class intialization is performed. See GClassInitFunc() for a
;;; discussion of the class intialization process.
;;;
;;; g_class :
;;;     The GTypeClass structure to finalize.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassInitFunc ()
;;;
;;; void (*GClassInitFunc) (gpointer g_class, gpointer class_data);
;;;
;;; A callback function used by the type system to initialize the class of a
;;; specific type. This function should initialize all static class members. The
;;; initialization process of a class involves:
;;;
;;;     1 - Copying common members from the parent class over to the derived
;;;         class structure.
;;;
;;;     2 - Zero initialization of the remaining members not copied over from
;;;         the parent class.
;;;
;;;     3 - Invocation of the GBaseInitFunc() initializers of all parent types
;;;         and the class' type.
;;;
;;;     4 - Invocation of the class' GClassInitFunc() initializer.
;;;
;;; Since derived classes are partially initialized through a memory copy of the
;;; parent class, the general rule is that GBaseInitFunc() and
;;; GBaseFinalizeFunc() should take care of necessary reinitialization and
;;; release of those class members that were introduced by the type that
;;; specified these GBaseInitFunc()/GBaseFinalizeFunc(). GClassInitFunc() should
;;; only care about initializing static class members, while dynamic class
;;; members (such as allocated strings or reference counted resources) are
;;; better handled by a GBaseInitFunc() for this type, so proper initialization
;;; of the dynamic class members is performed for class initialization of
;;; derived types as well. An example may help to correspond the intend of the
;;; different class initializers:
;;;
;;;   typedef struct {
;;;     GObjectClass parent_class;
;;;     gint         static_integer;
;;;     gchar       *dynamic_string;
;;;   } TypeAClass;
;;;   static void
;;;   type_a_base_class_init (TypeAClass *class)
;;;   {
;;;     class->dynamic_string = g_strdup ("some string");
;;;   }
;;;   static void
;;;   type_a_base_class_finalize (TypeAClass *class)
;;;   {
;;;     g_free (class->dynamic_string);
;;;   }
;;;   static void
;;;   type_a_class_init (TypeAClass *class)
;;;   {
;;;     class->static_integer = 42;
;;;   }
;;;
;;;   typedef struct {
;;;     TypeAClass   parent_class;
;;;     gfloat       static_float;
;;;     GString     *dynamic_gstring;
;;;   } TypeBClass;
;;;   static void
;;;   type_b_base_class_init (TypeBClass *class)
;;;   {
;;;     class->dynamic_gstring = g_string_new ("some other string");
;;;   }
;;;   static void
;;;   type_b_base_class_finalize (TypeBClass *class)
;;;   {
;;;     g_string_free (class->dynamic_gstring);
;;;   }
;;;   static void
;;;   type_b_class_init (TypeBClass *class)
;;;   {
;;;     class->static_float = 3.14159265358979323846;
;;;   }
;;;
;;; Initialization of TypeBClass will first cause initialization of TypeAClass
;;; (derived classes reference their parent classes, see g_type_class_ref() on
;;; this). Initialization of TypeAClass roughly involves zero-initializing its
;;; fields, then calling its GBaseInitFunc() type_a_base_class_init() to
;;; allocate its dynamic members (dynamic_string), and finally calling its
;;; GClassInitFunc() type_a_class_init() to initialize its static members
;;; (static_integer). The first step in the initialization process of TypeBClass
;;; is then a plain memory copy of the contents of TypeAClass into TypeBClass
;;; and zero-initialization of the remaining fields in TypeBClass. The dynamic
;;; members of TypeAClass within TypeBClass now need reinitialization which is
;;; performed by calling type_a_base_class_init() with an argument of
;;; TypeBClass. After that, the GBaseInitFunc() of TypeBClass,
;;; type_b_base_class_init() is called to allocate the dynamic members of
;;; TypeBClass (dynamic_gstring), and finally the GClassInitFunc() of
;;; TypeBClass, type_b_class_init(), is called to complete the initialization
;;; process with the static members (static_float). Corresponding finalization
;;; counter parts to the GBaseInitFunc() functions have to be provided to
;;; release allocated resources at class finalization time.
;;;
;;; g_class :
;;;     The GTypeClass structure to initialize.
;;;
;;; class_data :
;;;     The class_data member supplied via the GTypeInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassFinalizeFunc ()
;;;
;;; void (*GClassFinalizeFunc) (gpointer g_class, gpointer class_data);
;;;
;;; A callback function used by the type system to finalize a class. This
;;; function is rarely needed, as dynamically allocated class resources should
;;; be handled by GBaseInitFunc() and GBaseFinalizeFunc(). Also, specification
;;; of a GClassFinalizeFunc() in the GTypeInfo structure of a static type is
;;; invalid, because classes of static types will never be finalized (they are
;;; artificially kept alive when their reference count drops to zero).
;;;
;;; g_class :
;;;     The GTypeClass structure to finalize.
;;;
;;; class_data :
;;;     The class_data member supplied via the GTypeInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInstanceInitFunc ()
;;;
;;; void (*GInstanceInitFunc) (GTypeInstance *instance, gpointer g_class);
;;;
;;; A callback function used by the type system to initialize a new instance of
;;; a type. This function initializes all instance members and allocates any
;;; resources required by it. Initialization of a derived instance involves
;;; calling all its parent types instance initializers, so the class member of
;;; the instance is altered during its initialization to always point to the
;;; class that belongs to the type the current initializer was introduced for.
;;;
;;; instance :
;;;     The instance to initialize.
;;;
;;; g_class :
;;;     The class of the type the instance is created for.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceInitFunc ()
;;;
;;; void (*GInterfaceInitFunc) (gpointer g_iface, gpointer iface_data);
;;;
;;; A callback function used by the type system to initialize a new interface.
;;; This function should initialize all internal data and allocate any resources
;;; required by the interface.
;;;
;;; g_iface :
;;;     The interface structure to initialize.
;;;
;;; iface_data :
;;;     The interface_data supplied via the GInterfaceInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceFinalizeFunc ()
;;;
;;; void (*GInterfaceFinalizeFunc) (gpointer g_iface, gpointer iface_data);
;;;
;;; A callback function used by the type system to finalize an interface. This
;;; function should destroy any internal data and release any resources
;;; allocated by the corresponding GInterfaceInitFunc() function.
;;;
;;; g_iface :
;;;     The interface structure to finalize.
;;;
;;; iface_data :
;;;     The interface_data supplied via the GInterfaceInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeClassCacheFunc ()
;;;
;;; gboolean (*GTypeClassCacheFunc) (gpointer cache_data, GTypeClass *g_class);
;;;
;;; A callback function which is called when the reference count of a class
;;; drops to zero. It may use g_type_class_ref() to prevent the class from being
;;; freed. You should not call g_type_class_unref() from a GTypeClassCacheFunc
;;; function to prevent infinite recursion, use g_type_class_unref_uncached()
;;; instead.
;;;
;;; The functions have to check the class id passed in to figure whether they
;;; actually want to cache the class of this type, since all classes are routed
;;; through the same GTypeClassCacheFunc chain.
;;;
;;; cache_data :
;;;     data that was given to the g_type_add_class_cache_func() call
;;;
;;; g_class :
;;;     The GTypeClass structure which is unreferenced
;;;
;;; Returns :
;;;     TRUE to stop further GTypeClassCacheFuncs from being called, FALSE to
;;;     continue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_static_simple ()
;;;
;;; GType g_type_register_static_simple (GType parent_type,
;;;                                      const gchar *type_name,
;;;                                      guint class_size,
;;;                                      GClassInitFunc class_init,
;;;                                      guint instance_size,
;;;                                      GInstanceInitFunc instance_init,
;;;                                      GTypeFlags flags);
;;;
;;; Registers type_name as the name of a new static type derived from
;;; parent_type. The value of flags determines the nature (e.g. abstract or not)
;;; of the type. It works by filling a GTypeInfo struct and calling
;;; g_type_register_static().
;;;
;;; parent_type :
;;;     Type from which this type will be derived.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; class_size :
;;;     Size of the class structure (see GTypeInfo)
;;;
;;; class_init :
;;;     Location of the class initialization function (see GTypeInfo)
;;;
;;; instance_size :
;;;     Size of the instance structure (see GTypeInfo)
;;;
;;; instance_init :
;;;     Location of the instance initialization function (see GTypeInfo)
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The new type identifier.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_register_static_simple"
          g-type-register-static-simple) g-type
  (parent-type g-type)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags g-type-flags))

(export 'g-type-register-static-simple)

;;; ----------------------------------------------------------------------------
;;; g_type_register_dynamic ()
;;;
;;; GType g_type_register_dynamic (GType parent_type,
;;;                                const gchar *type_name,
;;;                                GTypePlugin *plugin,
;;;                                GTypeFlags flags);
;;;
;;; Registers type_name as the name of a new dynamic type derived from
;;; parent_type. The type system uses the information contained in the
;;; GTypePlugin structure pointed to by plugin to manage the type and its
;;; instances (if not abstract). The value of flags determines the nature (e.g.
;;; abstract or not) of the type.
;;;
;;; parent_type :
;;;     Type from which this type will be derived.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; plugin :
;;;     The GTypePlugin structure to retrieve the GTypeInfo from.
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The new type identifier or G_TYPE_INVALID if registration failed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_fundamental ()
;;;
;;; GType g_type_register_fundamental (GType type_id,
;;;                                    const gchar *type_name,
;;;                                    const GTypeInfo *info,
;;;                                    const GTypeFundamentalInfo *finfo,
;;;                                    GTypeFlags flags);
;;;
;;; Registers type_id as the predefined identifier and type_name as the name of
;;; a fundamental type. If type_id is already registered, or a type named
;;; type_name is already registered, the behaviour is undefined. The type system
;;; uses the information contained in the GTypeInfo structure pointed to by info
;;; and the GTypeFundamentalInfo structure pointed to by finfo to manage the
;;; type and its instances. The value of flags determines additional
;;; characteristics of the fundamental type.
;;;
;;; type_id :
;;;     A predefined type identifier.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; info :
;;;     The GTypeInfo structure for this type.
;;;
;;; finfo :
;;;     The GTypeFundamentalInfo structure for this type.
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The predefined type identifier.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_static ()
;;;
;;; void g_type_add_interface_static (GType instance_type,
;;;                                   GType interface_type,
;;;                                   const GInterfaceInfo *info);
;;;
;;; Adds the static interface_type to instantiable_type. The information
;;; contained in the GInterfaceInfo structure pointed to by info is used to
;;; manage the relationship.
;;;
;;; instance_type :
;;;     GType value of an instantiable type.
;;;
;;; interface_type :
;;;     GType value of an interface type.
;;;
;;; info :
;;;     The GInterfaceInfo structure for this (instance_type, interface_type)
;;;     combination.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_add_interface_static" g-type-add-interface-static) :void
  (instance-type g-type)
  (interface-type g-type)
  (info (:pointer g-interface-info)))

(export 'g-type-add-interface-static)

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_dynamic ()
;;;
;;; void g_type_add_interface_dynamic (GType instance_type,
;;;                                    GType interface_type,
;;;                                    GTypePlugin *plugin);
;;;
;;; Adds the dynamic interface_type to instantiable_type. The information
;;; contained in the GTypePlugin structure pointed to by plugin is used to
;;; manage the relationship.
;;;
;;; instance_type :
;;;     the GType value of an instantiable type.
;;;
;;; interface_type :
;;;     the GType value of an interface type.
;;;
;;; plugin :
;;;     the GTypePlugin structure to retrieve the GInterfaceInfo from.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_add_prerequisite ()
;;;
;;; void g_type_interface_add_prerequisite (GType interface_type,
;;;                                         GType prerequisite_type);
;;;
;;; Adds prerequisite_type to the list of prerequisites of interface_type. This
;;; means that any type implementing interface_type must also implement
;;; prerequisite_type. Prerequisites can be thought of as an alternative to
;;; interface derivation (which GType doesn't support). An interface can have at
;;; most one instantiatable prerequisite type.
;;;
;;; interface_type :
;;;     GType value of an interface type.
;;;
;;; prerequisite_type :
;;;     GType value of an interface or instantiatable type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_add_prerequisite" g-type-interface-add-prerequisite)
    :void
  (interface-type g-type)
  (prerequisite-type g-type))

(export 'g-type-interface-add-prerequisite)

;;; ----------------------------------------------------------------------------
;;; g_type_get_plugin ()
;;;
;;; GTypePlugin * g_type_get_plugin (GType type);
;;;
;;; Returns the GTypePlugin structure for type or NULL if type does not have a
;;; GTypePlugin structure.
;;;
;;; type :
;;;     The GType to retrieve the plugin for.
;;;
;;; Returns :
;;;     The corresponding plugin if type is a dynamic type, NULL otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_get_plugin ()
;;;
;;; GTypePlugin * g_type_interface_get_plugin (GType instance_type,
;;;                                            GType interface_type);
;;;
;;; Returns the GTypePlugin structure for the dynamic interface interface_type
;;; which has been added to instance_type, or NULL if interface_type has not
;;; been added to instance_type or does not have a GTypePlugin structure. See
;;; g_type_add_interface_dynamic().
;;;
;;; instance_type :
;;;     the GType value of an instantiatable type.
;;;
;;; interface_type :
;;;     the GType value of an interface type.
;;;
;;; Returns :
;;;     the GTypePlugin for the dynamic interface interface_type of
;;;     instance_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental_next ()
;;;
;;; GType g_type_fundamental_next (void);
;;;
;;; Returns the next free fundamental type id which can be used to register a
;;; new fundamental type with g_type_register_fundamental(). The returned type
;;; ID represents the highest currently registered fundamental type identifier.
;;;
;;; Returns :
;;;     The nextmost fundamental type ID to be registered, or 0 if the type
;;;     system ran out of fundamental type IDs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental ()
;;;
;;; GType g_type_fundamental (GType type_id);
;;;
;;; Internal function, used to extract the fundamental type ID portion. Use
;;; G_TYPE_FUNDAMENTAL() instead.
;;;
;;; type_id :
;;;     valid type ID
;;;
;;; Returns :
;;;     fundamental type ID
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_fundamental" g-type-fundamental) g-type
  (gtype g-type))

(export 'g-type-fundamental)

;;; ----------------------------------------------------------------------------
;;; g_type_create_instance ()
;;;
;;; GTypeInstance * g_type_create_instance (GType type);
;;;
;;; Creates and initializes an instance of type if type is valid and can be
;;; instantiated. The type system only performs basic allocation and structure
;;; setups for instances: actual instance creation should happen through
;;; functions supplied by the type's fundamental type implementation. So use of
;;; g_type_create_instance() is reserved for implementators of fundamental types
;;; only. E.g. instances of the GObject hierarchy should be created via
;;; g_object_new() and never directly through g_type_create_instance() which
;;; doesn't handle things like singleton objects or object construction. Note:
;;; Do not use this function, unless you're implementing a fundamental type.
;;; Also language bindings should not use this function but g_object_new()
;;; instead.
;;;
;;; type :
;;;     An instantiatable type to create an instance for.
;;;
;;; Returns :
;;;     An allocated and initialized instance, subject to further treatment by
;;;     the fundamental type implementation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_free_instance ()
;;;
;;; void g_type_free_instance (GTypeInstance *instance);
;;;
;;; Frees an instance of a type, returning it to the instance pool for the type,
;;; if there is one.
;;;
;;; Like g_type_create_instance(), this function is reserved for implementors of
;;; fundamental types.
;;;
;;; instance :
;;;     an instance of a type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_class_cache_func ()
;;;
;;; void g_type_add_class_cache_func (gpointer cache_data,
;;;                                   GTypeClassCacheFunc cache_func);
;;;
;;; Adds a GTypeClassCacheFunc to be called before the reference count of a
;;; class goes from one to zero. This can be used to prevent premature class
;;; destruction. All installed GTypeClassCacheFunc functions will be chained
;;; until one of them returns TRUE. The functions have to check the class id
;;; passed in to figure whether they actually want to cache the class of this
;;; type, since all classes are routed through the same GTypeClassCacheFunc
;;; chain.
;;;
;;; cache_data :
;;;     data to be passed to cache_func
;;;
;;; cache_func :
;;;     a GTypeClassCacheFunc
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_class_cache_func ()
;;;
;;; void g_type_remove_class_cache_func (gpointer cache_data,
;;;                                      GTypeClassCacheFunc cache_func);
;;;
;;; Removes a previously installed GTypeClassCacheFunc. The cache maintained by
;;; cache_func has to be empty when calling g_type_remove_class_cache_func() to
;;; avoid leaks.
;;;
;;; cache_data :
;;;     data that was given when adding cache_func
;;;
;;; cache_func :
;;;     a GTypeClassCacheFunc
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref_uncached ()
;;;
;;; void g_type_class_unref_uncached (gpointer g_class);
;;;
;;; A variant of g_type_class_unref() for use in GTypeClassCacheFunc
;;; implementations. It unreferences a class without consulting the chain of
;;; GTypeClassCacheFuncs, avoiding the recursion which would occur otherwise.
;;;
;;; g_class :
;;;     The GTypeClass structure to unreference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_check ()
;;;
;;; void g_type_add_interface_check (gpointer check_data,
;;;                                  GTypeInterfaceCheckFunc check_func);
;;;
;;; Adds a function to be called after an interface vtable is initialized for
;;; any class (i.e. after the interface_init member of GInterfaceInfo has been
;;; called).
;;;
;;; This function is useful when you want to check an invariant that depends on
;;; the interfaces of a class. For instance, the implementation of GObject uses
;;; this facility to check that an object implements all of the properties that
;;; are defined on its interfaces.
;;;
;;; check_data :
;;;     data to pass to check_func
;;;
;;; check_func :
;;;     function to be called after each interface is initialized.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_interface_check ()
;;;
;;; void g_type_remove_interface_check (gpointer check_data,
;;;                                     GTypeInterfaceCheckFunc check_func);
;;;
;;; Removes an interface check function added with g_type_add_interface_check().
;;;
;;; check_data :
;;;     callback data passed to g_type_add_interface_check()
;;;
;;; check_func :
;;;     callback function passed to g_type_add_interface_check()
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeInterfaceCheckFunc ()
;;;
;;; void (*GTypeInterfaceCheckFunc) (gpointer check_data, gpointer g_iface);
;;;
;;; A callback called after an interface vtable is initialized. See
;;; g_type_add_interface_check().
;;;
;;; check_data :
;;;     data passed to g_type_add_interface_check().
;;;
;;; g_iface :
;;;     the interface that has been initialized
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_value_table_peek ()
;;;
;;; GTypeValueTable * g_type_value_table_peek (GType type);
;;;
;;; Returns the location of the GTypeValueTable associated with type. Note that
;;; this function should only be used from source code that implements or has
;;; internal knowledge of the implementation of type.
;;;
;;; type :
;;;     A GType value.
;;;
;;; Returns :
;;;     Location of the GTypeValueTable associated with type or NULL if there is
;;;     no GTypeValueTable associated with type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_value_table_peek" g-type-value-table-peek)
    (:pointer g-type-value-table)
  (type g-type))

(export 'g-type-value-table-peek)

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE()
;;;
;;; #define G_DEFINE_TYPE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, 0, {})
;;;
;;; A convenience macro for type implementations, which declares a class
;;; initialization function, an instance initialization function (see GTypeInfo
;;; for information about these) and a static variable named t_n_parent_class
;;; pointing to the parent class. Furthermore, it defines a *_get_type()
;;; function. See G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_TYPE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, 0) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for type implementations. Similar to G_DEFINE_TYPE(),
;;; but allows you to insert custom code into the *_get_type() function, e.g.
;;; interface implementations via G_IMPLEMENT_INTERFACE(). See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT, {})
;;;
;;; A convenience macro for type implementations. Similar to G_DEFINE_TYPE(),
;;; but defines an abstract type. See G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for type implementations. Similar to
;;; G_DEFINE_TYPE_WITH_CODE(), but defines an abstract type and allows you to
;;; insert custom code into the *_get_type() function, e.g. interface
;;; implementations via G_IMPLEMENT_INTERFACE(). See G_DEFINE_TYPE_EXTENDED()
;;; for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the type_name_get_type() function.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_INTERFACE()
;;;
;;; #define G_DEFINE_INTERFACE(TN, t_n, T_P)
;;;         G_DEFINE_INTERFACE_WITH_CODE(TN, t_n, T_P, ;)
;;;
;;; A convenience macro for GTypeInterface definitions, which declares a default
;;; vtable initialization function and defines a *_get_type() function.
;;;
;;; The macro expects the interface initialization function to have the name
;;; t_n ## _default_init, and the interface structure to have the name
;;; TN ## Interface.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the prerequisite type for the interface, or 0
;;;     (G_TYPE_INVALID) for no prerequisite type.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_INTERFACE_WITH_CODE()
;;;
;;; #define G_DEFINE_INTERFACE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_INTERFACE_EXTENDED_BEGIN(TN, t_n, T_P) {_C_;}
;;;        _G_DEFINE_INTERFACE_EXTENDED_END()
;;;
;;; A convenience macro for GTypeInterface definitions. Similar to
;;; G_DEFINE_INTERFACE(), but allows you to insert custom code into the
;;; *_get_type() function, e.g. additional interface implementations via
;;; G_IMPLEMENT_INTERFACE(), or additional prerequisite types. See
;;; G_DEFINE_TYPE_EXTENDED() for a similar example using
;;; G_DEFINE_TYPE_WITH_CODE().
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the prerequisite type for the interface, or 0
;;;     (G_TYPE_INVALID) for no prerequisite type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IMPLEMENT_INTERFACE()
;;;
;;; #define G_IMPLEMENT_INTERFACE(TYPE_IFACE, iface_init)
;;;
;;; A convenience macro to ease interface addition in the _C_ section of
;;; G_DEFINE_TYPE_WITH_CODE() or G_DEFINE_ABSTRACT_TYPE_WITH_CODE(). See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; Note that this macro can only be used together with the
;;; G_DEFINE_TYPE_* macros, since it depends on variable names from those
;;; macros.
;;;
;;; TYPE_IFACE :
;;;     The GType of the interface to add
;;;
;;; iface_init :
;;;     The interface init function
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_EXTENDED()
;;;
;;; #define G_DEFINE_TYPE_EXTENDED(TN, t_n, T_P, _f_, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, _f_) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; The most general convenience macro for type implementations, on which
;;; G_DEFINE_TYPE(), etc are based.
;;;
;;;   G_DEFINE_TYPE_EXTENDED (GtkGadget,
;;;                           gtk_gadget,
;;;                           GTK_TYPE_WIDGET,
;;;                           0,
;;;                           G_IMPLEMENT_INTERFACE (TYPE_GIZMO,
;;;                                                  gtk_gadget_gizmo_init));
;;;
;;; expands to
;;;
;;;   static void     gtk_gadget_init       (GtkGadget      *self);
;;;   static void     gtk_gadget_class_init (GtkGadgetClass *klass);
;;;   static gpointer gtk_gadget_parent_class = NULL;
;;;   static void     gtk_gadget_class_intern_init (gpointer klass)
;;;   {
;;;     gtk_gadget_parent_class = g_type_class_peek_parent (klass);
;;;     gtk_gadget_class_init ((GtkGadgetClass*) klass);
;;;   }
;;;
;;;   GType
;;;   gtk_gadget_get_type (void)
;;;   {
;;;     static volatile gsize g_define_type_id__volatile = 0;
;;;     if (g_once_init_enter (&g_define_type_id__volatile))
;;;       {
;;;         GType g_define_type_id =
;;;           g_type_register_static_simple
;;;                              (GTK_TYPE_WIDGET,
;;;                               g_intern_static_string ("GtkGadget"),
;;;                               sizeof (GtkGadgetClass),
;;;                               (GClassInitFunc) gtk_gadget_class_intern_init,
;;;                               sizeof (GtkGadget),
;;;                               (GInstanceInitFunc) gtk_gadget_init,
;;;                               (GTypeFlags) flags);
;;;         {
;;;           const GInterfaceInfo g_implement_interface_info = {
;;;             (GInterfaceInitFunc) gtk_gadget_gizmo_init
;;;           };
;;;           g_type_add_interface_static (g_define_type_id,
;;;                                        TYPE_GIZMO,
;;;                                        &g_implement_interface_info);
;;;         }
;;;         g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
;;;       }
;;;     return g_define_type_id__volatile;
;;;   }
;;;
;;; The only pieces which have to be manually provided are the definitions of
;;; the instance and class structure and the definitions of the instance and
;;; class init functions.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _f_ :
;;;     GTypeFlags to pass to g_type_register_static()
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_BOXED_TYPE()
;;;
;;; #define G_DEFINE_BOXED_TYPE(TypeName, type_name, copy_func, free_func)
;;;         G_DEFINE_BOXED_TYPE_WITH_CODE (TypeName,
;;;                                        type_name,
;;;                                        copy_func,
;;;                                        free_func, {})
;;;
;;; A convenience macro for boxed type implementations, which defines a
;;; type_name_get_type() function registering the boxed type.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; copy_func :
;;;     the GBoxedCopyFunc for the new type
;;;
;;; free_func :
;;;     the GBoxedFreeFunc for the new type
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_BOXED_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_BOXED_TYPE_WITH_CODE(TypeName, type_name, copy_func, free_func, _C_)
;;;        _G_DEFINE_BOXED_TYPE_BEGIN (TypeName, type_name, copy_func, free_func) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for boxed type implementations. Similar to
;;; G_DEFINE_BOXED_TYPE(), but allows to insert custom code into the
;;; type_name_get_type() function, e.g. to register value transformations with
;;; g_value_register_transform_func().
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; copy_func :
;;;     the GBoxedCopyFunc for the new type
;;;
;;; free_func :
;;;     the GBoxedFreeFunc for the new type
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_POINTER_TYPE()
;;;
;;; #define G_DEFINE_POINTER_TYPE(TypeName, type_name)
;;;         G_DEFINE_POINTER_TYPE_WITH_CODE (TypeName, type_name, {})
;;;
;;; A convenience macro for pointer type implementations, which defines a
;;; type_name_get_type() function registering the pointer type.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_POINTER_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_POINTER_TYPE_WITH_CODE(TypeName, type_name, _C_)
;;;        _G_DEFINE_POINTER_TYPE_BEGIN (TypeName, type_name) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for pointer type implementations. Similar to
;;; G_DEFINE_POINTER_TYPE(), but allows to insert custom code into the
;;; type_name_get_type() function.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

|#

;;; --- End of file gtk-atdoc.gobject.type-info.lisp ---------------------------
