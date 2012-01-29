;;; ----------------------------------------------------------------------------
;;; gtk.object.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkObject
;;; 
;;; The base class of the GTK+ type hierarchy
;;; 
;;; Synopsis
;;; 
;;;     GtkObject
;;;
;;;     GTK_OBJECT_TYPE
;;;     GTK_OBJECT_TYPE_NAME
;;;
;;;     GtkObjectFlags
;;;
;;;     GTK_OBJECT_FLAGS
;;;     GTK_OBJECT_FLOATING
;;;
;;;     GtkArgFlags
;;;
;;;     gtk_object_new
;;;     gtk_object_sink
;;;     gtk_object_ref
;;;     gtk_object_unref
;;;     gtk_object_weakref
;;;     gtk_object_weakunref
;;;     gtk_object_destroy
;;;     gtk_object_get
;;;     gtk_object_set
;;;     gtk_object_set_data
;;;     gtk_object_set_data_full
;;;     gtk_object_remove_data
;;;     gtk_object_get_data
;;;     gtk_object_remove_no_notify
;;;     gtk_object_set_user_data
;;;     gtk_object_get_user_data
;;;     gtk_object_add_arg_type
;;;     gtk_object_set_data_by_id
;;;     gtk_object_set_data_by_id_full
;;;     gtk_object_get_data_by_id
;;;     gtk_object_remove_data_by_id
;;;     gtk_object_remove_no_notify_by_id
;;;     gtk_object_data_try_key
;;;     gtk_object_data_force_id
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                +----GtkAdjustment
;;;                +----GtkCellRenderer
;;;                +----GtkFileFilter
;;;                +----GtkItemFactory
;;;                +----GtkTooltips
;;;                +----GtkTreeViewColumn
;;;                +----GtkRecentFilter
;;; 
;;; Properties
;;; 
;;;   "user-data"                gpointer              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "destroy"                                        : No Hooks
;;; 
;;; Description
;;; Description
;;; 
;;; GtkObject is the base class for all widgets, and for a few non-widget
;;; objects such as GtkAdjustment. GtkObject predates GObject; non-widgets that
;;; derive from GtkObject rather than GObject do so for backward compatibility
;;; reasons.
;;; 
;;; GtkObjects are created with a "floating" reference count. This means that
;;; the initial reference is not owned by anyone. Calling g_object_unref() on a
;;; newly-created GtkObject is incorrect, the floating reference has to be
;;; removed first. This can be done by anyone at any time, by calling
;;; g_object_ref_sink() to convert the floating reference into a regular
;;; reference. g_object_ref_sink() returns a new reference if an object is
;;; already sunk (has no floating reference).
;;; 
;;; When you add a widget to its parent container, the parent container will do
;;; this:
;;; 
;;;   g_object_ref_sink (G_OBJECT (child_widget));
;;; 
;;; This means that the container now owns a reference to the child widget and
;;; the child widget has no floating reference.
;;; 
;;; The purpose of the floating reference is to keep the child widget alive
;;; until you add it to a parent container:
;;; 
;;;    button = gtk_button_new ();
;;;    /* button has one floating reference to keep it alive */
;;;    gtk_container_add (GTK_CONTAINER (container), button);
;;;    /* button has one non-floating reference owned by the container */
;;; 
;;; GtkWindow is a special case, because GTK+ itself will ref/sink it on
;;; creation. That is, after calling gtk_window_new(), the GtkWindow will have
;;; one reference which is owned by GTK+, and no floating references.
;;; 
;;; One more factor comes into play: the "destroy" signal, emitted by the
;;; gtk_object_destroy() method. The "destroy" signal asks all code owning a
;;; reference to an object to release said reference. So, for example, if you
;;; call gtk_object_destroy() on a GtkWindow, GTK+ will release the reference
;;; count that it owns; if you call gtk_object_destroy() on a GtkButton, then
;;; the button will be removed from its parent container and the parent
;;; container will release its reference to the button. Because these references
;;; are released, calling gtk_object_destroy() should result in freeing all
;;; memory associated with an object, unless some buggy code fails to release
;;; its references in response to the "destroy" signal. Freeing memory (referred
;;; to as finalization only happens if the reference count reaches zero.
;;; 
;;; Some simple rules for handling GtkObject:
;;; 
;;;     * Never call g_object_unref() unless you have previously called
;;;       g_object_ref(), even if you created the GtkObject. (Note: this is not
;;;       true for GObject; for GObject, the creator of the object owns a
;;;       reference.)
;;;     * Call gtk_object_destroy() to get rid of most objects in most cases.
;;;       In particular, widgets are almost always destroyed in this way.
;;;     * Because of the floating reference count, you don't need to worry about
;;;       reference counting for widgets and toplevel windows, unless you
;;;       explicitly call g_object_ref() yourself.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "user-data" property
;;; 
;;;   "user-data"                gpointer              : Read / Write
;;; 
;;; Anonymous User Data Pointer.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "destroy" signal
;;; 
;;; void user_function (GtkObject *object,
;;;                     gpointer   user_data)      : No Hooks
;;; 
;;; Signals that all holders of a reference to the GtkObject should release the
;;; reference that they hold. May result in finalization of the object if all
;;; references are released.
;;; 
;;; object :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkObject
;;; 
;;; typedef struct _GtkObject GtkObject;
;;; 
;;; The object itself. You should never use these members directly - use the
;;; accessing macros instead.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkObject" gtk-object
  (:superclass g-initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_object_get_type")
  ((user-data gtk-object-user-data
    "user-data" "gpointer" t t)))

;;; ----------------------------------------------------------------------------
;;; GTK_OBJECT_TYPE
;;; 
;;; #define GTK_OBJECT_TYPE G_OBJECT_TYPE
;;; 
;;; Warning
;;; 
;;; GTK_OBJECT_TYPE has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use G_OBJECT_TYPE() instead.
;;; 
;;; Gets the type of an object.
;;; 
;;; object :
;;;     a GtkObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_OBJECT_TYPE_NAME
;;; 
;;; #define GTK_OBJECT_TYPE_NAME G_OBJECT_TYPE_NAME
;;; 
;;; Warning
;;; 
;;; GTK_OBJECT_TYPE_NAME has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Use G_OBJECT_TYPE_NAME() instead.
;;; 
;;; Gets the name of an object's type.
;;; 
;;; object :
;;;     a GtkObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkObjectFlags
;;; 
;;; typedef enum {
;;;   GTK_IN_DESTRUCTION = 1 << 0, /* Used internally during dispose */
;;;   GTK_FLOATING       = 1 << 1,
;;;   GTK_RESERVED_1     = 1 << 2,
;;;   GTK_RESERVED_2     = 1 << 3
;;; } GtkObjectFlags;
;;; 
;;; Warning
;;; 
;;; GtkObjectFlags has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Do not re-use GtkObject flags but use your own
;;; variable to store flags.
;;; 
;;; Tells about the state of the object.
;;; 
;;; GTK_IN_DESTRUCTION
;;;     the object is currently being destroyed. This is used internally by
;;;     GTK+ to prevent reinvokations during destruction.
;;; 
;;; GTK_FLOATING
;;;     
;;; 
;;; GTK_RESERVED_1
;;;     
;;; 
;;; GTK_RESERVED_2
;;;     reserved for future use
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkObjectFlags" gtk-object-flags
  (:export t
   :type-initializer "gtk_object_flags_get_type")
  (:in-destruction 1)
  (:floating 2)
  (:reserved-1 4)
  (:reserved-2 8))

;;; ----------------------------------------------------------------------------
;;; GTK_OBJECT_FLAGS()
;;; 
;;; #define GTK_OBJECT_FLAGS(obj) (GTK_OBJECT (obj)->flags)
;;; 
;;; Warning
;;; 
;;; GTK_OBJECT_FLAGS has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Do not re-use GtkObject flags but use your own
;;; variable to store flags.
;;; 
;;; Gets the GtkObjectFlags for an object without directly accessing its
;;; members.
;;; 
;;; obj :
;;;     the object whose flags are returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_OBJECT_FLOATING()
;;; 
;;; #define GTK_OBJECT_FLOATING(obj) (g_object_is_floating (obj))
;;; 
;;; Warning
;;; 
;;; GTK_OBJECT_FLOATING is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Evaluates to TRUE if the object still has its floating reference count.
;;; See the overview documentation for GtkObject.
;;; 
;;; obj :
;;;     the object to examine.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkArgFlags
;;; 
;;; typedef enum {
;;;   GTK_ARG_READABLE       = G_PARAM_READABLE,
;;;   GTK_ARG_WRITABLE       = G_PARAM_WRITABLE,
;;;   GTK_ARG_CONSTRUCT      = G_PARAM_CONSTRUCT,
;;;   GTK_ARG_CONSTRUCT_ONLY = G_PARAM_CONSTRUCT_ONLY,
;;;   GTK_ARG_CHILD_ARG      = 1 << 4
;;; } GtkArgFlags;
;;; 
;;; Warning
;;; 
;;; GtkArgFlags is deprecated and should not be used in newly-written code.
;;; Use corresponding GParamSpec features instead
;;; 
;;; Possible flags indicating how an argument should be treated.
;;; 
;;; GTK_ARG_READABLE
;;;     the argument is readable. (i.e. can be queried)
;;; 
;;; GTK_ARG_WRITABLE
;;;     the argument is writable. (i.e. settable)
;;; 
;;; GTK_ARG_CONSTRUCT
;;;     the argument needs construction.
;;; 
;;; GTK_ARG_CONSTRUCT_ONLY
;;;     the argument needs construction (and will be set once during object
;;;     creation), but is otherwise cannot be set. Hence this flag is not
;;;     allowed with GTK_ARG_WRITABLE, and is redundant with GTK_ARG_CONSTRUCT.
;;; 
;;; GTK_ARG_CHILD_ARG
;;;     an argument type that applies to (and may be different for) each child.
;;;     Used by GtkContainer.
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkArgFlags" gtk-arg-flags
  (:export t
   :type-initializer "gtk_arg_flags_get_type")
  (:readable 1)
  (:writable 2)
  (:construct 4)
  (:construct-only 8)
  (:child-arg 16))

;;; ----------------------------------------------------------------------------
;;; gtk_object_new ()
;;; 
;;; GtkObject * gtk_object_new (GType type,
;;;                             const gchar *first_property_name,
;;;                             ...);
;;; 
;;; Warning
;;; 
;;; gtk_object_new is deprecated and should not be used in newly-written code.
;;; Use g_object_new() instead.
;;; 
;;; Constructs an object given its arguments, enumerated in the call to the
;;; function.
;;; 
;;; type :
;;;     the type identifying this object. Returned by gtk_type_unique()
;;;     (although for a properly-written object it should be accessible through
;;;     a GTK_TYPE_FOO macro.)
;;; 
;;; first_property_name :
;;;     name of the first property to set when constructing the object.
;;; 
;;; Returns :
;;;     the new GtkObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_sink ()
;;; 
;;; void gtk_object_sink (GtkObject *object);
;;; 
;;; Warning
;;; 
;;; gtk_object_sink has been deprecated since version 2.10 and should not be
;;; used in newly-written code. Use g_object_ref_sink() instead
;;; 
;;; Removes the floating reference from a GtkObject, if it exists; otherwise
;;; does nothing. See the GtkObject overview documentation at the top of the
;;; page.
;;; 
;;; object :
;;;     the object to sink.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_ref ()
;;; 
;;; GtkObject * gtk_object_ref (GtkObject *object);
;;; 
;;; Warning
;;; 
;;; gtk_object_ref is deprecated and should not be used in newly-written code.
;;; Use g_object_ref() instead.
;;; 
;;; Increases the reference count of the object.
;;; 
;;; object :
;;;     the object to reference.
;;; 
;;; Returns :
;;;     object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_unref ()
;;; 
;;; void gtk_object_unref (GtkObject *object);
;;; 
;;; Warning
;;; 
;;; gtk_object_unref is deprecated and should not be used in newly-written code.
;;; Use g_object_unref() instead.
;;; 
;;; Decreases the reference count of an object. When its reference count drops
;;; to 0, the object is finalized (i.e. its memory is freed).
;;; 
;;; object :
;;;     the object to dereference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_weakref ()
;;; 
;;; void gtk_object_weakref (GtkObject *object,
;;;                          GDestroyNotify notify,
;;;                          gpointer data);
;;; 
;;; Warning
;;; 
;;; gtk_object_weakref is deprecated and should not be used in newly-written
;;; code. Use g_object_weak_ref() instead.
;;; 
;;; Adds a weak reference callback to an object. Weak references are used for
;;; notification when an object is finalized. They are called "weak references"
;;; because they allow you to safely hold a pointer to an object without calling
;;; g_object_ref() (g_object_ref() adds a strong reference, that is, forces the
;;; object to stay alive).
;;; 
;;; object :
;;;     object to weakly reference.
;;; 
;;; notify :
;;;     callback to invoke before the object is freed.
;;; 
;;; data :
;;;     extra data to pass to notify.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_weakunref ()
;;; 
;;; void gtk_object_weakunref (GtkObject *object,
;;;                            GDestroyNotify notify,
;;;                            gpointer data);
;;; 
;;; Warning
;;; 
;;; gtk_object_weakunref is deprecated and should not be used in newly-written
;;; code. Use g_object_weak_unref() instead.
;;; 
;;; Removes a weak reference callback to an object.
;;; 
;;; object :
;;;     object stop weakly referencing.
;;; 
;;; notify :
;;;     callback to search for.
;;; 
;;; data :
;;;     data to search for.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_destroy ()
;;; 
;;; void gtk_object_destroy (GtkObject *object);
;;; 
;;; Warning
;;; 
;;; gtk_object_destroy has been deprecated since version 2.24 and should not be
;;; used in newly-written code. Use gtk_widget_destroy() instead (if object is a
;;; widget)
;;; 
;;; Emits the "destroy" signal notifying all reference holders that they should
;;; release the GtkObject. See the overview documentation at the top of the page
;;; for more details.
;;; 
;;; The memory for the object itself won't be deleted until its reference count
;;; actually drops to 0; gtk_object_destroy() merely asks reference holders to
;;; release their references, it does not free the object.
;;; 
;;; object :
;;;     the object to destroy.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_object_destroy" gtk-object-destroy) :void
  (object (g-object gtk-object)))

(export 'gtk-object-destroy)

;;; ----------------------------------------------------------------------------
;;; gtk_object_get ()
;;; 
;;; void gtk_object_get (GtkObject *object,
;;;                      const gchar *first_property_name,
;;;                      ...);
;;; 
;;; Warning
;;; 
;;; gtk_object_get is deprecated and should not be used in newly-written code.
;;; Use g_object_get() instead.
;;; 
;;; Gets properties of an object.
;;; 
;;; object :
;;;     a GtkObject.
;;; 
;;; first_property_name :
;;;     name of first property to get the value for.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_set ()
;;; 
;;; void gtk_object_set (GtkObject *object,
;;;                      const gchar *first_property_name,
;;;                      ...);
;;; 
;;; Warning
;;; 
;;; gtk_object_set is deprecated and should not be used in newly-written code.
;;; Use g_object_set() instead.
;;; 
;;; Sets properties on an object.
;;; 
;;; void set_box_properties (GtkBox* box)
;;; {
;;;   gtk_object_set (GTK_OBJECT (box), "homogeneous", TRUE,
;;;                                     "spacing", 8,
;;;                                     NULL);
;;; }
;;; 
;;; object :
;;;     a GtkObject.
;;; 
;;; first_property_name :
;;;     name of the first property to set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_set_data ()
;;; 
;;; void gtk_object_set_data (GtkObject *object,
;;;                           const gchar *key,
;;;                           gpointer data);
;;; 
;;; Warning
;;; 
;;; gtk_object_set_data is deprecated and should not be used in newly-written
;;; code. Use g_object_set_data() instead.
;;; 
;;; Each object carries around a table of associations from strings to pointers.
;;; This function lets you set an association.
;;; 
;;; If the object already had an association with that name, the old association
;;; will be destroyed.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; key :
;;;     name of the key.
;;; 
;;; data :
;;;     data to associate with that key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_set_data_full ()
;;; 
;;; void gtk_object_set_data_full (GtkObject *object,
;;;                                const gchar *key,
;;;                                gpointer data,
;;;                                GDestroyNotify destroy);
;;; 
;;; Warning
;;; 
;;; gtk_object_set_data_full is deprecated and should not be used in
;;; newly-written code. Use g_object_set_data_full() instead.
;;; 
;;; Like gtk_object_set_data() except it adds notification for when the
;;; association is destroyed, either by gtk_object_remove_data() or when the
;;; object is destroyed.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; key :
;;;     name of the key.
;;; 
;;; data :
;;;     data to associate with that key.
;;; 
;;; destroy :
;;;     function to call when the association is destroyed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_remove_data ()
;;; 
;;; void gtk_object_remove_data (GtkObject *object, const gchar *key);
;;; 
;;; Warning
;;; 
;;; gtk_object_remove_data is deprecated and should not be used in newly-written
;;; code. Use g_object_set_data() to set the object data to NULL instead.
;;; 
;;; Removes a specified datum from the object's data associations (the
;;; object_data). Subsequent calls to gtk_object_get_data() will return NULL.
;;; 
;;; If you specified a destroy handler with gtk_object_set_data_full(), it will
;;; be invoked.
;;; 
;;; object :
;;;     the object maintaining the association.
;;; 
;;; key :
;;;     name of the key for that association.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_get_data ()
;;; 
;;; gpointer gtk_object_get_data (GtkObject *object, const gchar *key);
;;; 
;;; Warning
;;; 
;;; gtk_object_get_data is deprecated and should not be used in newly-written
;;; code. Use g_object_get_data() instead.
;;; 
;;; Get a named field from the object's table of associations (the object_data).
;;; 
;;; object :
;;;     the object maintaining the associations.
;;; 
;;; key :
;;;     name of the key for that association.
;;; 
;;; Returns :
;;;     the data if found, or NULL if no such data exists.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_remove_no_notify ()
;;; 
;;; void gtk_object_remove_no_notify (GtkObject *object, const gchar *key);
;;; 
;;; Warning
;;; 
;;; gtk_object_remove_no_notify is deprecated and should not be used in
;;; newly-written code. Use g_object_steal_data() instead.
;;; 
;;; Remove a specified datum from the object's data associations (the
;;; object_data), without invoking the association's destroy handler.
;;; 
;;; Just like gtk_object_remove_data() except that any destroy handler will be
;;; ignored. Therefore this only affects data set using
;;; gtk_object_set_data_full().
;;; 
;;; object :
;;;     the object maintaining the association.
;;; 
;;; key :
;;;     name of the key for that association.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_set_user_data ()
;;; 
;;; void gtk_object_set_user_data (GtkObject *object, gpointer data);
;;; 
;;; Warning
;;; 
;;; gtk_object_set_user_data is deprecated and should not be used in
;;; newly-written code. Use g_object_set_data() instead.
;;; 
;;; For convenience, every object offers a generic user data pointer. This
;;; function sets it.
;;; 
;;; object :
;;;     the object whose user data should be set.
;;; 
;;; data :
;;;     the new value for the user data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_get_user_data ()
;;; 
;;; gpointer gtk_object_get_user_data (GtkObject *object);
;;; 
;;; Warning
;;; 
;;; gtk_object_get_user_data is deprecated and should not be used in
;;; newly-written code. Use g_object_get_data() instead.
;;; 
;;; Get the object's user data pointer.
;;; 
;;; This is intended to be a pointer for your convenience in writing
;;; applications.
;;; 
;;; object :
;;;     the object.
;;; 
;;; Returns :
;;;     the user data field for object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_add_arg_type ()
;;; 
;;; void gtk_object_add_arg_type (const gchar *arg_name,
;;;                               GType arg_type,
;;;                               guint arg_flags,
;;;                               guint arg_id);
;;; 
;;; Warning
;;; 
;;; gtk_object_add_arg_type is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Deprecated in favor of the GObject property system including GParamSpec.
;;; Add a new type of argument to an object class. Usually this is called when
;;; registering a new type of object.
;;; 
;;; arg_name :
;;;     fully qualify object name, for example GtkObject::user_data.
;;; 
;;; arg_type :
;;;     type of the argument.
;;; 
;;; arg_flags :
;;;     bitwise-OR of the GtkArgFlags enum. (Whether the argument is settable
;;;     or gettable, whether it is set when the object is constructed.)
;;; 
;;; arg_id :
;;;     an internal number, passed in from here to the "set_arg" and "get_arg"
;;;     handlers of the object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_set_data_by_id ()
;;; 
;;; void gtk_object_set_data_by_id (GtkObject *object,
;;;                                 GQuark data_id,
;;;                                 gpointer data);
;;; 
;;; Warning
;;; 
;;; gtk_object_set_data_by_id is deprecated and should not be used in
;;; newly-written code. Use g_object_set_qdata() instead.
;;; 
;;; Just like gtk_object_set_data() except that it takes a GQuark instead of a
;;; string, so it is slightly faster.
;;; 
;;; Use gtk_object_data_try_key() and gtk_object_data_force_id() to get an id
;;; from a string.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; data_id :
;;;     quark of the key.
;;; 
;;; data :
;;;     data to associate with that key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_set_data_by_id_full ()
;;; 
;;; void gtk_object_set_data_by_id_full (GtkObject *object,
;;;                                      GQuark data_id,
;;;                                      gpointer data,
;;;                                      GDestroyNotify destroy);
;;; 
;;; Warning
;;; 
;;; gtk_object_set_data_by_id_full is deprecated and should not be used in
;;; newly-written code. Use g_object_set_qdata_full() instead.
;;; 
;;; Just like gtk_object_set_data_full() except that it takes a GQuark instead
;;; of a string, so it is slightly faster.
;;; 
;;; Use gtk_object_data_try_key() and gtk_object_data_force_id() to get an id
;;; from a string.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; data_id :
;;;     quark of the key.
;;; 
;;; data :
;;;     data to associate with that key.
;;; 
;;; destroy :
;;;     function to call when the association is destroyed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_get_data_by_id ()
;;; 
;;; gpointer gtk_object_get_data_by_id (GtkObject *object, GQuark data_id);
;;; 
;;; Warning
;;; 
;;; gtk_object_get_data_by_id is deprecated and should not be used in
;;; newly-written code. Use g_object_get_qdata() instead.
;;; 
;;; Just like gtk_object_get_data() except that it takes a GQuark instead of a
;;; string, so it is slightly faster.
;;; 
;;; Use gtk_object_data_try_key() and gtk_object_data_force_id() to get an id
;;; from a string.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; data_id :
;;;     quark of the key.
;;; 
;;; Returns :
;;;     the data if found, or NULL if no such data exists.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_remove_data_by_id ()
;;; 
;;; void gtk_object_remove_data_by_id (GtkObject *object, GQuark data_id);
;;; 
;;; Warning
;;; 
;;; gtk_object_remove_data_by_id is deprecated and should not be used in
;;; newly-written code. Use g_object_set_qdata() with data of NULL instead.
;;; 
;;; Just like gtk_object_remove_data() except that it takes a GQuark instead of
;;; a string, so it is slightly faster.
;;; 
;;; Remove a specified datum from the object's data associations. Subsequent
;;; calls to gtk_object_get_data() will return NULL.
;;; 
;;; Use gtk_object_data_try_key() and gtk_object_data_force_id() to get an id
;;; from a string.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; data_id :
;;;     quark of the key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_remove_no_notify_by_id ()
;;; 
;;; void gtk_object_remove_no_notify_by_id (GtkObject *object, GQuark key_id);
;;; 
;;; Warning
;;; 
;;; gtk_object_remove_no_notify_by_id is deprecated and should not be used in
;;; newly-written code. Use g_object_steal_qdata() instead.
;;; 
;;; Just like gtk_object_remove_no_notify() except that it takes a GQuark
;;; instead of a string, so it is slightly faster.
;;; 
;;; Use gtk_object_data_try_key() and gtk_object_data_force_id() to get an id
;;; from a string.
;;; 
;;; object :
;;;     object containing the associations.
;;; 
;;; key_id :
;;;     quark of the key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_data_try_key
;;; 
;;; #define gtk_object_data_try_key        g_quark_try_string
;;; 
;;; Warning
;;; 
;;; gtk_object_data_try_key is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Useless deprecated macro. Ignore it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_object_data_force_id
;;; 
;;; #define gtk_object_data_force_id    g_quark_from_string
;;; 
;;; Warning
;;; 
;;; gtk_object_data_force_id is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Useless deprecated macro. Ignore it.
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.object.lisp --------------------------------------------
