;;; ----------------------------------------------------------------------------
;;; gobject.closures.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; Closures
;;;
;;;     Functions as first-class objects
;;;
;;; Types and Values
;;;
;;;     GClosure
;;;     G_TYPE_CLOSURE
;;;     GCClosure
;;;     g_cclosure_marshal_BOOL__FLAGS
;;;     g_cclosure_marshal_BOOL__BOXED_BOXED
;;;
;;; Functions
;;;
;;;     G_CLOSURE_NEEDS_MARSHAL
;;;     G_CLOSURE_N_NOTIFIERS
;;;     G_CCLOSURE_SWAP_DATA
;;;     G_CALLBACK
;;;
;;;     GCallback
;;;     GClosureMarshal
;;;     GVaClosureMarshal
;;;     GClosureNotify
;;;
;;;     g_cclosure_new
;;;     g_cclosure_new_swap
;;;     g_cclosure_new_object
;;;     g_cclosure_new_object_swap
;;;     g_cclosure_marshal_generic
;;;     g_closure_new_object
;;;     g_closure_ref
;;;     g_closure_sink
;;;     g_closure_unref
;;;     g_closure_invoke
;;;     g_closure_invalidate
;;;     g_closure_add_finalize_notifier
;;;     g_closure_add_invalidate_notifier
;;;     g_closure_remove_finalize_notifier
;;;     g_closure_remove_invalidate_notifier
;;;     g_closure_new_simple
;;;     g_closure_set_marshal
;;;     g_closure_add_marshal_guards
;;;     g_closure_set_meta_marshal
;;;     g_source_set_closure
;;;     g_source_set_dummy_callback
;;;
;;;     g_cclosure_marshal_VOID__VOID
;;;     g_cclosure_marshal_VOID__BOOLEAN
;;;     g_cclosure_marshal_VOID__CHAR
;;;     g_cclosure_marshal_VOID__UCHAR
;;;     g_cclosure_marshal_VOID__INT
;;;     g_cclosure_marshal_VOID__UINT
;;;     g_cclosure_marshal_VOID__LONG
;;;     g_cclosure_marshal_VOID__ULONG
;;;     g_cclosure_marshal_VOID__ENUM
;;;     g_cclosure_marshal_VOID__FLAGS
;;;     g_cclosure_marshal_VOID__FLOAT
;;;     g_cclosure_marshal_VOID__DOUBLE
;;;     g_cclosure_marshal_VOID__STRING
;;;     g_cclosure_marshal_VOID__PARAM
;;;     g_cclosure_marshal_VOID__BOXED
;;;     g_cclosure_marshal_VOID__POINTER
;;;     g_cclosure_marshal_VOID__OBJECT
;;;     g_cclosure_marshal_VOID__VARIANT
;;;     g_cclosure_marshal_STRING__OBJECT_POINTER
;;;     g_cclosure_marshal_VOID__UINT_POINTER
;;;     g_cclosure_marshal_BOOLEAN__FLAGS
;;;     g_cclosure_marshal_BOOLEAN__BOXED_BOXED
;;;     g_cclosure_marshal_generic_va
;;;     g_cclosure_marshal_VOID__VOIDv
;;;     g_cclosure_marshal_VOID__BOOLEANv
;;;     g_cclosure_marshal_VOID__CHARv
;;;     g_cclosure_marshal_VOID__UCHARv
;;;     g_cclosure_marshal_VOID__INTv
;;;     g_cclosure_marshal_VOID__UINTv
;;;     g_cclosure_marshal_VOID__LONGv
;;;     g_cclosure_marshal_VOID__ULONGv
;;;     g_cclosure_marshal_VOID__ENUMv
;;;     g_cclosure_marshal_VOID__FLAGSv
;;;     g_cclosure_marshal_VOID__FLOATv
;;;     g_cclosure_marshal_VOID__DOUBLEv
;;;     g_cclosure_marshal_VOID__STRINGv
;;;     g_cclosure_marshal_VOID__PARAMv
;;;     g_cclosure_marshal_VOID__BOXEDv
;;;     g_cclosure_marshal_VOID__POINTERv
;;;     g_cclosure_marshal_VOID__OBJECTv
;;;     g_cclosure_marshal_VOID__VARIANTv
;;;     g_cclosure_marshal_STRING__OBJECT_POINTERv
;;;     g_cclosure_marshal_VOID__UINT_POINTERv
;;;     g_cclosure_marshal_BOOLEAN__FLAGSv
;;;     g_cclosure_marshal_BOOLEAN__BOXED_BOXEDv
;;;
;;; Description
;;;
;;; A GClosure represents a callback supplied by the programmer. It will
;;; generally comprise a function of some kind and a marshaller used to call it.
;;; It is the reponsibility of the marshaller to convert the arguments for the
;;; invocation from GValues into a suitable form, perform the callback on the
;;; converted arguments, and transform the return value back into a GValue.
;;;
;;; In the case of C programs, a closure usually just holds a pointer to a
;;; function and maybe a data argument, and the marshaller converts between
;;; GValue and native C types. The GObject library provides the GCClosure type
;;; for this purpose. Bindings for other languages need marshallers which
;;; convert between GValues and suitable representations in the runtime of the
;;; language in order to use functions written in that languages as callbacks.
;;;
;;; Within GObject, closures play an important role in the implementation of
;;; signals. When a signal is registered, the c_marshaller argument to
;;; g_signal_new() specifies the default C marshaller for any closure which is
;;; connected to this signal. GObject provides a number of C marshallers for
;;; this purpose, see the g_cclosure_marshal_*() functions. Additional C
;;; marshallers can be generated with the glib-genmarshal utility. Closures can
;;; be explicitly connected to signals with g_signal_connect_closure(), but it
;;; usually more convenient to let GObject create a closure automatically by
;;; using one of the g_signal_connect_*() functions which take a callback
;;; function/user data pair.
;;;
;;; Using closures has a number of important advantages over a simple callback
;;; function/data pointer combination:
;;;
;;;     Closures allow the callee to get the types of the callback parameters,
;;;     which means that language bindings don't have to write individual glue
;;;     for each callback type.
;;;
;;;     The reference counting of GClosure makes it easy to handle reentrancy
;;;     right; if a callback is removed while it is being invoked, the closure
;;;     and its parameters won't be freed until the invocation finishes.
;;;
;;;     g_closure_invalidate() and invalidation notifiers allow callbacks to be
;;;     automatically removed when the objects they point to go away.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; G_CLOSURE_NEEDS_MARSHAL()
;;;
;;; #define G_CLOSURE_NEEDS_MARSHAL(closure)
;;;         (((GClosure*) (closure))->marshal == NULL)
;;;
;;; Check if the closure still needs a marshaller. See g_closure_set_marshal().
;;;
;;; closure :
;;;     a GClosure
;;;
;;; Returns :
;;;     TRUE if a GClosureMarshal marshaller has not yet been set on closure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_CLOSURE_N_NOTIFIERS()
;;;
;;; #define G_CLOSURE_N_NOTIFIERS(cl)
;;;
;;; Get the total number of notifiers connected with the closure cl. The count
;;; includes the meta marshaller, the finalize and invalidate notifiers and the
;;; marshal guards. Note that each guard counts as two notifiers. See
;;; g_closure_set_meta_marshal(), g_closure_add_finalize_notifier(),
;;; g_closure_add_invalidate_notifier() and g_closure_add_marshal_guards().
;;;
;;; cl :
;;;     a GClosure
;;;
;;; Returns :
;;;     number of notifiers
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_CCLOSURE_SWAP_DATA()
;;;
;;; #define G_CCLOSURE_SWAP_DATA(cclosure)
;;;         (((GClosure*) (cclosure))->derivative_flag)
;;;
;;; Checks whether the user data of the GCClosure should be passed as the first
;;; parameter to the callback. See g_cclosure_new_swap().
;;;
;;; cclosure :
;;;     a GCClosure
;;;
;;; Returns :
;;;     TRUE if data has to be swapped.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_CALLBACK()
;;;
;;; #define G_CALLBACK(f) ((GCallback) (f))
;;;
;;; Cast a function pointer to a GCallback.
;;;
;;; f :
;;;     a function pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GCallback ()
;;;
;;; void (*GCallback) (void);
;;;
;;; The type used for callback functions in structure definitions and function
;;; signatures. This does not mean that all callback functions must take no
;;; parameters and return void. The required signature of a callback function is
;;; determined by the context in which is used (e.g. the signal to which it is
;;; connected). Use G_CALLBACK() to cast the callback function to a GCallback.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GClosure
;;; ----------------------------------------------------------------------------

(defcstruct g-closure
  (:private-data :uint32)
  (:marshal :pointer)
  (:data :pointer)
  (:notifiers :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-closure atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-closure atdoc:*external-symbols*)
 "@version{2020-10-18}
  @begin{short}
    A @sym{g-closure} structure represents a callback supplied by the
    programmer.
  @end{short}
  @begin{pre}
(defcstruct g-closure
  (:private-data :uint32)
  (:marshal :pointer)
  (:data :pointer)
  (:notifiers :pointer))
  @end{pre}")

(export 'g-closure)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CLOSURE
;;; ----------------------------------------------------------------------------

(defcfun ("g_closure_get_type" g-type-closure) g-type
 #+cl-cffi-gtk-documentation
 "@version{2020-10-18}
  @begin{short}
    Returns the @class{g-type} ID for a @symbol{g-closure} structure.
  @end{short}
  @see-symbol{g-closure}")

(glib-init::at-init nil (g-type-closure))

(export 'g-type-closure)

;;; ----------------------------------------------------------------------------
;;; struct GCClosure
;;;
;;; struct GCClosure {
;;;   GClosure closure;
;;;   gpointer callback;
;;; };
;;;
;;; A GCClosure is a specialization of GClosure for C function callbacks.
;;;
;;; GClosure closure;
;;;     the GClosure
;;;
;;; gpointer callback;
;;;     the callback function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClosureMarshal ()
;;;
;;; void (*GClosureMarshal) (GClosure *closure,
;;;                          GValue *return_value,
;;;                          guint n_param_values,
;;;                          const GValue *param_values,
;;;                          gpointer invocation_hint,
;;;                          gpointer marshal_data);
;;;
;;; The type used for marshaller functions.
;;;
;;; closure :
;;;     the GClosure to which the marshaller belongs
;;;
;;; return_value :
;;;     a GValue to store the return value. May be NULL if the callback of
;;;     closure does not return a value
;;;
;;; n_param_values :
;;;     the length of the param_values array
;;;
;;; param_values :
;;;     an array of GValues holding the arguments on which to invoke the
;;;     callback of closure
;;;
;;; invocation_hint :
;;;     the invocation hint given as the last argument to g_closure_invoke()
;;;
;;; marshal_data :
;;;     additional data specified when registering the marshaller, see
;;;     g_closure_set_marshal() and g_closure_set_meta_marshal()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClosureNotify ()
;;;
;;; void (*GClosureNotify) (gpointer data, GClosure *closure);
;;;
;;; The type used for the various notification callbacks which can be registered
;;; on closures.
;;;
;;; data :
;;;     data specified when registering the notification callback
;;;
;;; closure :
;;;     the GClosure on which the notification is emitted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new ()
;;;
;;; GClosure * g_cclosure_new (GCallback callback_func,
;;;                            gpointer user_data,
;;;                            GClosureNotify destroy_data);
;;;
;;; Creates a new closure which invokes callback_func with user_data as the last
;;; parameter.
;;;
;;; callback_func :
;;;     the function to invoke
;;;
;;; user_data :
;;;     user data to pass to callback_func
;;;
;;; destroy_data :
;;;     destroy notify to be called when user_data is no longer used
;;;
;;; Returns :
;;;     a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new_swap ()
;;;
;;; GClosure * g_cclosure_new_swap (GCallback callback_func,
;;;                                 gpointer user_data,
;;;                                 GClosureNotify destroy_data);
;;;
;;; Creates a new closure which invokes callback_func with user_data as the
;;; first parameter.
;;;
;;; callback_func :
;;;     the function to invoke
;;;
;;; user_data :
;;;     user data to pass to callback_func
;;;
;;; destroy_data :
;;;     destroy notify to be called when user_data is no longer used
;;;
;;; Returns :
;;;     a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new_object ()
;;;
;;; GClosure * g_cclosure_new_object (GCallback callback_func, GObject *object);
;;;
;;; A variant of g_cclosure_new() which uses object as user_data and calls
;;; g_object_watch_closure() on object and the created closure. This function is
;;; useful when you have a callback closely associated with a GObject, and want
;;; the callback to no longer run after the object is is freed.
;;;
;;; callback_func :
;;;     the function to invoke
;;;
;;; object :
;;;     a GObject pointer to pass to callback_func
;;;
;;; Returns :
;;;     a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new_object_swap ()
;;;
;;; GClosure * g_cclosure_new_object_swap (GCallback callback_func,
;;;                                        GObject *object);
;;;
;;; A variant of g_cclosure_new_swap() which uses object as user_data and calls
;;; g_object_watch_closure() on object and the created closure. This function is
;;; useful when you have a callback closely associated with a GObject, and want
;;; the callback to no longer run after the object is is freed.
;;;
;;; callback_func :
;;;     the function to invoke
;;;
;;; object :
;;;     a GObject pointer to pass to callback_func
;;;
;;; Returns :
;;;     a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_generic ()
;;;
;;; void g_cclosure_marshal_generic (GClosure *closure,
;;;                                  GValue *return_gvalue,
;;;                                  guint n_param_values,
;;;                                  const GValue *param_values,
;;;                                  gpointer invocation_hint,
;;;                                  gpointer marshal_data);
;;;
;;; A generic marshaller function implemented via libffi.
;;;
;;; closure :
;;;     A GClosure.
;;;
;;; return_gvalue :
;;;     A GValue to store the return value. May be NULL if the callback of
;;;     closure does not return a value.
;;;
;;; n_param_values :
;;;     The length of the param_values array.
;;;
;;; param_values :
;;;     An array of GValues holding the arguments on which to invoke the
;;;     callback of closure.
;;;
;;; invocation_hint :
;;;     The invocation hint given as the last argument to g_closure_invoke().
;;;
;;; marshal_data :
;;;     Additional data specified when registering the marshaller, see
;;;     g_closure_set_marshal() and g_closure_set_meta_marshal()
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_new_object ()
;;;
;;; GClosure * g_closure_new_object (guint sizeof_closure, GObject *object);
;;;
;;; A variant of g_closure_new_simple() which stores object in the data field of
;;; the closure and calls g_object_watch_closure() on object and the created
;;; closure. This function is mainly useful when implementing new types of
;;; closures.
;;;
;;; sizeof_closure :
;;;     the size of the structure to allocate, must be at least sizeof
;;;     (GClosure)
;;;
;;; object :
;;;     a GObject pointer to store in the data field of the newly allocated
;;;     GClosure
;;;
;;; Returns :
;;;     a newly allocated GClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_ref ()
;;; ----------------------------------------------------------------------------

;; Not used in the Lisp library and not exported.

(defcfun ("g_closure_ref" g-closure-ref) (:pointer (:struct g-closure))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{@symbol{g-closure} to increment the reference count on}
  @return{The @arg{closure} passed in, for convenience.}
  Increments the reference count on a @arg{closure} to force it staying alive
  while the caller holds a pointer to it."
  (closure (:pointer (:struct g-closure))))

;;; ----------------------------------------------------------------------------
;;; g_closure_sink ()
;;; ----------------------------------------------------------------------------

;; Not used in the Lisp library and not exported.

(defcfun ("g_closure_sink" g-closure-sink) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{@symbol{g-closure} to decrement the initial reference count
    on, if it is still being held}
  @begin{short}
    Takes over the initial ownership of a @arg{closure}.
  @end{short}
  Each closure is initially created in a floating state, which means that the
  initial reference count is not owned by any caller. The @sym{g-closure-sink}
  function checks to see if the object is still floating, and if so, unsets the
  floating state and decreases the reference count. If the closure is not
  floating, the @fun{g-closure-sink} function does nothing. The reason for the
  existence of the floating state is to prevent cumbersome code sequences like:
  @begin{pre}
   closure = g_cclosure_new (cb_func, cb_data);
   g_source_set_closure (source, closure);
   g_closure_unref (closure); // XXX GObject does not really need this
  @end{pre}
  Because the @fun{g-source-set-closure} function (and similar functions) take
  ownership of the initial reference count, if it is unowned, we instead can
  write:
  @begin{pre}
   g_source_set_closure (source, g_cclosure_new (cb_func, cb_data));
  @end{pre}
  Generally, this function is used together with the @fun{g-closure-ref}
  function. Ane example of storing a closure for later notification looks like:
  @begin{pre}
   static GClosure *notify_closure = NULL;
   void
   foo_notify_set_closure (GClosure *closure)
   {
     if (notify_closure)
       g_closure_unref (notify_closure);
     notify_closure = closure;
     if (notify_closure)
       {
         g_closure_ref (notify_closure);
         g_closure_sink (notify_closure);
       @}
   @}
  @end{pre}
  Because the @sym{g-closure-sink} may decrement the reference count of a
  closure (if it has not been called on closure yet) just like the functions
  @fun{g-closure-unref}, @fun{g-closure-ref} should be called prior to this
  function."
  (closure (:pointer (:struct g-closure))))

;;; ----------------------------------------------------------------------------
;;; g_closure_unref ()
;;; ----------------------------------------------------------------------------

;; Not used in the Lisp library and not exported.

(defcfun ("g_closure_unref" g-closure-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{@symbol{g-closure} to decrement the reference count on}
  Decrements the reference count of a @arg{closure} after it was previously
  incremented by the same caller. If no other callers are using the
  @arg{closure}, then the closure will be destroyed and freed."
  (closure (:pointer (:struct g-closure))))

;;; ----------------------------------------------------------------------------
;;; g_closure_invoke ()
;;;
;;; void g_closure_invoke (GClosure *closure,
;;;                        GValue *return_value,
;;;                        guint n_param_values,
;;;                        const GValue *param_values,
;;;                        gpointer invocation_hint);
;;;
;;; Invokes the closure, i.e. executes the callback represented by the closure.
;;;
;;; closure :
;;;     a GClosure
;;;
;;; return_value :
;;;     a GValue to store the return value. May be NULL if the callback of
;;;     closure does not return a value
;;;
;;; n_param_values :
;;;     the length of the param_values array
;;;
;;; param_values :
;;;     an array of GValues holding the arguments on which to invoke the
;;;     callback of closure
;;;
;;; invocation_hint :
;;;     a context-dependent invocation hint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_invalidate ()
;;; ----------------------------------------------------------------------------

;; Not used in the Lisp library and not exported.

(defcfun ("g_closure_invalidate" g-closure-invalidate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{@symbol{g-closure} to invalidate}
  @begin{short}
    Sets a flag on the @arg{closure} to indicate that its calling environment
    has become invalid, and thus causes any future invocations of the
    @code{g_closure_invoke()} function on this @arg{closure} to be ignored.
  @end{short}
  Also, invalidation notifiers installed on the @arg{closure} will be called at
  this point. Note that unless you are holding a reference to the closure
  yourself, the invalidation notifiers may unref the closure and cause it to be
  destroyed, so if you need to access the closure after calling the
  @sym{g-closure-invalidate} function, make sure that you have previously
  called the @fun{g-closure-ref} function.

  Note that the @sym{g-closure-invalidate} function will also be called when the
  reference count of a closure drops to zero (unless it has already been
  invalidated before)."
  (closure (:pointer (:struct g-closure))))

;;; ----------------------------------------------------------------------------
;;; g_closure_add_finalize_notifier ()
;;; ----------------------------------------------------------------------------

;; Used to implement signals in the Lisp library, but not exported.

(defcfun ("g_closure_add_finalize_notifier" g-closure-add-finalize-notifier)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{a @symbol{g-closure}}
  @argument[notify-data]{data to pass to @arg{notify-func}}
  @argument[notify-func]{the callback function to register}
  @begin{short}
    Registers a finalization notifier which will be called when the reference
    count of closure goes down to 0.
  @end{short}
  Multiple finalization notifiers on a single closure are invoked in unspecified
  order. If a single call to the @fun{g-closure-unref} function results in the
  closure being both invalidated and finalized, then the invalidate notifiers
  will be run before the finalize notifiers."
  (closure (:pointer (:struct g-closure)))
  (notify-data :pointer)
  (notify-func :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_add_invalidate_notifier ()
;;; ----------------------------------------------------------------------------

;; Not used in the Lisp library and not exported.

(defcfun ("g_closure_add_invalidate_notifier" g-closure-add-invalidate-notifier)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{a @symbol{g-closure}}
  @argument[notify-data]{data to pass to @arg{notify-func}}
  @argument[notify-func]{the callback function to register}
  @begin{short}
    Registers an invalidation notifier which will be called when the closure is
    invalidated with the @fun{g-closure-invalidate} function.
  @end{short}
  Invalidation notifiers are invoked before finalization notifiers, in an
  unspecified order."
  (closure (:pointer (:struct g-closure)))
  (notify-data :pointer)
  (notify-func :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_remove_finalize_notifier ()
;;;
;;; void g_closure_remove_finalize_notifier (GClosure *closure,
;;;                                          gpointer notify_data,
;;;                                          GClosureNotify notify_func);
;;;
;;; Removes a finalization notifier.
;;;
;;; Notice that notifiers are automatically removed after they are run.
;;;
;;; closure :
;;;     a GClosure
;;;
;;; notify_data :
;;;     data which was passed to g_closure_add_finalize_notifier() when
;;;     registering notify_func
;;;
;;; notify_func :
;;;     the callback function to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_remove_invalidate_notifier ()
;;;
;;; void g_closure_remove_invalidate_notifier (GClosure *closure,
;;;                                            gpointer notify_data,
;;;                                            GClosureNotify notify_func);
;;;
;;; Removes an invalidation notifier.
;;;
;;; Notice that notifiers are automatically removed after they are run.
;;;
;;; closure :
;;;     a GClosure
;;;
;;; notify_data :
;;;     data which was passed to g_closure_add_invalidate_notifier() when
;;;     registering notify_func
;;;
;;; notify_func :
;;;     the callback function to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_new_simple ()
;;; ----------------------------------------------------------------------------

;; Used to implement signals in the Lisp library, but not exported.

(defcfun ("g_closure_new_simple" g-closure-new-simple)
    (:pointer (:struct g-closure))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[sizeof-closure]{the size of the structure to allocate, must be at
    least @code{sizeof (GClosure)}}
  @argument[data]{data to store in the data field of the newly allocated
    @symbol{g-closure}}
  @return{A newly allocated @symbol{g-closure}.}
  @begin{short}
    Allocates a structure of the given size and initializes the initial part as
    a @symbol{g-closure}.
  @end{short}
  This function is mainly useful when implementing new types of closures.
  @begin{pre}
   typedef struct _MyClosure MyClosure;
   struct _MyClosure
   {
     GClosure closure;
     // extra data goes here
   @};

   static void
   my_closure_finalize (gpointer  notify_data,
                        GClosure *closure)
   {
     MyClosure *my_closure = (MyClosure *)closure;

     // free extra data here
   @}

   MyClosure *my_closure_new (gpointer data)
   {
     GClosure *closure;
     MyClosure *my_closure;

     closure = g_closure_new_simple (sizeof (MyClosure), data);
     my_closure = (MyClosure *) closure;

     // initialize extra data here

     g_closure_add_finalize_notifier (closure, notify_data,
                                      my_closure_finalize);
     return my_closure;
   @}
  @end{pre}"
  (sizeof-closure :uint)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_set_marshal ()
;;; ----------------------------------------------------------------------------

;; Used to implement signals in the Lisp library, but not exported.

(defcfun ("g_closure_set_marshal" g-closure-set-marshal) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[closure]{a @symbol{g-closure}}
  @argument[marshal]{a @code{GClosureMarshal} function}
  @begin{short}
    Sets the marshaller of closure.
  @end{short}
  The @arg{marshal-data} of marshal provides a way for a meta marshaller to
  provide additional information to the marshaller. (See the
  @fun{g-closure-set-meta-marshal}.) For GObject's C predefined marshallers
  (the @code{g_cclosure_marshal_*()} functions), what it provides is a callback
  function to use instead of @code{closure->callback}."
  (closure (:pointer (:struct g-closure)))
  (marshal :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_add_marshal_guards ()
;;;
;;; void g_closure_add_marshal_guards (GClosure *closure,
;;;                                    gpointer pre_marshal_data,
;;;                                    GClosureNotify pre_marshal_notify,
;;;                                    gpointer post_marshal_data,
;;;                                    GClosureNotify post_marshal_notify);
;;;
;;; Adds a pair of notifiers which get invoked before and after the closure
;;; callback, respectively. This is typically used to protect the extra
;;; arguments for the duration of the callback. See g_object_watch_closure() for
;;; an example of marshal guards.
;;;
;;; closure :
;;;     a GClosure
;;;
;;; pre_marshal_data :
;;;     data to pass to pre_marshal_notify
;;;
;;; pre_marshal_notify :
;;;     a function to call before the closure callback
;;;
;;; post_marshal_data :
;;;     data to pass to post_marshal_notify
;;;
;;; post_marshal_notify :
;;;     a function to call after the closure callback
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_set_meta_marshal ()
;;;
;;; void g_closure_set_meta_marshal (GClosure *closure,
;;;                                  gpointer marshal_data,
;;;                                  GClosureMarshal meta_marshal);
;;;
;;; Sets the meta marshaller of closure. A meta marshaller wraps
;;; closure->marshal and modifies the way it is called in some fashion. The most
;;; common use of this facility is for C callbacks. The same marshallers
;;; (generated by glib-genmarshal) are used everywhere, but the way that we get
;;; the callback function differs. In most cases we want to use
;;; closure->callback, but in other cases we want to use some different
;;; technique to retrieve the callback function.
;;;
;;; For example, class closures for signals (see g_signal_type_cclosure_new())
;;; retrieve the callback function from a fixed offset in the class structure.
;;; The meta marshaller retrieves the right callback and passes it to the
;;; marshaller as the marshal_data argument.
;;;
;;; closure :
;;;     a GClosure
;;;
;;; marshal_data :
;;;     context-dependent data to pass to meta_marshal
;;;
;;; meta_marshal :
;;;     a GClosureMarshal function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_closure ()
;;;
;;; void g_source_set_closure (GSource *source, GClosure *closure);
;;;
;;; Set the callback for a source as a GClosure.
;;;
;;; If the source is not one of the standard GLib types, the closure_callback
;;; and closure_marshal fields of the GSourceFuncs structure must have been
;;; filled in with pointers to appropriate functions.
;;;
;;; source :
;;;     the source
;;;
;;; closure :
;;;     a GClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_dummy_callback ()
;;;
;;; void g_source_set_dummy_callback (GSource *source);
;;;
;;; Sets a dummy callback for source. The callback will do nothing, and if the
;;; source expects a gboolean return value, it will return TRUE. (If the source
;;; expects any other type of return value, it will return a 0/NULL value;
;;; whatever g_value_init() initializes a GValue to for that type.)
;;;
;;; If the source is not one of the standard GLib types, the closure_callback
;;; and closure_marshal fields of the GSourceFuncs structure must have been
;;; filled in with pointers to appropriate functions.
;;;
;;; source :
;;;     the source
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.closures.lisp --------------------------------------
