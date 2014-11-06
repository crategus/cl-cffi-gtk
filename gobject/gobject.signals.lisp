;;; ----------------------------------------------------------------------------
;;; gobject.signals.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.36.2 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; Signals
;;;
;;; A means for customization of object behaviour and a general purpose
;;; notification mechanism
;;;
;;; Synopsis
;;;
;;;     GSignalInvocationHint
;;;     GSignalCMarshaller
;;;     GSignalFlags
;;;     GSignalMatchType
;;;     GSignalQuery
;;;
;;;     G_SIGNAL_TYPE_STATIC_SCOPE
;;;     G_SIGNAL_MATCH_MASK
;;;     G_SIGNAL_FLAGS_MASK
;;;
;;;     g_signal_new
;;;     g_signal_newv
;;;     g_signal_new_valist
;;;     g_signal_query
;;;     g_signal_lookup
;;;     g_signal_name
;;;     g_signal_list_ids
;;;     g_signal_emit
;;;     g_signal_emit_by_name
;;;     g_signal_emitv
;;;     g_signal_emit_valist
;;;     g_signal_connect
;;;     g_signal_connect_after
;;;     g_signal_connect_swapped
;;;     g_signal_connect_object
;;;
;;;     GConnectFlags
;;;
;;;     g_signal_connect_data
;;;     g_signal_connect_closure
;;;     g_signal_connect_closure_by_id
;;;     g_signal_handler_block
;;;     g_signal_handler_unblock
;;;     g_signal_handler_disconnect
;;;     g_signal_handler_find
;;;     g_signal_handlers_block_matched
;;;     g_signal_handlers_unblock_matched
;;;     g_signal_handlers_disconnect_matched
;;;     g_signal_handler_is_connected
;;;     g_signal_handlers_block_by_func
;;;     g_signal_handlers_unblock_by_func
;;;     g_signal_handlers_disconnect_by_func
;;;     g_signal_handlers_disconnect_by_data
;;;     g_signal_has_handler_pending
;;;     g_signal_stop_emission
;;;     g_signal_stop_emission_by_name
;;;     g_signal_override_class_closure
;;;     g_signal_chain_from_overridden
;;;     g_signal_new_class_handler
;;;     g_signal_override_class_handler
;;;     g_signal_chain_from_overridden_handler
;;;     g_signal_add_emission_hook
;;;     g_signal_remove_emission_hook
;;;     g_signal_parse_name
;;;     g_signal_get_invocation_hint
;;;     g_signal_type_cclosure_new
;;;     g_signal_accumulator_first_wins
;;;     g_signal_accumulator_true_handled
;;;
;;; Description
;;;
;;; The basic concept of the signal system is that of the emission of a signal.
;;; Signals are introduced per-type and are identified through strings. Signals
;;; introduced for a parent type are available in derived types as well, so
;;; basically they are a per-type facility that is inherited. A signal emission
;;; mainly involves invocation of a certain set of callbacks in precisely
;;; defined manner. There are two main categories of such callbacks, per-object
;;; [10] ones and user provided ones. The per-object callbacks are most often
;;; referred to as "object method handler" or "default (signal) handler", while
;;; user provided callbacks are usually just called "signal handler". The object
;;; method handler is provided at signal creation time (this most frequently
;;; happens at the end of an object class' creation), while user provided
;;; handlers are frequently connected and disconnected to/from a certain signal
;;; on certain object instances.
;;;
;;; A signal emission consists of five stages, unless prematurely stopped:
;;;
;;; 1 - Invocation of the object method handler for G_SIGNAL_RUN_FIRST signals
;;;
;;; 2 - Invocation of normal user-provided signal handlers (after flag FALSE)
;;;
;;; 3 - Invocation of the object method handler for G_SIGNAL_RUN_LAST signals
;;;
;;; 4 - Invocation of user provided signal handlers, connected with an after
;;;     flag of TRUE
;;;
;;; 5 - Invocation of the object method handler for G_SIGNAL_RUN_CLEANUP signals
;;;
;;; The user-provided signal handlers are called in the order they were
;;; connected in. All handlers may prematurely stop a signal emission, and any
;;; number of handlers may be connected, disconnected, blocked or unblocked
;;; during a signal emission. There are certain criteria for skipping user
;;; handlers in stages 2 and 4 of a signal emission. First, user handlers may be
;;; blocked, blocked handlers are omitted during callback invocation, to return
;;; from the "blocked" state, a handler has to get unblocked exactly the same
;;; amount of times it has been blocked before. Second, upon emission of a
;;; G_SIGNAL_DETAILED signal, an additional "detail" argument passed in to
;;; g_signal_emit() has to match the detail argument of the signal handler
;;; currently subject to invocation. Specification of no detail argument for
;;; signal handlers (omission of the detail part of the signal specification
;;; upon connection) serves as a wildcard and matches any detail argument passed
;;; in to emission.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------

;; A lisp-closure is a specialization of g-closure for Lisp function callbacks.

(defcstruct lisp-closure
  (:parent-instance (:struct g-closure)) ; A structure, not a pointer.
  (:object :pointer)
  (:function-id :int))

(export 'lisp-closure)

;;; ----------------------------------------------------------------------------

;; Called from g-signal-connect to create the callback function

(defun create-closure (object fn)
  (let ((function-id (save-handler-to-object object fn))
        (closure (g-closure-new-simple (foreign-type-size '(:struct lisp-closure))
                                       (null-pointer))))
    (setf (foreign-slot-value closure '(:struct lisp-closure) :function-id)
          function-id
          (foreign-slot-value closure '(:struct lisp-closure) :object)
          (pointer object))
    (g-closure-add-finalize-notifier closure
                                     (null-pointer)
                                     (callback lisp-closure-finalize))
    (g-closure-set-marshal closure (callback lisp-closure-marshal))
    closure))

;;; ----------------------------------------------------------------------------

;; Helper function for create-closure:
;; Store the new handler in the array of signal handlers of the GObject and
;; return the id of the handler in the array.

(defun save-handler-to-object (object handler)
  (flet ((find-free-signal-handler-id (object)
            (iter (with handlers = (g-object-signal-handlers object))
                  (for i from 0 below (length handlers))
                  (finding i such-that (null (aref handlers i))))))
    (let ((id (find-free-signal-handler-id object))
          (handlers (g-object-signal-handlers object)))
      (if id
          (progn
            (setf (aref handlers id) handler)
            id)
          (progn
            (vector-push-extend handler handlers)
            (1- (length handlers)))))))

;;; ----------------------------------------------------------------------------

;; A GClosureMarshal function used when creating a signal handler

(defcallback lisp-closure-marshal :void
    ((closure (:pointer (:struct lisp-closure)))
     (return-value (:pointer (:struct g-value)))
     (count-of-args :uint)
     (args (:pointer (:struct g-value)))
     (invocation-hint :pointer)
     (marshal-data :pointer))
  (declare (ignore invocation-hint marshal-data))
  (let* ((args (parse-closure-arguments count-of-args args))
         (function-id (foreign-slot-value closure
                                          '(:struct lisp-closure)
                                          :function-id))
         (addr (pointer-address (foreign-slot-value closure
                                                    '(:struct lisp-closure)
                                                    :object)))
         (object (or (gethash addr *foreign-gobjects-strong*)
                     (gethash addr *foreign-gobjects-weak*)))
         (return-type (and (not (null-pointer-p return-value))
                           (g-value-type return-value)))
         (fn (retrieve-handler-from-object object function-id))
         (fn-result (call-with-restarts fn args)))
    (when return-type
      (set-g-value return-value fn-result return-type :g-value-init nil))))

;;; ----------------------------------------------------------------------------

;; Helper functions for lisp-closure-marshal

(defun parse-closure-arguments (count-of-args args)
  (loop
     for i from 0 below count-of-args
     collect (parse-g-value (mem-aptr args '(:struct g-value) i))))

(defun retrieve-handler-from-object (object handler-id)
  (aref (g-object-signal-handlers object) handler-id))

(defun call-with-restarts (fn args)
  (restart-case
      (apply fn args)
    (return-from-g-closure (&optional v)
                           :report "Return value from closure" v)))

;;; ----------------------------------------------------------------------------

;; A finalization notifier function used when creating a signal handler

(defcallback lisp-closure-finalize :void
    ((data :pointer)
     (closure (:pointer (:struct lisp-closure))))
  (declare (ignore data))
  (finalize-lisp-closure closure))

;;; ----------------------------------------------------------------------------

;; Helper functions for lisp-signal-handler-closure-finalize

(defun finalize-lisp-closure (closure)
  (let* ((function-id (foreign-slot-value closure
                                          '(:struct lisp-closure)
                                          :function-id))
         (addr (pointer-address (foreign-slot-value closure
                                                    '(:struct lisp-closure)
                                                    :object)))
         (object (or (gethash addr *foreign-gobjects-strong*)
                     (gethash addr *foreign-gobjects-weak*))))
    (when object
      (delete-handler-from-object object function-id))))

(defun delete-handler-from-object (object handler-id)
  (let ((handlers (g-object-signal-handlers object)))
    (setf (aref handlers handler-id) nil)
    (iter (while (plusp (length handlers)))
          (while (null (aref handlers (1- (length handlers)))))
          (vector-pop handlers))
    nil))

;;; ----------------------------------------------------------------------------
;;; struct GSignalInvocationHint
;;;
;;; struct GSignalInvocationHint {
;;;   guint        signal_id;
;;;   GQuark       detail;
;;;   GSignalFlags run_type;
;;; };
;;;
;;; The GSignalInvocationHint structure is used to pass on additional
;;; information to callbacks during a signal emission.
;;;
;;; guint signal_id;
;;;     The signal id of the signal invoking the callback
;;;
;;; GQuark detail;
;;;     The detail passed on for this emission
;;;
;;; GSignalFlags run_type;
;;;     The stage the signal emission is currently in, this field will contain
;;;     one of G_SIGNAL_RUN_FIRST, G_SIGNAL_RUN_LAST or G_SIGNAL_RUN_CLEANUP.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSignalAccumulator ()
;;;
;;; gboolean (*GSignalAccumulator) (GSignalInvocationHint *ihint,
;;;                                 GValue *return_accu,
;;;                                 const GValue *handler_return,
;;;                                 gpointer data);
;;;
;;; The signal accumulator is a special callback function that can be used to
;;; collect return values of the various callbacks that are called during a
;;; signal emission. The signal accumulator is specified at signal creation
;;; time, if it is left NULL, no accumulation of callback return values is
;;; performed. The return value of signal emissions is then the value returned
;;; by the last callback.
;;;
;;; ihint :
;;;     Signal invocation hint, see GSignalInvocationHint.
;;;
;;; return_accu :
;;;     Accumulator to collect callback return values in, this is the return
;;;     value of the current signal emission.
;;;
;;; handler_return :
;;;     A GValue holding the return value of the signal handler.
;;;
;;; data :
;;;     Callback data that was specified when creating the signal.
;;;
;;; Returns :
;;;     The accumulator function returns whether the signal emission should be
;;;     aborted. Returning FALSE means to abort the current emission and TRUE is
;;;     returned for continuation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSignalCMarshaller
;;;
;;; typedef GClosureMarshal GSignalCMarshaller;
;;;
;;; This is the signature of marshaller functions, required to marshall arrays
;;; of parameter values to signal emissions into C language callback
;;; invocations. It is merely an alias to GClosureMarshal since the GClosure
;;; mechanism takes over responsibility of actual function invocation for the
;;; signal system.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSignalEmissionHook ()
;;;
;;; gboolean (*GSignalEmissionHook) (GSignalInvocationHint *ihint,
;;;                                  guint n_param_values,
;;;                                  const GValue *param_values,
;;;                                  gpointer data);
;;;
;;; A simple function pointer to get invoked when the signal is emitted. This
;;; allows you to tie a hook to the signal type, so that it will trap all
;;; emissions of that signal, from any object.
;;;
;;; You may not attach these to signals created with the G_SIGNAL_NO_HOOKS flag.
;;;
;;; ihint :
;;;     Signal invocation hint, see GSignalInvocationHint.
;;;
;;; n_param_values :
;;;     the number of parameters to the function, including the instance on
;;;     which the signal was emitted.
;;;
;;; param_values :
;;;     the instance on which the signal was emitted, followed by the parameters
;;;     of the emission
;;;
;;; data :
;;;     user data associated with the hook.
;;;
;;; Returns :
;;;     whether it wants to stay connected. If it returns FALSE, the signal hook
;;;     is disconnected (and destroyed).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GSignalFlags
;;; ----------------------------------------------------------------------------

(defbitfield g-signal-flags
  :run-first
  :run-last
  :run-cleanup
  :no-recurse
  :detailed
  :action
  :no-hooks
  :must-collect
  :deprecated)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-flags atdoc:*symbol-name-alias*) "Bitfield"
      (gethash 'g-signal-flags atdoc:*external-symbols*)
 "@version{2013-6-30}
  @begin{short}
    The signal flags are used to specify a signal's behaviour, the overall
    signal description outlines how especially the RUN flags control the stages
    of a signal emission.
  @end{short}
  @begin{pre}
(defbitfield g-signal-flags
  :run-first
  :run-last
  :run-cleanup
  :no-recurse
  :detailed
  :action
  :no-hooks
  :must-collect
  :deprecated)
  @end{pre}
  @begin[code]{table}
    @entry[:run-first]{Invoke the object method handler in the first emission
      stage.}
    @entry[:run-last]{Invoke the object method handler in the third emission
      stage.}
    @entry[:run-cleanup]{Invoke the object method handler in the last emission
      stage.}
    @entry[:no-recurse]{Signals being emitted for an object while currently
      being in emission for this very object will not be emitted recursively,
      but instead cause the first emission to be restarted.}
    @entry[:detailed]{This signal supports \"::detail\" appendices to the
      signal name upon handler connections and emissions.}
    @entry[:action]{Action signals are signals that may freely be emitted on
      alive objects from user code via the function @fun{g-signal-emit} and
      friends, without the need of being embedded into extra code that performs
      pre or post emission adjustments on the object. They can also be thought
      of as object methods which can be called generically by third-party code.}
    @entry[:no-hooks]{No emissions hooks are supported for this signal.}
    @entry[:must-collect]{Varargs signal emission will always collect the
      arguments, even if there are no signal handlers connected. Since 2.30.}
    @entry[:deprecated]{The signal is deprecated and will be removed in a future
      version. A warning will be generated if it is connected while running with
      @code{G_ENABLE_DIAGNOSTIC = 1}. Since 2.32.}
  @end{table}")

(export 'g-signal-flags)

;;; ----------------------------------------------------------------------------
;;; enum GSignalMatchType
;;; ----------------------------------------------------------------------------

(defbitfield g-signal-match-type
  :id
  :detail
  :closure
  :func
  :data
  :unblocked)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-match-type atdoc:*symbol-name-alias*) "Bitfield"
      (gethash 'g-signal-match-type atdoc:*external-symbols*)
 "@version{2013-7-31}
  @begin{short}
    The match types specify what the functions
    @fun{g-signal-handlers-block-matched},
    @fun{g-signal-handlers-unblock-matched} and
    @fun{g-signal-handlers-disconnect-matched} match signals by.
  @end{short}
  @begin{pre}
(defbitfield g-signal-match-type
  :id
  :detail
  :closure
  :func
  :data
  :unblocked)
  @end{pre}
  @begin[code]{table}
    @entry[:id]{The signal id must be equal.}
    @entry[:detail]{The signal detail be equal.}
    @entry[:closure]{The closure must be the same.}
    @entry[:func]{The C closure callback must be the same.}
    @entry[:data]{The closure data must be the same.}
    @entry[:unblocked]{Only unblocked signals may matched.}
  @end{table}
  @see-function{g-signal-handlers-block-matched}
  @see-function{g-signal-handlers-unblock-matched}
  @see-function{g-signal-handlers-disconnect-matched}")

(export 'g-signal-match-type)

;;; ----------------------------------------------------------------------------
;;; struct GSignalQuery
;;;
;;; struct GSignalQuery {
;;;   guint        signal_id;
;;;   const gchar  *signal_name;
;;;   GType        itype;
;;;   GSignalFlags signal_flags;
;;;   /* mangled with G_SIGNAL_TYPE_STATIC_SCOPE flag */
;;;   GType        return_type;
;;;   guint        n_params;
;;;   /* mangled with G_SIGNAL_TYPE_STATIC_SCOPE flag */
;;;   const GType  *param_types;
;;; };
;;;
;;; A structure holding in-depth information for a specific signal. It is filled
;;; in by the g_signal_query() function.
;;;
;;; guint signal_id;
;;;     The signal id of the signal being queried, or 0 if the signal to be
;;;     queried was unknown.
;;;
;;; const gchar *signal_name;
;;;     The signal name.
;;;
;;; GType itype;
;;;     The interface/instance type that this signal can be emitted for.
;;;
;;; GSignalFlags signal_flags;
;;;     The signal flags as passed in to g_signal_new().
;;;
;;; GType return_type;
;;;     The return type for user callbacks.
;;;
;;; guint n_params;
;;;     The number of parameters that user callbacks take.
;;;
;;; const GType *param_types;
;;;     The individual parameter types for user callbacks, note that the
;;;     effective callback signature is:
;;;
;;;     @return_type callback (gpointer     data1,
;;;                            [param_types param_names,]
;;;                            gpointer     data2);
;;; ----------------------------------------------------------------------------

(defcstruct %g-signal-query
  (:signal-id :uint)
  (:signal-name :string)
  (:owner-type g-type)
  (:signal-flags g-signal-flags)
  (:return-type (g-type :mangled-p t))
  (:n-params :uint)
  (:param-types (:pointer (g-type :mangled-p t))))

;;; ----------------------------------------------------------------------------

(defstruct signal-info
  id
  name
  owner-type
  flags
  return-type
  param-types
  detail)

(export 'signal-info)
(export 'signal-info-id)
(export 'signal-info-name)
(export 'signal-info-owner-type)
(export 'signal-info-flags)
(export 'signal-info-return-type)
(export 'signal-info-param-types)
(export 'signal-info-detail)

(defmethod print-object ((instance signal-info) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
         (format stream
                 "Signal [#~A] ~A ~A.~A~@[::~A~] (~{~A~^, ~})~@[ [~{~A~^, ~}]~]"
                 (signal-info-id instance)
                 (gtype-name (signal-info-return-type instance))
                 (gtype-name (signal-info-owner-type instance))
                 (signal-info-name instance)
                 (signal-info-detail instance)
                 (mapcar #'gtype-name (signal-info-param-types instance))
                 (signal-info-flags instance)))))

;;; ----------------------------------------------------------------------------

;; Utility function to list all signals of a given type.

(defun list-signals (type &key include-inherited)
  (unless (g-type= type +g-type-invalid+)
    (let ((signals (with-foreign-object (n-ids :uint)
                     (with-unwind (ids (%g-signal-list-ids type n-ids) g-free)
                       (iter (for i from 0 below (mem-ref n-ids :uint))
                             (collect
                               (g-signal-query (mem-aref ids :uint i))))))))
      (if include-inherited
          (nconc
            (list-signals (g-type-parent type) :include-inherited t)
            (iter (for interface in (g-type-interfaces type))
                  (nconcing (list-signals interface :include-inherited t)))
                  signals)
          signals))))

;;; ----------------------------------------------------------------------------
;;; G_SIGNAL_TYPE_STATIC_SCOPE
;;;
;;; #define G_SIGNAL_TYPE_STATIC_SCOPE (G_TYPE_FLAG_RESERVED_ID_BIT)
;;;
;;; This macro flags signal argument types for which the signal system may
;;; assume that instances thereof remain persistent across all signal emissions
;;; they are used in. This is only useful for non ref-counted, value-copy types.
;;;
;;; To flag a signal argument in this way, add | G_SIGNAL_TYPE_STATIC_SCOPE to
;;; the corresponding argument of g_signal_new().
;;;
;;; g_signal_new ("size_request",
;;;   G_TYPE_FROM_CLASS (gobject_class),
;;;      G_SIGNAL_RUN_FIRST,
;;;      G_STRUCT_OFFSET (GtkWidgetClass, size_request),
;;;      NULL, NULL,
;;;      _gtk_marshal_VOID__BOXED,
;;;      G_TYPE_NONE, 1,
;;;      GTK_TYPE_REQUISITION | G_SIGNAL_TYPE_STATIC_SCOPE);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_SIGNAL_MATCH_MASK
;;;
;;; #define G_SIGNAL_MATCH_MASK  0x3f
;;;
;;; A mask for all GSignalMatchType bits.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_SIGNAL_FLAGS_MASK
;;;
;;; #define G_SIGNAL_FLAGS_MASK  0x1ff
;;;
;;; A mask for all GSignalFlags bits.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_new ()
;;;
;;; guint g_signal_new (const gchar *signal_name,
;;;                     GType itype,
;;;                     GSignalFlags signal_flags,
;;;                     guint class_offset,
;;;                     GSignalAccumulator accumulator,
;;;                     gpointer accu_data,
;;;                     GSignalCMarshaller c_marshaller,
;;;                     GType return_type,
;;;                     guint n_params,
;;;                     ...);
;;;
;;; Creates a new signal. (This is usually done in the class initializer.)
;;;
;;; A signal name consists of segments consisting of ASCII letters and digits,
;;; separated by either the '-' or '_' character. The first character of a
;;; signal name must be a letter. Names which violate these rules lead to
;;; undefined behaviour of the GSignal system.
;;;
;;; When registering a signal and looking up a signal, either separator can be
;;; used, but they cannot be mixed.
;;;
;;; If 0 is used for class_offset subclasses cannot override the class handler
;;; in their class_init method by doing
;;; super_class->signal_handler = my_signal_handler. Instead they will have to
;;; use g_signal_override_class_handler().
;;;
;;; If c_marshaller is NULL, g_cclosure_marshal_generic() will be used as the
;;; marshaller for this signal.
;;;
;;; signal_name :
;;;     the name for the signal
;;;
;;; itype :
;;;     the type this signal pertains to. It will also pertain to types which
;;;     are derived from this type.
;;;
;;; signal_flags :
;;;     a combination of GSignalFlags specifying detail of when the default
;;;     handler is to be invoked. You should at least specify G_SIGNAL_RUN_FIRST
;;;     or G_SIGNAL_RUN_LAST.
;;;
;;; class_offset :
;;;     The offset of the function pointer in the class structure for this type.
;;;     Used to invoke a class method generically. Pass 0 to not associate a
;;;     class method slot with this signal.
;;;
;;; accumulator :
;;;     the accumulator for this signal; may be NULL.
;;;
;;; accu_data :
;;;     user data for the accumulator.
;;;
;;; c_marshaller :
;;;     the function to translate arrays of parameter values to signal emissions
;;;     into C language callback invocations or NULL
;;;
;;; return_type :
;;;     the type of return value, or G_TYPE_NONE for a signal without a return
;;;     value.
;;;
;;; n_params :
;;;     the number of parameter types to follow.
;;;
;;; ... :
;;;     a list of types, one for each parameter.
;;;
;;; Returns :
;;;     the signal id
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_newv ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_newv" g-signal-newv) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[signal-name]{the name for the signal}
  @argument[itype]{the type this signal pertains to. It will also pertain to
    types which are derived from this type.}
  @argument[signal-flags]{a combination of @symbol{g-signal-flags} specifying
    detail of when the default handler is to be invoked. You should at least
    specify @code{:run-first} or @code{:run-last}}
  @argument[class-closure]{the closure to invoke on signal emission;
    may be @code{nil}}
  @argument[accumulator]{the accumulator for this signal; may be @code{nil}}
  @argument[accu-data]{user data for the accumulator}
  @argument[c-marshaller]{the function to translate arrays of parameter values
    to signal emissions into C language callback invocations or @code{nil}}
  @argument[return-type]{the type of return value, or @var{+g-type-none+} for
    a signal without a return value}
  @argument[n-params]{the length of @arg{param-types}}
  @argument[param-types]{an array of types, one for each parameter}
  @return{The signal ID.}
  @begin{short}
    Creates a new signal. (This is usually done in the class initializer.)
  @end{short}

  See the function @fun{g-signal-new} for details on allowed signal names.

  If @arg{c-marshaller} is @code{nil} the function
  @code{g-cclosure-marshal-generic} will be used as the marshaller for this
  signal.
  @see-function{g-signal-new}"
  (signal-name :string)
  (itype g-type)
  (signal-flags g-signal-flags)
  (class-closure :pointer)
  (accumulator :pointer)
  (accu-data :pointer)
  (marschaller :pointer)
  (return-type g-type)
  (n-params :uint)
  (param-types (:pointer g-type)))

(export 'g-signal-newv)

;;; ----------------------------------------------------------------------------
;;; g_signal_new_valist ()
;;;
;;; guint g_signal_new_valist (const gchar *signal_name,
;;;                            GType itype,
;;;                            GSignalFlags signal_flags,
;;;                            GClosure *class_closure,
;;;                            GSignalAccumulator accumulator,
;;;                            gpointer accu_data,
;;;                            GSignalCMarshaller c_marshaller,
;;;                            GType return_type,
;;;                            guint n_params,
;;;                            va_list args);
;;;
;;; Creates a new signal. (This is usually done in the class initializer.)
;;;
;;; See g_signal_new() for details on allowed signal names.
;;;
;;; If c_marshaller is NULL, g_cclosure_marshal_generic() will be used as the
;;; marshaller for this signal.
;;;
;;; signal_name :
;;;     the name for the signal
;;;
;;; itype :
;;;     the type this signal pertains to. It will also pertain to types which
;;;     are derived from this type.
;;;
;;; signal_flags :
;;;     a combination of GSignalFlags specifying detail of when the default
;;;     handler is to be invoked. You should at least specify G_SIGNAL_RUN_FIRST
;;;     or G_SIGNAL_RUN_LAST.
;;;
;;; class_closure :
;;;     The closure to invoke on signal emission; may be NULL.
;;;
;;; accumulator :
;;;     the accumulator for this signal; may be NULL.
;;;
;;; accu_data :
;;;     user data for the accumulator.
;;;
;;; c_marshaller :
;;;     the function to translate arrays of parameter values to signal emissions
;;;     into C language callback invocations or NULL
;;;
;;; return_type :
;;;     the type of return value, or G_TYPE_NONE for a signal without a return
;;;     value.
;;;
;;; n_params :
;;;     the number of parameter types in args.
;;;
;;; args :
;;;     va_list of GType, one for each parameter.
;;;
;;; Returns :
;;;     the signal id
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_query ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_query" %g-signal-query) :void
  (signal-id :uint)
  (query (:pointer (:struct %g-signal-query))))

(defun g-signal-query (signal-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[signal-id]{the signal ID of the signal to query information for}
  @begin{return}
    @code{query} -- the signal info
  @end{return}
  @begin{short}
    Returns the signal info.
  @end{short}

  Queries the signal system for in-depth information about a specific signal.
  This function will return signal-specific information. If an invalid signal ID
  is passed in, the @arg{signal-id} member is 0."
  (with-foreign-object (query '(:struct %g-signal-query))
    (%g-signal-query signal-id query)
    (assert (not (zerop (foreign-slot-value query
                                            '(:struct %g-signal-query)
                                            :signal-id))))
    (let ((param-types
           (iter (with param-types = (foreign-slot-value query
                                                         '(:struct %g-signal-query)
                                                         :param-types))
                 (for i from 0 below (foreign-slot-value query
                                                         '(:struct %g-signal-query)
                                                         :n-params))
                 (for param-type = (mem-aref param-types
                                             '(g-type :mangled-p t)
                                             i))
                 (collect param-type))))
      (make-signal-info :id signal-id
                        :name (foreign-slot-value query
                                                  '(:struct %g-signal-query)
                                                  :signal-name)
                        :owner-type (foreign-slot-value query
                                                        '(:struct %g-signal-query)
                                                        :owner-type)
                        :flags (foreign-slot-value query
                                                   '(:struct %g-signal-query)
                                                   :signal-flags)
                        :return-type (foreign-slot-value query
                                                         '(:struct %g-signal-query)
                                                         :return-type)
                        :param-types param-types))))

(export 'g-signal-query)

;;; ----------------------------------------------------------------------------
;;; g_signal_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_lookup" g-signal-lookup) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[name]{the signal's name}
  @argument[itype]{the type that the signal operates on}
  @return{The signal's identifying number, or 0 if no signal was found.}
  @begin{short}
    Given the name of the signal and the type of object it connects to, gets the
    signal's identifying integer.
  @end{short}
  Emitting the signal by number is somewhat faster than using the name each
  time.

  Also tries the ancestors of the given type.

  See the function @fun{g-signal-new} for details on allowed signal names.
  @see-function{g-signal-new}"
  (name :string)
  (itype g-type))

(export 'g-signal-lookup)

;;; ----------------------------------------------------------------------------
;;; g_signal_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_name" g-signal-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[signal-id]{the signal's identifying number}
  @return{The signal name, or @code{nil} if the signal number was invalid.}
  @begin{short}
    Given the signal's identifier, finds its name.
  @end{short}

  Two different signals may have the same name, if they have differing types."
  (signal-id :uint))

(export 'g-signal-name)

;;; ----------------------------------------------------------------------------
;;; g_signal_list_ids ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_list_ids" %g-signal-list-ids) (:pointer :uint)
  (itype g-type)
  (n-ids (:pointer :uint)))

;; Returns a list of signal ids for itype

(defun g-signal-list-ids (itype)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[itype]{instance or interface type}
  @return{A list of signal IDs.}
  @begin{short}
    Lists the signals by ID that a certain instance or interface type created.
  @end{short}
  Further information about the signals can be acquired through the function
  @fun{g-signal-query}.
  @see-function{g-signal-query}"
  (when (g-type-is-object itype) ; itype must be of type GObject
    (with-foreign-object (n-ids :uint)
      (with-unwind (ids (%g-signal-list-ids itype n-ids) g-free)
        (iter (for i from 0 below (mem-ref n-ids :uint))
              (collect (mem-aref ids :uint i)))))))

(export 'g-signal-list-ids)

;;; ----------------------------------------------------------------------------
;;; g_signal_emit ()
;;; ----------------------------------------------------------------------------

(defun g-signal-emit (object signal-name &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @argument[instance]{the instance the signal is being emitted on}
  @argument[signal-name]{the signal name}
  @argument[args]{parameters to be passed to the signal}
  @return{The return value of the signal.}
  @short{Emits a signal.}

  Note that the function @sym{g-signal-emit} resets the return value to the
  default if no handlers are connected, in contrast to the function
  @fun{g-signal-emitv}.
  @see-function{g-signal-emitv}"
  (let* ((object-type (g-type-from-instance (pointer object)))
         (signal-info (g-signal-parse-name object-type signal-name)))
    (unless signal-info
      (error "Signal ~A not found on object ~A" signal-name object))
    (let ((params-count (length (signal-info-param-types signal-info))))
      (with-foreign-object (params '(:struct g-value) (1+ params-count)) ;
        (set-g-value (mem-aptr params '(:struct g-value) 0)              ;
                     object
                     object-type
                     :zero-g-value t)
        (iter (for i from 0 below params-count)
              (for arg in args)
              (for type in (signal-info-param-types signal-info))
              (set-g-value (mem-aptr params '(:struct g-value) (1+ i)) ;
                           arg
                           type
                           :zero-g-value t))
        (prog1
          (if (eq (signal-info-return-type signal-info)
                  (gtype +g-type-none+))
              ;; Emit a signal which has no return value
              (g-signal-emitv params
                              (signal-info-id signal-info)
                              signal-name
                              (null-pointer))
              ;; Emit a signal which has a return value
              (with-foreign-object (return-value '(:struct g-value)) ;
                (g-value-zero return-value)
                (g-value-init return-value
                              (signal-info-return-type signal-info))
                (g-signal-emitv params
                                (signal-info-id signal-info)
                                signal-name
                                return-value)
                (prog1
                  ;; Return value of the signal
                  (parse-g-value return-value)
                  (g-value-unset return-value))))
          (iter (for i from 0 below (1+ params-count))
                (g-value-unset (mem-aptr params '(:struct g-value) i))))))))  ;

(export 'g-signal-emit)

;;; ----------------------------------------------------------------------------
;;; g_signal_emit_by_name ()
;;;
;;; void g_signal_emit_by_name (gpointer instance,
;;;                             const gchar *detailed_signal,
;;;                             ...);
;;;
;;; Emits a signal.
;;;
;;; Note that g_signal_emit_by_name() resets the return value to the default if
;;; no handlers are connected, in contrast to g_signal_emitv().
;;;
;;; instance :
;;;     the instance the signal is being emitted on.
;;;
;;; detailed_signal :
;;;     a string of the form "signal-name::detail".
;;;
;;; ... :
;;;     parameters to be passed to the signal, followed by a location for the
;;;     return value. If the return type of the signal is G_TYPE_NONE, the
;;;     return value location can be omitted.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_emitv ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_emitv" g-signal-emitv) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[instance-and-params]{argument list for the signal emission. The
    first element in the array is a @symbol{g-value} for the instance the signal
    is being emitted on. The rest are any arguments to be passed to the signal.}
  @argument[signal-id]{the signal id}
  @argument[detail]{the detail}
  @argument[return-value]{Location to store the return value of the signal
    emission.}
  @begin{short}
    Emits a signal.
  @end{short}

  Note that the function @sym{g-signal-emitv} does not change @arg{return-value}
  if no handlers are connected, in contrast to the functions @fun{g-signal-emit}
  and @fun{g-signal-emit-valist}.
  @see-symbol{g-value}
  @see-function{g-signal-emit}
  @see-function{g-signal-emit-valist}"
  (instance-and-params (:pointer (:struct g-value)))
  (signal-id :uint)
  (detail g-quark)
  (return-value (:pointer (:struct g-value))))

(export 'g-signal-emitv)

;;; ----------------------------------------------------------------------------
;;; g_signal_emit_valist ()
;;;
;;; void g_signal_emit_valist (gpointer instance,
;;;                            guint signal_id,
;;;                            GQuark detail,
;;;                            va_list var_args);
;;;
;;; Emits a signal.
;;;
;;; Note that g_signal_emit_valist() resets the return value to the default if
;;; no handlers are connected, in contrast to g_signal_emitv().
;;;
;;; instance :
;;;     the instance the signal is being emitted on.
;;;
;;; signal_id :
;;;     the signal id
;;;
;;; detail :
;;;     the detail
;;;
;;; var_args :
;;;     a list of parameters to be passed to the signal, followed by a location
;;;     for the return value. If the return type of the signal is G_TYPE_NONE,
;;;     the return value location can be omitted.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect()
;;; ----------------------------------------------------------------------------

(defun g-signal-connect (instance detailed-signal handler &key after)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[instance]{the instance to connect to}
  @argument[detailed-signal]{a string of the form \"signal-name::detail\"}
  @argument[handler]{the callback function to connect}
  @argument[key]{if @em{true} the @arg{handler} is called after the default
    handler}
  @return{The handler ID.}
  @begin{short}
    Connects a callback function to a signal for a particular object.
  @end{short}

  The handler will be called before the default handler of the signal. If the
  keyword argument @arg{after} is @em{true}, the @arg{handler} will be called
  after the default handler of the signal.
  @see-function{g-signal-connect-after}"
  (g-signal-connect-closure (pointer instance)
                            detailed-signal
                            (create-closure instance handler)
                            after))

(export 'g-signal-connect)

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_after()
;;; ----------------------------------------------------------------------------

(declaim (inline g-signal-connect-after))

(defun g-signal-connect-after (instance detailed-signal handler)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[instance]{the instance to connect to.}
  @argument[detailed-signal]{a string of the form \"signal-name::detail\"}
  @argument[handler]{the callback function to connect}
  @return{The handler ID.}
  @begin{short}
    Connects a callback function to a signal for a particular object.
  @end{short}

  The @arg{handler} will be called after the default handler of the signal.

  @subheading{Lisp implementation}
    In the Lisp implementation the function @fun{g-signal-connect} has a
    keyword argument @arg{after}.
  @see-function{g-signal-connect}"
  (g-signal-connect instance detailed-signal handler :after t))

(export 'g-signal-connect-after)

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_swapped()
;;;
;;; #define g_signal_connect_swapped(instance, detailed_signal, c_handler, data)
;;;
;;; Connects a GCallback function to a signal for a particular object.
;;;
;;; The instance on which the signal is emitted and data will be swapped when
;;; calling the handler.
;;;
;;; instance :
;;;     the instance to connect to.
;;;
;;; detailed_signal :
;;;     a string of the form "signal-name::detail".
;;;
;;; c_handler :
;;;     the GCallback to connect.
;;;
;;; data :
;;;     data to pass to c_handler calls.
;;;
;;; Returns :
;;;     the handler id
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_object ()
;;;
;;; gulong g_signal_connect_object (gpointer instance,
;;;                                 const gchar *detailed_signal,
;;;                                 GCallback c_handler,
;;;                                 gpointer gobject,
;;;                                 GConnectFlags connect_flags);
;;;
;;; This is similar to g_signal_connect_data(), but uses a closure which ensures
;;; that the gobject stays alive during the call to c_handler by temporarily
;;; adding a reference count to gobject.
;;;
;;; Note that there is a bug in GObject that makes this function much less
;;; useful than it might seem otherwise. Once gobject is disposed, the callback
;;; will no longer be called, but, the signal handler is not currently
;;; disconnected. If the instance is itself being freed at the same time than
;;; this doesn't matter, since the signal will automatically be removed, but if
;;; instance persists, then the signal handler will leak. You should not remove
;;; the signal yourself because in a future versions of GObject, the handler
;;; will automatically be disconnected.
;;;
;;; It's possible to work around this problem in a way that will continue to
;;; work with future versions of GObject by checking that the signal handler is
;;; still connected before disconnected it:
;;;
;;;   if (g_signal_handler_is_connected (instance, id))
;;;     g_signal_handler_disconnect (instance, id);
;;;
;;; instance :
;;;     the instance to connect to.
;;;
;;; detailed_signal :
;;;     a string of the form "signal-name::detail".
;;;
;;; c_handler :
;;;     the GCallback to connect.
;;;
;;; gobject :
;;;     the object to pass as data to c_handler.
;;;
;;; connect_flags :
;;;     a combination of GConnectFlags.
;;;
;;; Returns :
;;;     the handler id.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GConnectFlags
;;;
;;; typedef enum {
;;;   G_CONNECT_AFTER   = 1 << 0,
;;;   G_CONNECT_SWAPPED = 1 << 1
;;; } GConnectFlags;
;;;
;;; The connection flags are used to specify the behaviour of a signal's
;;; connection.
;;;
;;; G_CONNECT_AFTER
;;;     whether the handler should be called before or after the default handler
;;;     of the signal.
;;;
;;; G_CONNECT_SWAPPED
;;;     whether the instance and data should be swapped when calling the
;;;     handler.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_data ()
;;;
;;; gulong g_signal_connect_data (gpointer instance,
;;;                               const gchar *detailed_signal,
;;;                               GCallback c_handler,
;;;                               gpointer data,
;;;                               GClosureNotify destroy_data,
;;;                               GConnectFlags connect_flags);
;;;
;;; Connects a GCallback function to a signal for a particular object. Similar
;;; to g_signal_connect(), but allows to provide a GClosureNotify for the data
;;; which will be called when the signal handler is disconnected and no longer
;;; used. Specify connect_flags if you need ..._after() or ..._swapped()
;;; variants of this function.
;;;
;;; instance :
;;;     the instance to connect to.
;;;
;;; detailed_signal :
;;;     a string of the form "signal-name::detail".
;;;
;;; c_handler :
;;;     the GCallback to connect.
;;;
;;; data :
;;;     data to pass to c_handler calls.
;;;
;;; destroy_data :
;;;     a GClosureNotify for data.
;;;
;;; connect_flags :
;;;     a combination of GConnectFlags.
;;;
;;; Returns :
;;;     the handler id
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_closure ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_connect_closure" g-signal-connect-closure) :ulong
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[instance]{the instance to connect to}
  @argument[detailed-signal]{a string of the form \"signal-name::detail\"}
  @argument[closure]{the closure to connect}
  @argument[after]{whether the handler should be called before or after the
    default handler of the signal}
  @return{The handler ID.}
  Connects a closure to a signal for a particular object."
  (instance :pointer)
  (detailed-signal :string)
  (closure (:pointer (:struct g-closure)))
  (after :boolean))

(export 'g-signal-connect-closure)

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_closure_by_id ()
;;;
;;; gulong g_signal_connect_closure_by_id (gpointer instance,
;;;                                        guint signal_id,
;;;                                        GQuark detail,
;;;                                        GClosure *closure,
;;;                                        gboolean after);
;;;
;;; Connects a closure to a signal for a particular object.
;;;
;;; instance :
;;;     the instance to connect to.
;;;
;;; signal_id :
;;;     the id of the signal.
;;;
;;; detail :
;;;     the detail.
;;;
;;; closure :
;;;     the closure to connect.
;;;
;;; after :
;;;     whether the handler should be called before or after the default handler
;;;     of the signal.
;;;
;;; Returns :
;;;     the handler id
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_block ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_block" g-signal-handler-block) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[instance]{the instance to block the signal handler of}
  @argument[handler-id]{handler ID of the handler to be blocked}
  @begin{short}
    Blocks a handler of an instance so it will not be called during any signal
    emissions unless it is unblocked again.
  @end{short}
  Thus \"blocking\" a signal handler means to temporarily deactive it, a signal
  handler has to be unblocked exactly the same amount of times it has been
  blocked before to become active again.

  The @arg{handler-id} has to be a valid signal handler ID, connected to a
  signal of instance."
  (instance g-object)
  (handler-id :ulong))

(export 'g-signal-handler-block)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_unblock ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_unblock" g-signal-handler-unblock) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[instance]{the instance to unblock the signal handler of}
  @argument[handler-id]{handler ID of the handler to be unblocked}
  @begin{short}
    Undoes the effect of a previous @fun{g-signal-handler-block} call.
  @end{short}
  A blocked handler is skipped during signal emissions and will not be invoked,
  unblocking it (for exactly the amount of times it has been blocked before)
  reverts its \"blocked\" state, so the handler will be recognized by the signal
  system and is called upon future or currently ongoing signal emissions
  (since the order in which handlers are called during signal emissions is
  deterministic, whether the unblocked handler in question is called as part
  of a currently ongoing emission depends on how far that emission has
  proceeded yet).

  The @arg{handler-id} has to be a valid id of a signal handler that is
  connected to a signal of instance and is currently blocked.
  @see-function{g-signal-handler-block}"
  (instance g-object)
  (handler-id :ulong))

(export 'g-signal-handler-unblock)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_disconnect ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_disconnect" g-signal-handler-disconnect) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[instance]{the instance to remove the signal handler from}
  @argument[handler-id]{handler ID of the handler to be disconnected}
  @begin{short}
    Disconnects a handler from an instance so it will not be called during any
    future or currently ongoing emissions of the signal it has been connected
    to.
  @end{short}
  The @arg{handler-id} becomes invalid and may be reused.

  The @arg{handler-id} has to be a valid signal handler id, connected to a
  signal of instance."
  (object g-object)
  (handler-id :ulong))

(export 'g-signal-handler-disconnect)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_find ()
;;;
;;; gulong g_signal_handler_find (gpointer instance,
;;;                               GSignalMatchType mask,
;;;                               guint signal_id,
;;;                               GQuark detail,
;;;                               GClosure *closure,
;;;                               gpointer func,
;;;                               gpointer data);
;;;
;;; Finds the first signal handler that matches certain selection criteria. The
;;; criteria mask is passed as an OR-ed combination of GSignalMatchType flags,
;;; and the criteria values are passed as arguments. The match mask has to be
;;; non-0 for successful matches. If no handler was found, 0 is returned.
;;;
;;; instance :
;;;     The instance owning the signal handler to be found.
;;;
;;; mask :
;;;     Mask indicating which of signal_id, detail, closure, func and/or data
;;;     the handler has to match.
;;;
;;; signal_id :
;;;     Signal the handler has to be connected to.
;;;
;;; detail :
;;;     Signal detail the handler has to be connected to.
;;;
;;; closure :
;;;     The closure the handler will invoke.
;;;
;;; func :
;;;     The C closure callback of the handler (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handler's closure.
;;;
;;; Returns :
;;;     A valid non-0 signal handler id for a successful match.
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_find" %g-signal-handler-find) :ulong
  (instance g-object)
  (mask g-signal-match-type)
  (signal-id :uint)
  (detail g-quark)
  (closure (:pointer (:struct g-closure)))
  (func :pointer)
  (data :pointer))

(defun g-signal-handler-find (instance signal-id)
  (%g-signal-handler-find instance
                          :id
                          signal-id
                          (null-pointer)
                          (null-pointer)
                          (null-pointer)
                          (null-pointer)))

(export 'g-signal-handler-find)

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_block_matched ()
;;;
;;; guint g_signal_handlers_block_matched (gpointer instance,
;;;                                        GSignalMatchType mask,
;;;                                        guint signal_id,
;;;                                        GQuark detail,
;;;                                        GClosure *closure,
;;;                                        gpointer func,
;;;                                        gpointer data);
;;;
;;; Blocks all handlers on an instance that match a certain selection criteria.
;;; The criteria mask is passed as an OR-ed combination of GSignalMatchType
;;; flags, and the criteria values are passed as arguments. Passing at least one
;;; of the G_SIGNAL_MATCH_CLOSURE, G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA
;;; match flags is required for successful matches. If no handlers were found, 0
;;; is returned, the number of blocked handlers otherwise.
;;;
;;; instance :
;;;     The instance to block handlers from.
;;;
;;; mask :
;;;     Mask indicating which of signal_id, detail, closure, func and/or data
;;;     the handlers have to match.
;;;
;;; signal_id :
;;;     Signal the handlers have to be connected to.
;;;
;;; detail :
;;;     Signal detail the handlers have to be connected to.
;;;
;;; closure :
;;;     The closure the handlers will invoke.
;;;
;;; func :
;;;     The C closure callback of the handlers (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handlers' closures.
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_unblock_matched ()
;;;
;;; guint g_signal_handlers_unblock_matched (gpointer instance,
;;;                                          GSignalMatchType mask,
;;;                                          guint signal_id,
;;;                                          GQuark detail,
;;;                                          GClosure *closure,
;;;                                          gpointer func,
;;;                                          gpointer data);
;;;
;;; Unblocks all handlers on an instance that match a certain selection
;;; criteria. The criteria mask is passed as an OR-ed combination of
;;; GSignalMatchType flags, and the criteria values are passed as arguments.
;;; Passing at least one of the G_SIGNAL_MATCH_CLOSURE, G_SIGNAL_MATCH_FUNC or
;;; G_SIGNAL_MATCH_DATA match flags is required for successful matches. If no
;;; handlers were found, 0 is returned, the number of unblocked handlers
;;; otherwise. The match criteria should not apply to any handlers that are not
;;; currently blocked.
;;;
;;; instance :
;;;     The instance to unblock handlers from.
;;;
;;; mask :
;;;     Mask indicating which of signal_id, detail, closure, func and/or data
;;;     the handlers have to match.
;;;
;;; signal_id :
;;;     Signal the handlers have to be connected to.
;;;
;;; detail :
;;;     Signal detail the handlers have to be connected to.
;;;
;;; closure :
;;;     The closure the handlers will invoke.
;;;
;;; func :
;;;     The C closure callback of the handlers (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handlers' closures.
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_disconnect_matched ()
;;;
;;; guint g_signal_handlers_disconnect_matched (gpointer instance,
;;;                                             GSignalMatchType mask,
;;;                                             guint signal_id,
;;;                                             GQuark detail,
;;;                                             GClosure *closure,
;;;                                             gpointer func,
;;;                                             gpointer data);
;;;
;;; Disconnects all handlers on an instance that match a certain selection
;;; criteria. The criteria mask is passed as an OR-ed combination of
;;; GSignalMatchType flags, and the criteria values are passed as arguments.
;;; Passing at least one of the G_SIGNAL_MATCH_CLOSURE, G_SIGNAL_MATCH_FUNC or
;;; G_SIGNAL_MATCH_DATA match flags is required for successful matches. If no
;;; handlers were found, 0 is returned, the number of disconnected handlers
;;; otherwise.
;;;
;;; instance :
;;;     The instance to remove handlers from.
;;;
;;; mask :
;;;     Mask indicating which of signal_id, detail, closure, func and/or data
;;;     the handlers have to match.
;;;
;;; signal_id :
;;;     Signal the handlers have to be connected to.
;;;
;;; detail :
;;;     Signal detail the handlers have to be connected to.
;;;
;;; closure :
;;;     The closure the handlers will invoke.
;;;
;;; func :
;;;     The C closure callback of the handlers (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handlers' closures.
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_is_connected ()
;;;
;;; gboolean g_signal_handler_is_connected (gpointer instance,
;;;                                         gulong handler_id);
;;;
;;; Returns whether handler_id is the id of a handler connected to instance.
;;;
;;; instance :
;;;     The instance where a signal handler is sought.
;;;
;;; handler_id :
;;;     the handler id.
;;;
;;; Returns :
;;;     whether handler_id identifies a handler connected to instance.
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_is_connected" g-signal-handler-is-connected)
    :boolean
  (instance g-object)
  (handler-id :ulong))

(export 'g-signal-handler-is-connected)

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_block_by_func()
;;;
;;; #define g_signal_handlers_block_by_func(instance, func, data)
;;;
;;; Blocks all handlers on an instance that match func and data.
;;;
;;; instance :
;;;     The instance to block handlers from.
;;;
;;; func :
;;;     The C closure callback of the handlers (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handlers' closures.
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_unblock_by_func()
;;;
;;; #define g_signal_handlers_unblock_by_func(instance, func, data)
;;;
;;; Unblocks all handlers on an instance that match func and data.
;;;
;;; instance :
;;;     The instance to unblock handlers from.
;;;
;;; func :
;;;     The C closure callback of the handlers (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handlers' closures.
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_disconnect_by_func()
;;;
;;; #define g_signal_handlers_disconnect_by_func(instance, func, data)
;;;
;;; Disconnects all handlers on an instance that match func and data.
;;;
;;; instance :
;;;     The instance to remove handlers from.
;;;
;;; func :
;;;     The C closure callback of the handlers (useless for non-C closures).
;;;
;;; data :
;;;     The closure data of the handlers' closures.
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_disconnect_by_data()
;;;
;;; #define g_signal_handlers_disconnect_by_data(instance, data)
;;;
;;; Disconnects all handlers on an instance that match data.
;;;
;;; instance :
;;;     The instance to remove handlers from
;;;
;;; data :
;;;     the closure data of the handlers' closures
;;;
;;; Returns :
;;;     The number of handlers that matched.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_has_handler_pending ()
;;;
;;; gboolean g_signal_has_handler_pending (gpointer instance,
;;;                                        guint signal_id,
;;;                                        GQuark detail,
;;;                                        gboolean may_be_blocked);
;;;
;;; Returns whether there are any handlers connected to instance for the given
;;; signal id and detail.
;;;
;;; One example of when you might use this is when the arguments to the signal
;;; are difficult to compute. A class implementor may opt to not emit the signal
;;; if no one is attached anyway, thus saving the cost of building the
;;; arguments.
;;;
;;; instance :
;;;     the object whose signal handlers are sought.
;;;
;;; signal_id :
;;;     the signal id.
;;;
;;; detail :
;;;     the detail.
;;;
;;; may_be_blocked :
;;;     whether blocked handlers should count as match.
;;;
;;; Returns :
;;;     TRUE if a handler is connected to the signal, FALSE otherwise.
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_has_handler_pending" g-signal-has-handler-pending) :boolean
  (instance g-object)
  (signal-id :uint)
  (detail g-quark)
  (may-be-blocked :boolean))

(export 'g-signal-has-handler-pending)

;;; ----------------------------------------------------------------------------
;;; g_signal_stop_emission ()
;;;
;;; void g_signal_stop_emission (gpointer instance,
;;;                              guint signal_id,
;;;                              GQuark detail);
;;;
;;; Stops a signal's current emission.
;;;
;;; This will prevent the default method from running, if the signal was
;;; G_SIGNAL_RUN_LAST and you connected normally (i.e. without the "after"
;;; flag).
;;;
;;; Prints a warning if used on a signal which isn't being emitted.
;;;
;;; instance :
;;;     the object whose signal handlers you wish to stop.
;;;
;;; signal_id :
;;;     the signal identifier, as returned by g_signal_lookup().
;;;
;;; detail :
;;;     the detail which the signal was emitted with.
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_stop_emission" g-signal-stop-emission) :void
  (instance :pointer)
  (signal-id :uint)
  (detail g-quark))

(export 'g-signal-stop-emission)

;;; ----------------------------------------------------------------------------
;;; g_signal_stop_emission_by_name ()
;;;
;;; void g_signal_stop_emission_by_name (gpointer instance,
;;;                                      const gchar *detailed_signal);
;;;
;;; Stops a signal's current emission.
;;;
;;; This is just like g_signal_stop_emission() except it will look up the signal
;;; id for you.
;;;
;;; instance :
;;;     the object whose signal handlers you wish to stop.
;;;
;;; detailed_signal :
;;;     a string of the form "signal-name::detail".
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_override_class_closure ()
;;;
;;; void g_signal_override_class_closure (guint signal_id,
;;;                                       GType instance_type,
;;;                                       GClosure *class_closure);
;;;
;;; Overrides the class closure (i.e. the default handler) for the given signal
;;; for emissions on instances of instance_type. instance_type must be derived
;;; from the type to which the signal belongs.
;;;
;;; See g_signal_chain_from_overridden() and
;;; g_signal_chain_from_overridden_handler() for how to chain up to the parent
;;; class closure from inside the overridden one.
;;;
;;; signal_id :
;;;     the signal id
;;;
;;; instance_type :
;;;     the instance type on which to override the class closure for the signal.
;;;
;;; class_closure :
;;;     the closure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_chain_from_overridden ()
;;;
;;; void g_signal_chain_from_overridden (const GValue *instance_and_params,
;;;                                      GValue *return_value);
;;;
;;; Calls the original class closure of a signal. This function should only be
;;; called from an overridden class closure; see
;;; g_signal_override_class_closure() and g_signal_override_class_handler().
;;;
;;; instance_and_params :
;;;     (array) the argument list of the signal emission. The first element in
;;;     the array is a GValue for the instance the signal is being emitted on.
;;;     The rest are any arguments to be passed to the signal.
;;;
;;; return_value :
;;;     Location for the return value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_new_class_handler ()
;;;
;;; guint g_signal_new_class_handler (const gchar *signal_name,
;;;                                   GType itype,
;;;                                   GSignalFlags signal_flags,
;;;                                   GCallback class_handler,
;;;                                   GSignalAccumulator accumulator,
;;;                                   gpointer accu_data,
;;;                                   GSignalCMarshaller c_marshaller,
;;;                                   GType return_type,
;;;                                   guint n_params,
;;;                                   ...);
;;;
;;; Creates a new signal. (This is usually done in the class initializer.)
;;;
;;; This is a variant of g_signal_new() that takes a C callback instead off a
;;; class offset for the signal's class handler. This function doesn't need a
;;; function pointer exposed in the class structure of an object definition,
;;; instead the function pointer is passed directly and can be overriden by
;;; derived classes with g_signal_override_class_closure() or
;;; g_signal_override_class_handler()and chained to with
;;; g_signal_chain_from_overridden() or
;;; g_signal_chain_from_overridden_handler().
;;;
;;; See g_signal_new() for information about signal names.
;;;
;;; If c_marshaller is NULL g_cclosure_marshal_generic will be used as the
;;; marshaller for this signal.
;;;
;;; signal_name :
;;;     the name for the signal
;;;
;;; itype :
;;;     the type this signal pertains to. It will also pertain to types which
;;;     are derived from this type.
;;;
;;; signal_flags :
;;;     a combination of GSignalFlags specifying detail of when the default
;;;     handler is to be invoked. You should at least specify G_SIGNAL_RUN_FIRST
;;;     or G_SIGNAL_RUN_LAST.
;;;
;;; class_handler :
;;;     a GCallback which acts as class implementation of this signal. Used to
;;;     invoke a class method generically. Pass NULL to not associate a class
;;;     method with this signal.
;;;
;;; accumulator :
;;;     the accumulator for this signal; may be NULL.
;;;
;;; accu_data :
;;;     user data for the accumulator.
;;;
;;; c_marshaller :
;;;     the function to translate arrays of parameter values to signal emissions
;;;     into C language callback invocations or NULL.
;;;
;;; return_type :
;;;     the type of return value, or G_TYPE_NONE for a signal without a return
;;;     value.
;;;
;;; n_params :
;;;     the number of parameter types to follow.
;;;
;;; ... :
;;;     a list of types, one for each parameter.
;;;
;;; Returns :
;;;     the signal id
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_override_class_handler ()
;;;
;;; void g_signal_override_class_handler (const gchar *signal_name,
;;;                                       GType instance_type,
;;;                                       GCallback class_handler);
;;;
;;; Overrides the class closure (i.e. the default handler) for the given signal
;;; for emissions on instances of instance_type with callabck class_handler.
;;; instance_type must be derived from the type to which the signal belongs.
;;;
;;; See g_signal_chain_from_overridden() and
;;; g_signal_chain_from_overridden_handler() for how to chain up to the parent
;;; class closure from inside the overridden one.
;;;
;;; signal_name :
;;;     the name for the signal
;;;
;;; instance_type :
;;;     the instance type on which to override the class handler for the signal.
;;;
;;; class_handler :
;;;     the handler.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_chain_from_overridden_handler ()
;;;
;;; void g_signal_chain_from_overridden_handler (gpointer instance, ...);
;;;
;;; Calls the original class closure of a signal. This function should only be
;;; called from an overridden class closure; see
;;; g_signal_override_class_closure() and g_signal_override_class_handler().
;;;
;;; instance :
;;;     the instance the signal is being emitted on.
;;;
;;; ... :
;;;     parameters to be passed to the parent class closure, followed by a
;;;     location for the return value. If the return type of the signal is
;;;     G_TYPE_NONE, the return value location can be omitted.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_add_emission_hook ()
;;;
;;; gulong g_signal_add_emission_hook (guint signal_id,
;;;                                    GQuark detail,
;;;                                    GSignalEmissionHook hook_func,
;;;                                    gpointer hook_data,
;;;                                    GDestroyNotify data_destroy);
;;;
;;; Adds an emission hook for a signal, which will get called for any emission
;;; of that signal, independent of the instance. This is possible only for
;;; signals which don't have G_SIGNAL_NO_HOOKS flag set.
;;;
;;; signal_id :
;;;     the signal identifier, as returned by g_signal_lookup().
;;;
;;; detail :
;;;     the detail on which to call the hook.
;;;
;;; hook_func :
;;;     a GSignalEmissionHook function.
;;;
;;; hook_data :
;;;     user data for hook_func.
;;;
;;; data_destroy :
;;;     a GDestroyNotify for hook_data.
;;;
;;; Returns :
;;;     the hook id, for later use with g_signal_remove_emission_hook().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_remove_emission_hook ()
;;;
;;; void g_signal_remove_emission_hook (guint signal_id, gulong hook_id);
;;;
;;; Deletes an emission hook.
;;;
;;; signal_id :
;;;     the id of the signal
;;;
;;; hook_id :
;;;     the id of the emission hook, as returned by g_signal_add_emission_hook()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_parse_name ()
;;;
;;; gboolean g_signal_parse_name (const gchar *detailed_signal,
;;;                               GType itype,
;;;                               guint *signal_id_p,
;;;                               GQuark *detail_p,
;;;                               gboolean force_detail_quark);
;;;
;;; Internal function to parse a signal name into its signal_id and detail
;;; quark.
;;;
;;; detailed_signal :
;;;     a string of the form "signal-name::detail".
;;;
;;; itype :
;;;     The interface/instance type that introduced "signal-name".
;;;
;;; signal_id_p :
;;;     Location to store the signal id.
;;;
;;; detail_p :
;;;     Location to store the detail quark.
;;;
;;; force_detail_quark :
;;;     TRUE forces creation of a GQuark for the detail.
;;;
;;; Returns :
;;;     Whether the signal name could successfully be parsed and signal_id_p and
;;;     detail_p contain valid return values.
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_parse_name" %g-signal-parse-name) :boolean
 (detailed-signal :string)
 (itype g-type)
 (signal-id-p (:pointer :uint))
 (detail-p (:pointer g-quark))
 (force-detail-quark :boolean))

;; TODO: The Lisp function does not work as documented
;;       The return value is a complete signal-info structure.
;;       The function is used in g-signal-emit.

(defun g-signal-parse-name (owner-type signal-name)
  (with-foreign-objects ((signal-id :uint) (detail 'g-quark))
    (when (%g-signal-parse-name signal-name owner-type signal-id detail t)
      (let ((signal-info (g-signal-query (mem-ref signal-id :uint))))
        (setf (signal-info-detail signal-info) (mem-ref detail 'g-quark))
        signal-info))))

(export 'g-signal-parse-name)

;;; ----------------------------------------------------------------------------
;;; g_signal_get_invocation_hint ()
;;;
;;; GSignalInvocationHint * g_signal_get_invocation_hint (gpointer instance);
;;;
;;; Returns the invocation hint of the innermost signal emission of instance.
;;;
;;; instance :
;;;     the instance to query
;;;
;;; Returns :
;;;     the invocation hint of the innermost signal emission
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_type_cclosure_new ()
;;;
;;; GClosure * g_signal_type_cclosure_new (GType itype, guint struct_offset);
;;;
;;; Creates a new closure which invokes the function found at the offset
;;; struct_offset in the class structure of the interface or classed type
;;; identified by itype.
;;;
;;; itype :
;;;     the GType identifier of an interface or classed type
;;;
;;; struct_offset :
;;;     the offset of the member function of itype's class structure which is to
;;;     be invoked by the new closure
;;;
;;; Returns :
;;;     a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_accumulator_first_wins ()
;;;
;;; gboolean g_signal_accumulator_first_wins (GSignalInvocationHint *ihint,
;;;                                           GValue *return_accu,
;;;                                           const GValue *handler_return,
;;;                                           gpointer dummy);
;;;
;;; A predefined GSignalAccumulator for signals intended to be used as a hook
;;; for application code to provide a particular value. Usually only one such
;;; value is desired and multiple handlers for the same signal don't make much
;;; sense (except for the case of the default handler defined in the class
;;; structure, in which case you will usually want the signal connection to
;;; override the class handler).
;;;
;;; This accumulator will use the return value from the first signal handler
;;; that is run as the return value for the signal and not run any further
;;; handlers (ie: the first handler "wins").
;;;
;;; ihint :
;;;     standard GSignalAccumulator parameter
;;;
;;; return_accu :
;;;     standard GSignalAccumulator parameter
;;;
;;; handler_return :
;;;     standard GSignalAccumulator parameter
;;;
;;; dummy :
;;;     standard GSignalAccumulator parameter
;;;
;;; Returns :
;;;     standard GSignalAccumulator result
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_accumulator_true_handled ()
;;;
;;; gboolean g_signal_accumulator_true_handled (GSignalInvocationHint *ihint,
;;;                                             GValue *return_accu,
;;;                                             const GValue *handler_return,
;;;                                             gpointer dummy);
;;;
;;; A predefined GSignalAccumulator for signals that return a boolean values.
;;; The behavior that this accumulator gives is that a return of TRUE stops the
;;; signal emission: no further callbacks will be invoked, while a return of
;;; FALSE allows the emission to continue. The idea here is that a TRUE return
;;; indicates that the callback handled the signal, and no further handling is
;;; needed.
;;;
;;; ihint :
;;;     standard GSignalAccumulator parameter
;;;
;;; return_accu :
;;;     standard GSignalAccumulator parameter
;;;
;;; handler_return :
;;;     standard GSignalAccumulator parameter
;;;
;;; dummy :
;;;     standard GSignalAccumulator parameter
;;;
;;; Returns :
;;;     standard GSignalAccumulator result
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.signals.lisp ---------------------------------------
