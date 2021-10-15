;;; ----------------------------------------------------------------------------
;;; gobject.signals.lisp
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
;;; Signals
;;;
;;;     A means for customization of object behaviour and a general purpose
;;;     notification mechanism.
;;;
;;; Types and Values
;;;
;;;     GSignalInvocationHint
;;;     GSignalCMarshaller
;;;     GSignalCVaMarshaller
;;;     GSignalFlags                                       exported
;;;     GSignalMatchType                                   internal
;;;     GSignalQuery
;;;     GConnectFlags
;;;
;;;     G_SIGNAL_TYPE_STATIC_SCOPE
;;;     G_SIGNAL_MATCH_MASK
;;;     G_SIGNAL_FLAGS_MASK
;;;
;;; Functions
;;;
;;;     (*GSignalAccumulator)
;;;     (*GSignalEmissionHook)
;;;
;;;     g_signal_new
;;;     g_signal_newv
;;;     g_signal_new_valist
;;;     g_signal_set_va_marshaller
;;;     g_signal_query                                     exported
;;;     g_signal_lookup                                    exported
;;;     g_signal_name                                      exported
;;;     g_signal_list_ids                                  exported
;;;     g_signal_emit                                      exported
;;;     g_signal_emit_by_name
;;;     g_signal_emitv                                     internal
;;;     g_signal_emit_valist
;;;     g_signal_connect                                   exported
;;;     g_signal_connect_after                             exported
;;;     g_signal_connect_swapped
;;;     g_signal_connect_object
;;;     g_signal_connect_data
;;;     g_signal_connect_closure                           internal
;;;     g_signal_connect_closure_by_id
;;;     g_signal_handler_block                             exported
;;;     g_signal_handler_unblock                           exported
;;;     g_signal_handler_disconnect                        exported
;;;     g_signal_handler_find                              exported
;;;     g_signal_handlers_block_matched
;;;     g_signal_handlers_unblock_matched
;;;     g_signal_handlers_disconnect_matched
;;;     g_signal_handler_is_connected                      exported
;;;     g_signal_handlers_block_by_func
;;;     g_signal_handlers_unblock_by_func
;;;     g_signal_handlers_disconnect_by_func
;;;     g_signal_handlers_disconnect_by_data
;;;     g_signal_has_handler_pending                       exported
;;;     g_signal_stop_emission                             exported
;;;     g_signal_stop_emission_by_name                     exported
;;;     g_signal_override_class_closure
;;;     g_signal_chain_from_overridden
;;;     g_signal_new_class_handler
;;;     g_signal_override_class_handler
;;;     g_signal_chain_from_overridden_handler
;;;     g_signal_add_emission_hook
;;;     g_signal_remove_emission_hook
;;;     g_signal_is_valid_name
;;;     g_signal_parse_name                                internal
;;;     g_signal_get_invocation_hint
;;;     g_signal_type_cclosure_new
;;;     g_signal_accumulator_first_wins
;;;     g_signal_accumulator_true_handled
;;;     g_clear_signal_handler
;;;
;;; Description
;;;
;;; The basic concept of the signal system is that of the emission of a signal.
;;; Signals are introduced per-type and are identified through strings. Signals
;;; introduced for a parent type are available in derived types as well, so
;;; basically they are a per-type facility that is inherited.
;;;
;;; A signal emission mainly involves invocation of a certain set of callbacks
;;; in precisely defined manner. There are two main categories of such
;;; callbacks, per-object ones and user provided ones. (Although signals can
;;; deal with any kind of instantiatable type, I'm referring to those types as
;;; "object types" in the following, simply because that is the context most
;;; users will encounter signals in.) The per-object callbacks are most often
;;; referred to as "object method handler" or "default (signal) handler", while
;;; user provided callbacks are usually just called "signal handler".
;;;
;;; The object method handler is provided at signal creation time (this most
;;; frequently happens at the end of an object class' creation), while user
;;; provided handlers are frequently connected and disconnected to/from a
;;; certain signal on certain object instances.
;;;
;;; A signal emission consists of five stages, unless prematurely stopped:
;;;
;;; 1. Invocation of the object method handler for G_SIGNAL_RUN_FIRST signals
;;; 2. Invocation of normal user-provided signal handlers (where the after flag
;;;    is not set)
;;; 3. Invocation of the object method handler for G_SIGNAL_RUN_LAST signals
;;; 4. Invocation of user provided signal handlers (where the after flag is set)
;;; 5. Invocation of the object method handler for G_SIGNAL_RUN_CLEANUP signals
;;;
;;; The user-provided signal handlers are called in the order they were
;;; connected in.
;;;
;;; All handlers may prematurely stop a signal emission, and any number of
;;; handlers may be connected, disconnected, blocked or unblocked during a
;;; signal emission.
;;;
;;; There are certain criteria for skipping user handlers in stages 2 and 4 of
;;; a signal emission.
;;;
;;; First, user handlers may be blocked. Blocked handlers are omitted during
;;; callback invocation, to return from the blocked state, a handler has to get
;;; unblocked exactly the same amount of times it has been blocked before.
;;;
;;; Second, upon emission of a G_SIGNAL_DETAILED signal, an additional detail
;;; argument passed in to g_signal_emit() has to match the detail argument of
;;; the signal handler currently subject to invocation. Specification of no
;;; detail argument for signal handlers (omission of the detail part of the
;;; signal specification upon connection) serves as a wildcard and matches any
;;; detail argument passed in to emission.
;;;
;;; While the detail argument is typically used to pass an object property name
;;; (as with “notify”), no specific format is mandated for the detail string,
;;; other than that it must be non-empty.
;;;
;;; Memory management of signal handlers
;;;
;;; If you are connecting handlers to signals and using a GObject instance as
;;; your signal handler user data, you should remember to pair calls to
;;; g_signal_connect() with calls to g_signal_handler_disconnect() or
;;; g_signal_handlers_disconnect_by_func(). While signal handlers are
;;; automatically disconnected when the object emitting the signal is finalised,
;;; they are not automatically disconnected when the signal handler user data is
;;; destroyed. If this user data is a GObject instance, using it from a signal
;;; handler after it has been finalised is an error.
;;;
;;; There are two strategies for managing such user data. The first is to
;;; disconnect the signal handler (using g_signal_handler_disconnect() or
;;; g_signal_handlers_disconnect_by_func()) when the user data (object) is
;;; finalised; this has to be implemented manually. For non-threaded programs,
;;; g_signal_connect_object() can be used to implement this automatically.
;;; Currently, however, it is unsafe to use in threaded programs.
;;;
;;; The second is to hold a strong reference on the user data until after the
;;; signal is disconnected for other reasons. This can be implemented
;;; automatically using g_signal_connect_data().
;;;
;;; The first approach is recommended, as the second approach can result in
;;; effective memory leaks of the user data if the signal handler is never
;;; disconnected for some reason.
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
  (loop for i from 0 below count-of-args
        collect (parse-g-value (mem-aptr args '(:struct g-value) i))))

(defun retrieve-handler-from-object (object function-id)
  (aref (g-object-signal-handlers object) function-id))

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
;;; GSignalCVaMarshaller
;;;
;;; typedef GVaClosureMarshal GSignalCVaMarshaller;
;;;
;;; This is the signature of va_list marshaller functions, an optional
;;; marshaller that can be used in some situations to avoid marshalling the
;;; signal argument into GValues.
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
 "@version{2020-10-2}
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
    @entry[:detailed]{The signal supports \"::detail\" appendices to the
      signal name upon handler connections and emissions.}
    @entry[:action]{Action signals are signals that may freely be emitted on
      alive objects from user code via the function @fun{g-signal-emit} and
      friends, without the need of being embedded into extra code that performs
      pre or post emission adjustments on the object. They can also be thought
      of as object methods which can be called generically by third-party code.}
    @entry[:no-hooks]{No emissions hooks are supported for this signal.}
    @entry[:must-collect]{Varargs signal emission will always collect the
      arguments, even if there are no signal handlers connected. Since 2.30}
    @entry[:deprecated]{The signal is deprecated and will be removed in a future
      version. A warning will be generated if it is connected while running with
      @code{G_ENABLE_DIAGNOSTIC = 1}. Since 2.32}
  @end{table}
  @see-function{g-signal-query}
  @see-function{g-signal-emit}")

(export 'g-signal-flags)

;;; ----------------------------------------------------------------------------
;;; enum GSignalMatchType
;;; ----------------------------------------------------------------------------

;; Only the value :id is used in the function g-signal-handler-find
;; Consider to remove the implementation. We do not export this bitfield.

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

;;; ----------------------------------------------------------------------------
;;; struct GSignalQuery
;;; ----------------------------------------------------------------------------

(defcstruct %g-signal-query
  (:signal-id :uint)
  (:signal-name :string)
  (:owner-type g-type)
  (:signal-flags g-signal-flags)
  (:return-type (g-type :mangled-p t))
  (:n-params :uint)
  (:param-types (:pointer (g-type :mangled-p t))))

(defstruct g-signal-query
 #+cl-cffi-gtk-documentation
 "@version{2020-10-2}
  @begin{short}
    A structure holding in-depth information for a specific signal.
  @end{short}
  It is filled in by the function @fun{g-signal-query}.
  @begin{pre}
(defstruct g-signal-query
  signal-id
  signal-name
  owner-type
  signal-flags
  return-type
  param-types
  signal-detail)
  @end{pre}
  @begin[code]{table}
    @entry[signal-id]{A @code{:uint} with the signal ID of the signal being
      queried, or 0 if the signal to be queried was unknown.}
    @entry[signal-name]{A string with the signal name.}
    @entry[owner-type]{The interface/instance @class{g-object} type that this
      signal can be emitted for.}
    @entry[signal-flags]{The signal flags of type @symbol{g-signal-flags}.}
    @entry[return-type]{The return @class{g-type} for user callbacks.}
    @entry[param-types]{A list with the individual parameter types for user
      callbacks.}
    @enrtry[signal-detail]{A string with the signal detail.}
  @end{table}
  @see-class{g-type}
  @see-class{g-object}
  @see-symbol{g-signal-flags}
  @see-function{g-signal-query}"
  signal-id
  signal-name
  owner-type
  signal-flags
  return-type
  param-types
  signal-detail)

(defmethod print-object ((instance g-signal-query) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
         (format stream
                 "Signal [#~A] ~A ~A.~A~@[::~A~] (~{~A~^, ~})~@[ [~{~A~^, ~}]~]"
                 (g-signal-query-signal-id instance)
                 (gtype-name (g-signal-query-return-type instance))
                 (gtype-name (g-signal-query-owner-type instance))
                 (g-signal-query-signal-name instance)
                 (g-signal-query-signal-detail instance)
                 (mapcar #'gtype-name (g-signal-query-param-types instance))
                 (g-signal-query-signal-flags instance)))))

(export 'g-signal-query)

;;; ----------------------------------------------------------------------------
;;; Accessor details
;;; ----------------------------------------------------------------------------

;;; --- g-signal-query-signal-id -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-signal-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-signal-id 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-signal-id instance) => signal-id}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[signal-id]{a @code{:uint} with the signal ID of the signal being
      queried, or 0 if the signal to be queried was unknown}
  @begin{short}
    Accessor of the @code{signal-id} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-function{g-signal-query}")

(export 'g-signal-query-signal-id)

;;; --- g-signal-query-signal-name ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-signal-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-signal-name 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-signal-name instance) => signal-name}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[signal-name]{a string with the signal name}
  @begin{short}
    Accessor of the @code{signal-name} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-function{g-signal-query}")

(export 'g-signal-query-signal-name)

;;; --- g-signal-query-owner-type ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-owner-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-owner-type 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-owner-type instance) => owner-type}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[owner-type]{the interface/instance @class{g-object} type that this
    signal can be emitted for}
  @begin{short}
    Accessor of the @code{owner-type} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-class{g-object}
  @see-function{g-signal-query}")

(export 'g-signal-query-owner-type)

;;; --- g-signal-query-signal-flags ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-signal-flags atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-signal-flags 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-signal-flags instance) => signal-flags}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[signal-flags]{the signal flags of type @symbol{g-signal-flags}}
  @begin{short}
    Accessor of the @code{signal-flags} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-symbol{g-signal-flags}
  @see-function{g-signal-query}")

(export 'g-signal-query-signal-flags)

;;; --- g-signal-query-return-type ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-return-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-return-type 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-return-type instance) => return-type}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[return-type]{the return @class{g-type} for user callbacks}
  @begin{short}
    Accessor of the @code{return-type} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-function{g-signal-query}")

(export 'g-signal-query-return-type)

;;; --- g-signal-query-param-types ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-param-types atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-param-types 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-param-types instance) => param-types}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[param-types]{a list with the individual parameter types for user
    callbacks}
  @begin{short}
    Accessor of the @code{param-types} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-function{g-signal-query}")

(export 'g-signal-query-param-types)

;;; --- g-signal-query-signal-detail -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-signal-query-signal-detail atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-signal-query-signal-detail 'function)
 "@version{2020-10-3}
  @syntax[]{(g-signal-query-signal-detail instance) => signal-detail}
  @argument[instance]{a @struct{g-signal-query} structure}
  @argument[signal-detail]{a string with the signal detail}
  @begin{short}
    Accessor of the @code{signal-detail} slot of the @class{g-signal-query}
    structure.
  @end{short}
  See the function @fun{g-signal-query}.
  @see-function{g-signal-query}")

(export 'g-signal-query-signal-detail)

;;; ----------------------------------------------------------------------------
;;; enum GConnectFlags
;;; ----------------------------------------------------------------------------

(defbitfield g-connect-flags
  :after
  :swapped)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-connect-flags atdoc:*symbol-name-alias*)
      "Bitfield"
      (gethash 'g-connect-flags atdoc:*external-symbols*)
 "@version{2021-9-23}
  @begin{short}
    The connection flags are used to specify the behaviour of the connection
    of the signal.
  @end{short}
  @begin{pre}
(defbitfield g-connect-flags
  :after
  :swapped)
  @end{pre}
  @begin[code]{table}
    @entry[:after]{Whether the handler should be called before or after the
      default handler of the signal.}
    @entry[:swapped]{Whether the instance and data should be swapped when
      calling the handler.}
  @end{table}
  @see-function{g-signal-connect}")

(export 'g-connect-flags)

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

;; This function is not used in the cl-cffi-gtk library. We dot not export
;; the function.

(defcfun ("g_signal_newv" %g-signal-newv) :uint
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
;;; g_signal_set_va_marshaller ()
;;;
;;; void
;;; g_signal_set_va_marshaller (guint signal_id,
;;;                             GType instance_type,
;;;                             GSignalCVaMarshaller va_marshaller);
;;;
;;; Change the GSignalCVaMarshaller used for a given signal. This is a
;;; specialised form of the marshaller that can often be used for the common
;;; case of a single connected signal handler and avoids the overhead of GValue.
;;; Its use is optional.
;;;
;;; signal_id:
;;;     the signal id
;;;
;;; instance_type:
;;;     the instance type on which to set the marshaller.
;;;
;;; va_marshaller:
;;;     the marshaller to set.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_query ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_query" %g-signal-query) :void
  (signal-id :uint)
  (query (:pointer (:struct %g-signal-query))))

(defun g-signal-query (signal-id)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-3}
  @argument[signal-id]{a @code{:uint} with the signal ID of the signal to query
    information for}
  @return{A @struct{g-signal-query} structure with the signal info.}
  @begin{short}
    Returns the signal info.
  @end{short}
  Queries the signal system for in-depth information about a specific signal.
  This function will return signal-specific information. If an invalid signal
  ID is passed in, the @arg{signal-id} member is 0.
  @begin[Example]{dictionary}
    Retrieve information for the \"draw\" signal of a widget:
    @begin{pre}
  (setq query (g-signal-query (g-signal-lookup \"show\" \"GtkWidget\")))
=> #<Signal [#36] void GtkWidget.show () [RUN-FIRST]>
  (g-signal-query-signal-id query)
=> 36
  (g-signal-query-signal-name query)
=> \"show\"
  (g-signal-query-owner-type query)
=> #<GTYPE :name \"GtkWidget\" :id 18826464>
  (g-signal-query-signal-flags query)
=> (:RUN-FIRST)
  (g-signal-query-return-type query)
=> #<GTYPE :name \"void\" :id 4>
  (g-signal-query-param-types query)
=> NIL
  (g-signal-query-signal-detail query)
=> NIL
    @end{pre}
    A second example for the \"drag-drop\" signal of a widget:
    @begin{pre}
  (setq query (g-signal-query (g-signal-lookup \"drag-drop\" \"GtkWidget\")))
=> #<Signal [#91] gboolean GtkWidget.drag-drop (GdkDragContext, gint, gint, guint) [RUN-LAST]>
  (g-signal-query-signal-id query)
=> 91
  (g-signal-query-signal-name query)
=> \"drag-drop\"
  (g-signal-query-owner-type query)
=> #<GTYPE :name \"GtkWidget\" :id 18826464>
  (g-signal-query-signal-flags query)
=> (:RUN-LAST)
  (g-signal-query-return-type query)
=> #<GTYPE :name \"gboolean\" :id 20>
  (g-signal-query-param-types query)
=> (#<GTYPE :name \"GdkDragContext\" :id 18798624> #<GTYPE :name \"gint\" :id 24>
 #<GTYPE :name \"gint\" :id 24> #<GTYPE :name \"guint\" :id 28>)
  (g-signal-query-signal-detail query)
=> NIL
    @end{pre}
  @end{dictionary}
  @see-struct{g-signal-query}"
  (with-foreign-object (query '(:struct %g-signal-query))
    (%g-signal-query signal-id query)
    (assert (not (zerop (foreign-slot-value query
                                            '(:struct %g-signal-query)
                                            :signal-id))))
    (let ((param-types
            (iter
              (with param-types = (foreign-slot-value query
                                                      '(:struct %g-signal-query)
                                                      :param-types))
              (for i from 0 below (foreign-slot-value query
                                                      '(:struct %g-signal-query)
                                                      :n-params))
              (for param-type = (mem-aref param-types
                                          '(g-type :mangled-p t)
                                          i))
              (collect param-type))))
      (make-g-signal-query :signal-id
                           signal-id
                           :signal-name
                           (foreign-slot-value query
                                               '(:struct %g-signal-query)
                                               :signal-name)
                           :owner-type
                           (foreign-slot-value query
                                               '(:struct %g-signal-query)
                                               :owner-type)
                           :signal-flags
                           (foreign-slot-value query
                                               '(:struct %g-signal-query)
                                               :signal-flags)
                           :return-type
                           (foreign-slot-value query
                                               '(:struct %g-signal-query)
                                               :return-type)
                           :param-types param-types))))

(export 'g-signal-query)

;;; ----------------------------------------------------------------------------
;;; g_signal_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_lookup" g-signal-lookup) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-10-2}
  @argument[name]{a string with the signal's name}
  @argument[itype]{the @class{g-type} that the signal operates on}
  @return{A @code{:uint} with the signal's identifying number, or 0 if no
    signal was found.}
  @begin{short}
    Given the name of the signal and the type of object it connects to, gets
    the signal's identifying integer.
  @end{short}
  Emitting the signal by number is somewhat faster than using the name each
  time. Also tries the ancestors of the given type.
  @begin[Example]{dictionary}
    @begin{pre}
  (g-signal-lookup \"notify\" \"GObject\")
=> 1
  (g-signal-lookup \"notify\" \"GtkWidget\")
=> 1
  (g-signal-lookup \"unknown\" \"GObject\")
=> 0
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-function{g-signal-name}
  @see-function{g-signal-query}
  @see-function{g-signal-list-ids}"
  (name :string)
  (itype g-type))

(export 'g-signal-lookup)

;;; ----------------------------------------------------------------------------
;;; g_signal_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_name" g-signal-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-10-3}
  @argument[signal-id]{a @code{:uint} with the signal's identifying number}
  @return{A string with the signal name, or @code{nil} if the signal number was
    invalid.}
  @begin{short}
    Given the signal's identifier, finds its name.
  @end{short}
  Two different signals may have the same name, if they have differing types.
  @begin[Example]{dictionary}
    Get the signal ID for the \"draw\" signal and then get the name for the ID:
    @begin{pre}
  (g-signal-lookup \"draw\" \"GtkWidget\")
=> 52
  (g-signal-name *)
=> \"draw\"
    @end{pre}
    List the IDs for a button widget and retrieves the names of the signals:
    @begin{pre}
  (g-signal-list-ids \"GtkButton\")
=> (230 227 225 226 228 229)
  (mapcar #'g-signal-name *)
=> (\"activate\" \"clicked\" \"pressed\" \"released\" \"enter\" \"leave\")
    @end{pre}
  @end{dictionary}
  @see-function{g-signal-query}
  @see-function{g-signal-lookup}
  @see-function{g-signal-list-ids}"
  (signal-id :uint))

(export 'g-signal-name)

;;; ----------------------------------------------------------------------------
;;; g_signal_list_ids ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_list_ids" %g-signal-list-ids) (:pointer :uint)
  (itype g-type)
  (n-ids (:pointer :uint)))

(defun g-signal-list-ids (itype)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-3}
  @argument[itype]{a instance or interface @class{g-type}}
  @return{A list of signal IDs of type @code{:uint}.}
  @begin{short}
    Lists the signals by ID that a certain instance or interface type created.
  @end{short}
  Further information about the signals can be acquired through the function
  @fun{g-signal-query}.
  @begin[Example]{dictionary}
    Get the IDs for a window widget and show the names of the signals:
    @begin{pre}
  (mapcar #'g-signal-name (g-signal-list-ids \"GtkWindow\"))
=> (\"keys-changed\" \"set-focus\" \"activate-focus\" \"activate-default\"
    \"enable-debugging\")
    @end{pre}
  @end{dictionary}
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

(defun g-signal-emit (instance detailed-signal &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-3}
  @argument[instance]{the @class{g-object} instance the signal is being emitted
    on}
  @argument[detailed-signal]{a string with the detailed signal name}
  @argument[args]{parameters to be passed to the signal}
  @return{The return value of the signal.}
  @short{Emits a signal.}
  Note that the function @sym{g-signal-emit} resets the return value to the
  default if no handlers are connected.
  @begin[Lisp implementation]{dictionary}
    In the Lisp implementation this function takes not the signal ID but the
    detailed signal name as an argument. For this case the C library has the
    function @code{g_signal_emit_by_name()}, which is not implemented in the
    Lisp binding.

    At this time setting a @code{GParam} value is not implemented in the
    Lisp binding. Therefore, you can not emit a \"notify::<property>\"
    signal on an instance.
  @end{dictionary}
  @see-class{g-object}"
  (let* ((instance-type (g-type-from-instance (pointer instance)))
         (query (g-signal-parse-name instance-type detailed-signal)))
    (unless query
      (error "Signal ~A not found on instance ~A" detailed-signal instance))
    (let ((params-count (length (g-signal-query-param-types query))))
      (assert (= params-count (length args)))
      (with-foreign-object (params '(:struct g-value) (1+ params-count))
        (set-g-value (mem-aptr params '(:struct g-value) 0)
                     instance
                     instance-type
                     :zero-g-value t)
        (iter (for i from 0 below params-count)
              (for arg in args)
              (for type in (g-signal-query-param-types query))
              (set-g-value (mem-aptr params '(:struct g-value) (1+ i))
                           arg
                           type
                           :zero-g-value t))
        (prog1
          (if (equal (g-signal-query-return-type query)
                     (gtype +g-type-none+))
              ;; Emit a signal which has no return value
              (let ((detail (g-signal-query-signal-detail query)))
                (%g-signal-emitv params
                                 (g-signal-query-signal-id query)
                                 (if detail detail (null-pointer))
                                 (null-pointer)))
              ;; Emit a signal which has a return value
              (with-foreign-object (return-value '(:struct g-value))
                (g-value-init return-value
                              (g-signal-query-return-type query))
                (let ((detail (g-signal-query-signal-detail query)))
                  (%g-signal-emitv params
                                   (g-signal-query-signal-id query)
                                   (if detail detail (null-pointer))
                                   return-value))
                (prog1
                  ;; Return value of the signal
                  (parse-g-value return-value)
                  (g-value-unset return-value))))
          (iter (for i from 0 below (1+ params-count))
                (g-value-unset (mem-aptr params '(:struct g-value) i))))))))

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

;; Called from g-signal-emit. For internal use and not exported.

(defcfun ("g_signal_emitv" %g-signal-emitv) :void
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

(defun g-signal-connect (instance signal handler &key after)
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-8}
  @argument[instance]{the @class{g-object} instance to connect to}
  @argument[signal]{a string of the form \"signal-name::detail\"}
  @argument[handler]{the Lisp callback function to connect}
  @argument[after]{if @em{true} the handler is called after the default handler}
  @return{A unsigned long integer with the handler ID.}
  @begin{short}
    Connects a Lisp callback function to a signal for a particular object.
  @end{short}
  The handler will be called before the default handler of the signal. If the
  @arg{after} keyword argument is @em{true}, the handler will be called after
  the default handler of the signal.
  @begin[Lisp implmentation]{dictionary}
    The C library knows in addition the @code{g_signal_connect_after()}
    function, which is implemented as the @fun{g-signal-connect-after} function
    and is equivalent to this function with a @em{true} value  for the
    @arg{after} keyword argument.
  @end{dictionary}
  @begin[Example]{dictionary}
    Connect a Lisp lambda function to the signal \"destroy\" of a window:
    @begin{pre}
(g-signal-connect window \"destroy\"
                  (lambda (widget)
                    (declare (ignore widget))
                    (leave-gtk-main)))
    @end{pre}
    Connect a Lisp lambda function to the signal \"toggled\" of a toggle button:
    @begin{pre}
(g-signal-connect button \"toggled\"
   (lambda (widget)
     (if (gtk-toggle-button-active widget)
         (progn
           ;; If control reaches here, the toggle button is down
         )
        (progn
           ;; If control reaches here, the toggle button is up
         ))))
    @end{pre}
    If it is necessary to have a separate function which needs user data, the
    following implementation is possible:
    @begin{pre}
(defun separate-event-handler (widget arg1 arg2 arg3)
  [ here is the code of the event handler ] )

(g-signal-connect window \destroy\"
                  (lambda (widget)
                    (separate-event-handler widget arg1 arg2 arg3)))
    @end{pre}
    If no extra data is needed, but the callback function should be separated
    out than it is also possible to implement something like:
    @begin{pre}
(g-signal-connect window \"destroy\" #'separate-event-handler)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-function{g-signal-connect-after}"
  (%g-signal-connect-closure (pointer instance)
                             signal
                             (create-closure instance handler)
                             after))

(export 'g-signal-connect)

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_after()
;;; ----------------------------------------------------------------------------

(declaim (inline g-signal-connect-after))

(defun g-signal-connect-after (instance detailed-signal handler)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-24}
  @argument[instance]{the @class{g-object} instance to connect to}
  @argument[detailed-signal]{a string of the form \"signal-name::detail\"}
  @argument[handler]{the Lisp callback function to connect}
  @return{A unsigned long with the handler ID.}
  @begin{short}
    Connects a Lisp callback function to a signal for a particular object.
  @end{short}
  The handler will be called after the default handler of the signal.
  @begin[Lisp implementation]{dictionary}
    In the Lisp implementation the function @fun{g-signal-connect} has a
    keyword argument @arg{after}. This function is implemented as:
    @begin{pre}
 (g-signal-connect instance detailed-signal handler :after t)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
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
;;; this does not matter, since the signal will automatically be removed, but if
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

;; Called from g-signal-connect. For internal use and not exported.

(defcfun ("g_signal_connect_closure" %g-signal-connect-closure) :ulong
 #+cl-cffi-gtk-documentation
 "@version{2020-10-1}
  @argument[instance]{a @code{:pointer} to the instance to connect to}
  @argument[detailed-signal]{a string of the form \"signal-name::detail\"}
  @argument[closure]{the @symbol{g-closure} callback function to connect}
  @argument[after]{a boolean whether the handler should be called before or
    after the default handler of the signal}
  @return{A @code{:ulong} with the handler ID.}
  @begin{short}
    Connects a callback function to a signal for a particular object.
  @end{short}
  This is a low level function which is called from the function
  @fun{g-signal-connect} to connect a Lisp callback function to a signal.
  @see-symbol{g-closure}
  @see-function{g-signal-connect}"
  (instance :pointer)
  (detailed-signal :string)
  (closure (:pointer (:struct g-closure)))
  (after :boolean))

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
 "@version{2020-10-3}
  @argument[instance]{the @class{g-object} instance to block the signal handler
    of}
  @argument[handler-id]{a @code{:ulong} handler ID of the handler to be blocked}
  @begin{short}
    Blocks a handler of an instance so it will not be called during any signal
    emissions unless it is unblocked again.
  @end{short}
  Thus \"blocking\" a signal handler means to temporarily deactive it. A signal
  handler has to be unblocked exactly the same amount of times it has been
  blocked before to become active again.

  The @arg{handler-id} has to be a valid signal handler ID, connected to a
  signal of instance.
  @see-class{g-object}
  @see-function{g-signal-handler-unblock}"
  (instance g-object)
  (handler-id :ulong))

(export 'g-signal-handler-block)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_unblock ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_unblock" g-signal-handler-unblock) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-10-3}
  @argument[instance]{the @class{g-object} instance to unblock the signal
    handler of}
  @argument[handler-id]{a @code{:ulong} handler ID of the handler to be
    unblocked}
  @begin{short}
    Undoes the effect of a previous call of the function
    @fun{g-signal-handler-block}.
  @end{short}
  A blocked handler is skipped during signal emissions and will not be invoked,
  unblocking it (for exactly the amount of times it has been blocked before)
  reverts its \"blocked\" state, so the handler will be recognized by the signal
  system and is called upon future or currently ongoing signal emissions
  (since the order in which handlers are called during signal emissions is
  deterministic, whether the unblocked handler in question is called as part
  of a currently ongoing emission depends on how far that emission has
  proceeded yet).

  The @arg{handler-id} has to be a valid ID of a signal handler that is
  connected to a signal of instance and is currently blocked.
  @see-class{g-object}
  @see-function{g-signal-handler-block}"
  (instance g-object)
  (handler-id :ulong))

(export 'g-signal-handler-unblock)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_disconnect ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_disconnect" %g-signal-handler-disconnect) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-6}
  @argument[instance]{the @class{g-object} instance to remove the signal
    handler from}
  @argument[handler-id]{a @code{:ulong} handler ID of the handler to be
    disconnected}
  @begin{short}
    Disconnects a handler from an instance so it will not be called during any
    future or currently ongoing emissions of the signal it has been connected
    to.
  @end{short}
  The @arg{handler-id} becomes invalid and may be reused.

  The @arg{handler-id} has to be a valid signal handler ID, connected to a
  signal of instance.
  @see-class{g-object}
  @see-function{g-signal-connect}"
  (object (g-object :free-to-foreign nil)) ; to stop a bug, seems to work?
  (handler-id :ulong))

(defun g-signal-handler-disconnect (object handler-id)
  (declare (ignore object handler-id))
  ;; TODO: The implementation of %g-signal-handler-disconnect is wrong.
  ;; In Lisp we have a list of signal handlers. The disconnected signal
  ;; handler must be removed from this list. And this time we do nothing
)

(export 'g-signal-handler-disconnect)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_find ()
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
 #+cl-cffi-gtk-documentation
 "@version{2020-10-1}
  @argument[instance]{the @class{g-object} instance owning the signal handler
    to be found}
  @argument[signal-id]{a @code{:unit} with a signal the handler has to be
    connected to}
  @return{A valid non-0 signal handler ID of type @code{:ulong} for a
    successful match.}
  @begin{short}
    Finds the first signal handler that matches the given @arg{signal-id}.
  @end{short}
  If no handler was found, 0 is returned.
  @begin[Lisp implementation]{dictionary}
    In the Lisp implementation only the match type @code{:id} of the
    @symbol{g-signal-match-type} flags is implemented.
  @end{dictionary}
  @see-class{g-object}
  @see-function{g-signal-match-type}"
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
;;; match flags is required for successful matches. If no handlers were found,
;;; 0 is returned, the number of blocked handlers otherwise.
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_handler_is_connected" g-signal-handler-is-connected)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-10-1}
  @argument[instance]{the @class{g-object} instance where a signal handler is
    sought}
  @argument[handler-id]{a @code{:ulong} with the handler ID}
  @return{A boolean whether @arg{handler-id} identifies a handler connected to
    instance.}
  @begin{short}
    Returns whether @arg{handler-id} is the ID of a handler connected to
    instance.
  @end{short}
  @see-class{g-object}
  @see-function{g-signal-connect}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_has_handler_pending" g-signal-has-handler-pending) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-10-1}
  @argument[instance]{the @class{g-object} instance whose signal handlers are
    sought}
  @argument[signal-id]{a @code{:uint} with the signal ID}
  @argument[detail]{a string with the detail}
  @argument[may-be-blocked]{a boolean whether blocked handlers should count as
    match}
  @return{@em{True} if a handler is connected to the signal, @em{false}
    otherwise.}
  @begin{short}
    Returns whether there are any handlers connected to @arg{instance} for the
    given signal ID and detail.
  @end{short}

  If detail is 0 then it will only match handlers that were connected without
  detail. If detail is non-zero then it will match handlers connected both
  without detail and with the given detail. This is consistent with how a
  signal emitted with detail would be delivered to those handlers.

  Since 2.46 this also checks for a non-default class closure being installed,
  as this is basically always what you want.

  One example of when you might use this is when the arguments to the signal
  are difficult to compute. A class implementor may opt to not emit the signal
  if no one is attached anyway, thus saving the cost of building the arguments.
  @see-class{g-object}
  @see-type{g-quark}
  @see-function{g-signal-connect}"
  (instance g-object)
  (signal-id :uint)
  (detail g-quark)
  (may-be-blocked :boolean))

(export 'g-signal-has-handler-pending)

;;; ----------------------------------------------------------------------------
;;; g_signal_stop_emission ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_stop_emission" g-signal-stop-emission) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-10-1}
  @argument[instance]{the @class{g-object} instance whose signal handlers you
    wish to stop}
  @argument[signal-id]{a @code{:uint} with the signal identifier, as returned
    by the function @fun{g-signal-lookup}}
  @argument[detail]{a @type{g-quark} with the detail which the signal was
    emitted with}
  @begin{short}
    Stops a signal's current emission.
  @end{short}
  This will prevent the default method from running, if the signal was
  @code{:run-last} and you connected normally (i.e. without the \"after\"
  flag).

  Prints a warning if used on a signal which is not being emitted.
  @see-class{g-object}
  @see-type{g-quark}
  @see-function{g-signal-stop-emission-by-name}"
  (instance g-object)
  (signal-id :uint)
  (detail g-quark))

(export 'g-signal-stop-emission)

;;; ----------------------------------------------------------------------------
;;; g_signal_stop_emission_by_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_signal_stop_emission_by_name" g-signal-stop-emission-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-10-1}
  @argument[instance]{the @class{g-object} instance whose signal handlers you
    wish to stop}
  @argument[detailed-signal]{a string of the form \"signal-name::detail\"}
  @begin{short}
    Stops a signal's current emission.
  @end{short}
  This is just like the function @fun{g-signal-stop-emission} except it will
  look up the signal ID for you.
  @see-class{g-object}
  @see-function{g-signal-stop-emission}"
  (instance g-object)
  (detailed-signal :string))

(export 'g-signal-stop-emission-by-name)

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
;;; class offset for the signal's class handler. This function does not need a
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
;;; g_signal_is_valid_name ()
;;;
;;; gboolean
;;; g_signal_is_valid_name (const gchar *name);
;;;
;;; Validate a signal name. This can be useful for dynamically-generated signals
;;; which need to be validated at run-time before actually trying to create
;;; them.
;;;
;;; See canonical parameter names for details of the rules for valid names. The
;;; rules for signal names are the same as those for property names.
;;;
;;; name:
;;;     the canonical name of the signal
;;;
;;; Returns:
;;;     TRUE if name is a valid signal name, FALSE otherwise.
;;;
;;; Since 2.66
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

;; The Lisp function does not work as documented. The function is used in
;; g-signal-emit and not exported.

(defun g-signal-parse-name (owner-type signal-name)
  (with-foreign-objects ((signal-id :uint) (detail 'g-quark))
    (when (%g-signal-parse-name signal-name owner-type signal-id detail t)
      (let ((query (g-signal-query (mem-ref signal-id :uint))))
        (setf (g-signal-query-signal-detail query) (mem-ref detail 'g-quark))
        query))))

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
;;;     the offset of the member function of itype's class structure which is
;;;     to be invoked by the new closure
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

;;; ----------------------------------------------------------------------------
;;; g_clear_signal_handler ()
;;;
;;; void
;;; g_clear_signal_handler (gulong *handler_id_ptr,
;;;                         gpointer instance);
;;;
;;; Disconnects a handler from instance so it will not be called during any
;;; future or currently ongoing emissions of the signal it has been connected
;;; to. The handler_id_ptr is then set to zero, which is never a valid handler
;;; ID value (see g_signal_connect()).
;;;
;;; If the handler ID is 0 then this function does nothing.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts.
;;;
;;; handler_id_ptr:
;;;     A pointer to a handler ID (of type gulong) of the handler to be
;;;     disconnected.
;;;
;;; instance:
;;;     The instance to remove the signal handler from.
;;;
;;; Since 2.62
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.signals.lisp ---------------------------------------
