;;; ----------------------------------------------------------------------------
;;; gio.action-map.lisp
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
;;; GActionMap
;;;
;;; Interface for action containers
;;;
;;; Synopsis
;;;
;;;     GActionMap
;;;     GActionMapInterface
;;;
;;;     g_action_map_lookup_action
;;;
;;;     GActionEntry
;;;
;;;     g_action_map_add_action_entries
;;;     g_action_map_add_action
;;;     g_action_map_remove_action
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GActionMap
;;;
;;; Prerequisites
;;;
;;; GActionMap requires GActionGroup and GObject.
;;;
;;; Known Implementations
;;;
;;; GActionMap is implemented by GApplication and GSimpleActionGroup.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GActionMap
;;; ----------------------------------------------------------------------------

(define-g-interface "GActionMap" g-action-map
  (:export t
   :type-initializer "g_action_map_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-map atdoc:*class-name-alias*) "Interface"
      (documentation 'g-action-map 'type)
 "@version{2013-8-9}
  @begin{short}
    The @sym{g-action-map} interface is implemented by @class{g-action-group}
    implementations that operate by containing a number of named
    @class{g-action} instances, such as @class{g-simple-action-group}.
  @end{short}

  One useful application of this interface is to map the names of actions from
  various action groups to unique, prefixed names, e. g. by prepending \"app.\"
  or \"win.\". This is the motivation for the 'Map' part of the interface
  name.
  @see-class{g-action}
  @see-class{g-action-group}
  @see-class{g-simple-action-group}")

;;; ----------------------------------------------------------------------------
;;; struct GActionMapInterface
;;;
;;; struct GActionMapInterface {
;;;   GTypeInterface g_iface;
;;;
;;;   GAction * (* lookup_action) (GActionMap  *action_map,
;;;                                const gchar *action_name);
;;;   void      (* add_action)    (GActionMap  *action_map,
;;;                                GAction     *action);
;;;   void      (* remove_action) (GActionMap  *action_map,
;;;                                const gchar *action_name);
;;; };
;;;
;;; The virtual function table for GActionMap.
;;;
;;; GTypeInterface g_iface;
;;;
;;;
;;; lookup_action ()
;;;     the virtual function pointer for g_action_map_lookup_action()
;;;
;;; add_action ()
;;;     the virtual function pointer for g_action_map_add_action()
;;;
;;; remove_action ()
;;;     the virtual function pointer for g_action_map_remove_action()
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_action_map_lookup_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_lookup_action" g-action-map-lookup-action)
    (g-object g-action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-9}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[action-name]{the name of an action}
  @return{A @class{g-action}, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{action-name} in @arg{action-map}.
  @end{short}

  If no such action exists, returns @code{nil}.

  Since 2.32
  @see-class{g-action}
  @see-class{g-action-map}"
  (action-map (g-object g-action-map))
  (action-name :string))

(export 'g-action-map-lookup-action)

;;; ----------------------------------------------------------------------------
;;; struct GActionEntry
;;; ----------------------------------------------------------------------------

;; This structure is only used in the internal implementation of the function
;; g-action-map-add-action-entries and is therefore not exported.

(defcstruct (g-action-entry :size 32) ; The C structure has 12 private bytes.
  (name :string)
  (activate :pointer)
  (parameter-type :string)
  (state :string)
  (change-state :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-action-entry 'type)
 "@version{2013-8-9}
  @begin{short}
    The @sym{g-action-entry} structure defines a single action. It is for use
    with the function @fun{g-action-map-add-action-entries}.
  @end{short}

  The order of the items in the structure are intended to reflect frequency
  of use. It is permissible to use an incomplete initialiser in order to leave
  some of the later values as @code{NULL}. All values after name are optional.
  Additional optional fields may be added in the future.

  See the function @fun{g-action-map-add-action-entries} for an example.
  @begin{pre}
(defcstruct (g-action-entry :size 32) ; The C structure has 12 private bytes.
  (name :string)
  (activate :pointer)
  (parameter-type :string)
  (state :string)
  (change-state :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[name]{The name of the action.}
    @entry[activate]{The callback to connect to the \"activate\" signal of
      the action.}
    @entry[parameter-type]{The type of the parameter that must be
      passed to the activate function for this action, given as a single
      @type{g-variant} type string (or @code{nil} for no parameter).}
    @entry[state]{The initial state for this action, given in
      @type{g-variant} text format. The state is parsed with no extra type
      information, so type tags must be added to the string if they are
      necessary.}
    @entry[change-state]{The callback to connect to the \"change-state\"
      signal of the action.}
  @end{table}
  @see-function{g-action-map-add-action-entries}")

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action_entries ()
;;; ----------------------------------------------------------------------------

;; TODO: Consider to do an implemenation like the C function. This would allow
;; to pass Lisp functions instead of callback functions.

(defcfun ("g_action_map_add_action_entries" %g-action-map-add-action-entries)
    :void
  (action-map (g-object g-action-map))
  (entries (:pointer (:pointer (:struct g-action-entry))))
  (n-entries :int)
  (user-data :pointer))

(defun g-action-map-add-action-entries (action-map entries)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-9}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[entries]{a list of actions}
  @begin{short}
    A convenience function for creating multiple @class{g-simple-action}
    instances and adding them to a @class{g-action-map} object.
  @end{short}

  Each action in the list @arg{entries} is constructed from the following
  parameters:
  @begin[code]{table}
    @entry[name]{The name of the action.}
    @entry[activate]{The callback to connect to the \"activate\" signal of
      the action.}
    @entry[parameter-type]{The type of the parameter that must be
      passed to the activate function for this action, given as a single
      @type{g-variant} type string, or @code{nil} for no parameter.}
    @entry[state]{The initial state for this action, given in
      @type{g-variant} text format. The state is parsed with no extra type
      information, so type tags must be added to the string if they are
      necessary.}
    @entry[change-state]{The callback to connect to the \"change-state\"
      signal of the action.}
  @end{table}
  All values after name are optional. Additional optional fields may be added in
  the future.

  @b{Example :} Using the function @sym{g-action-map-add-action-entries}
  @begin{pre}
(defcallback activate-quit :void
    ((simple (g-object g-simple-action))
     (parameter (:pointer (:struct g-variant))))
  (declare (ignore simple parameter))
  (format t \"activate-quit called~%\"))

(defcallback activate-print-string :void
    ((simple (g-object g-simple-action))
     (parameter (:pointer (:struct g-variant))))
  (declare (ignore simple parameter))
  (format t \"activate-print-string~%\"))

(defun create-action-group ()
  (let ((entries (list (list \"quit\"
                             (callback activate-quit) nil nil nil)
                       (list \"print-string\"
                             (callback activate-print-string) \"s\" nil nil)))
        (group (g-simple-action-group-new)))
    (g-action-map-add-action-entries group entries)
    group))
  @end{pre}
  Since 2.32
  @see-class{g-action-map}
  @see-class{g-simple-action}"
  (let ((n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct g-action-entry) n-entries)
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct g-action-entry) i)
        do (setf (foreign-slot-value entry-ptr
                                     '(:struct g-action-entry)
                                     'name)
                 (first entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-action-entry)
                                     'activate)
                 (if (not (second entry)) (null-pointer) (second entry))
                 (foreign-slot-value entry-ptr
                                     '(:struct g-action-entry)
                                     'parameter-type)
                 (if (not (third entry)) (null-pointer) (third entry))
                 (foreign-slot-value entry-ptr
                                     '(:struct g-action-entry)
                                     'state)
                 (if (not (fourth entry)) (null-pointer) (fourth entry))
                 (foreign-slot-value entry-ptr
                                     '(:struct g-action-entry)
                                     'change-state)
                 (if (not (fifth entry)) (null-pointer) (fifth entry))))
      (%g-action-map-add-action-entries action-map
                                        entries-ptr
                                        n-entries
                                        (null-pointer)))))

(export 'g-action-map-add-action-entries)

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_add_action" %g-action-map-add-action) :void
  (action-map :pointer)
  (action :pointer))

(defun g-action-map-add-action (action-map action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-9}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[action]{a @class{g-action} object}
  @begin{short}
    Adds an @arg{action} to the @arg{action-map}.
  @end{short}

  If the action map already contains an action with the same name as
  @arg{action} then the old action is dropped from the action map.

  The action map takes its own reference on @arg{action}.

  Since 2.32
  @see-class{g-action}
  @see-class{g-action-map}
  @see-function{g-action-map-remove-action}"
  (%g-action-map-add-action (pointer action-map) (pointer action)))

(export 'g-action-map-add-action)

;;; ----------------------------------------------------------------------------
;;; g_action_map_remove_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_remove_action" g-action-map-remove-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-9}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[action-name]{the name of the action}
  @begin{short}
    Removes the named action from the action map.
  @end{short}

  If no action of this name is in the map then nothing happens.

  Since 2.32
  @see-class{g-action-map}
  @see-function{g-action-map-add-action}"
  (action-map (g-object g-action-map))
  (action-name :string))

(export 'g-action-map-remove-action)

;;; --- End of file gio.action-map.lisp ----------------------------------------
