;;; ----------------------------------------------------------------------------
;;; gio.action-map.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;;     Interface for action containers
;;;
;;; Types and Values
;;;
;;;     GActionMap
;;;     GActionEntry
;;;
;;; Functions
;;;
;;;     g_action_map_lookup_action
;;;     g_action_map_add_action_entries
;;;     g_action_map_add_action
;;;     g_action_map_remove_action
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GActionMap
;;;
;;; Prerequisites
;;;
;;;    GActionMap requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GActionMap is implemented by GApplication and GSimpleActionGroup.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GActionMap
;;; ----------------------------------------------------------------------------

(define-g-interface "GActionMap" g-action-map
  (:export t
   :type-initializer "g_action_map_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-map atdoc:*class-name-alias*)
      "Interface"
      (documentation 'g-action-map 'type)
 "@version{2021-5-11}
  @begin{short}
    The @sym{g-action-map} interface is implemented by @class{g-action-group}
    implementations that operate by containing a number of named
    @class{g-action} instances, such as a @class{g-simple-action-group} object.
  @end{short}

  One useful application of this interface is to map the names of actions from
  various action groups to unique, prefixed names, e.g. by prepending
  @code{app.} or @code{win.}. This is the motivation for the 'map' part of the
  interface name.
  @see-class{g-action}
  @see-class{g-action-group}
  @see-class{g-simple-action-group}")

;;; ----------------------------------------------------------------------------
;;; g_action_map_lookup_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_lookup_action" g-action-map-lookup-action)
    (g-object g-action)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-11}
  @argument[map]{a @class{g-action-map} object}
  @argument[name]{a string with the name of an action}
  @return{A @class{g-action} object, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{name} in the action map.
  @end{short}
  If no such action exists, returns @code{nil}.
  @see-class{g-action}
  @see-class{g-action-map}"
  (map (g-object g-action-map))
  (name :string))

(export 'g-action-map-lookup-action)

;;; ----------------------------------------------------------------------------
;;; struct GActionEntry
;;; ----------------------------------------------------------------------------

;; This structure is not used in the Lisp implementation

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action_entries ()
;;; ----------------------------------------------------------------------------

(defun g-action-map-add-action-entries (map entries)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-2}
  @argument[map]{a @class{g-action-map} object}
  @argument[entries]{a list of descriptions for the actions}
  @begin{short}
    A convenience function for creating multiple @class{g-simple-action}
    instances and adding them to a @class{g-action-map} object.
  @end{short}

  Each action in the list @arg{entries} is constructed from the following
  parameters:
  @begin[code]{table}
    @entry[name]{A string with the name of the action.}
    @entry[activate]{The callback to connect to the \"activate\" signal of the
      action. Since GLib 2.40, this can be @code{nil} for stateful actions, in
      which case the default handler is used. For boolean-stated actions with
      no parameter, this is a toggle. For other state types, and parameter type
      equal to the state type, this will be a function that just calls the
      callback function @code{change-state}, which you should provide.}
    @entry[parameter-type]{The type of the parameter that must be
      passed to the activate function for this action, given as a single
      @type{g-variant} type string, or @code{nil} for no parameter.}
    @entry[state]{The initial state for this action, given in
      @type{g-variant} text format. The state is parsed with no extra type
      information, so type tags must be added to the string if they are
      necessary. Stateless actions should give @code{nil} here.}
    @entry[change-state]{The callback to connect to the \"change-state\"
      signal of the action. All stateful actions should provide a handler here,
      stateless actions should not.}
  @end{table}
  All values after name are optional. Additional optional fields may be added
  in the future.
  @begin[Example]{dictionary}
    Using the function @sym{g-action-map-add-action-entries}:
    @begin{pre}
(defun activate-quit (action parameter)
  (declare (ignore action parameter)))

(defun activate-print (action parameter)
  (declare (ignore action parameter)))

(defun create-action-group ()
  (let ((entries (list (list \"quit\"
                             #'activate-quit)
                       (list \"print\"
                             #'activate-print
                             \"s\")))
        (group (g-simple-action-group-new)))
    (g-action-map-add-action-entries group entries)
    group))
    @end{pre}
  @end{dictionary}
  @see-class{g-action-map}
  @see-class{g-simple-action}"
  (dolist (entry entries)
    (let* ((action nil)
           (name (first entry))
           (activate (second entry))
           (vtype (when (third entry) (g-variant-type-new (third entry))))
           (state (when (fourth entry)
                    (g-variant-parse (when vtype vtype) (fourth entry))))
           (change-state (fifth entry)))
      (if state
          (setf action (g-simple-action-new-stateful name vtype state))
          (setf action (g-simple-action-new name vtype)))
      (when activate
        (g-signal-connect action "activate" activate))
      (when change-state
        (g-signal-connect action "change-state" change-state))
      (g-action-map-add-action map action))))

(export 'g-action-map-add-action-entries)

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_add_action" %g-action-map-add-action) :void
  (map :pointer)
  (action :pointer))

(defun g-action-map-add-action (map action)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-11}
  @argument[map]{a @class{g-action-map} object}
  @argument[action]{a @class{g-action} object}
  @begin{short}
    Adds an action to the action map.
  @end{short}
  If the action map already contains an action with the same name as
  @arg{action} then the old action is dropped from the action map.
  @see-class{g-action}
  @see-class{g-action-map}
  @see-function{g-action-map-remove-action}"
  (%g-action-map-add-action (pointer map) (pointer action)))

(export 'g-action-map-add-action)

;;; ----------------------------------------------------------------------------
;;; g_action_map_remove_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_remove_action" g-action-map-remove-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-5-11}
  @argument[map]{a @class{g-action-map} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Removes the named action from the action map.
  @end{short}
  If no action of this name is in the map then nothing happens.
  @see-class{g-action-map}
  @see-function{g-action-map-add-action}"
  (map (g-object g-action-map))
  (name :string))

(export 'g-action-map-remove-action)

;;; --- End of file gio.action-map.lisp ----------------------------------------
