;;; ----------------------------------------------------------------------------
;;; gio.simple-action-group.lisp
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
;;; GSimpleActionGroup
;;;
;;;     A simple GActionGroup implementation
;;;
;;; Types and Values
;;;
;;;     GSimpleActionGroup
;;;
;;; Functions
;;;
;;;     g_simple_action_group_new
;;;     g_simple_action_group_lookup                       deprecated
;;;     g_simple_action_group_insert                       deprecated
;;;     g_simple_action_group_remove                       deprecated
;;;     g_simple_action_group_add_entries                  deprecated
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GSimpleActionGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GSimpleActionGroup implements GActionGroup and GActionMap.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimpleActionGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GSimpleActionGroup" g-simple-action-group
  (:superclass g-object
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "g_simple_action_group_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-simple-action-group 'type)
 "@version{2021-4-15}
  @begin{short}
    The @sym{g-simple-action-group} class is a hash table filled with
    @class{g-action} objects, implementing the @class{g-action-group} and
    @class{g-action-map} interfaces.
  @end{short}
  @see-class{g-action}
  @see-class{g-action-map}
  @see-class{g-action-group}")

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-group-new))

(defun g-simple-action-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @return{A new @class{g-simple-action-group} object.}
  @begin{short}
    Creates a new, empty, action group.
  @end{short}
  @see-class{g-simple-action-group}"
  (make-instance 'g-simple-action-group))

(export 'g-simple-action-group-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_lookup ()                        deprecated
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_lookup" g-simple-action-group-lookup)
    (g-object g-action)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[group]{a @class{g-simple-action-group} object}
  @argument[name]{a string with the name of an action}
  @return{A @class{g-action} object, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{name} in the action group.
  @end{short}

  If no such action exists, returns @code{nil}.
  @begin[Warning]{dictionary}
    The function @sym{g-simple-action-group-lookup} has been deprecated since
    version 2.38 and should not be used in newly-written code. Use the function
    @fun{g-action-map-lookup-action}.
  @end{dictionary}
  @see-class{g-action}
  @see-class{g-simple-action-group}
  @see-function{g-action-map-lookup-action}"
  (group (g-object g-simple-action-group))
  (name :string))

(export 'g-simple-action-group-lookup)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_insert ()                        deprecated
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_insert" g-simple-action-group-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[group]{a @class{g-simple-action-group} object}
  @argument[action]{a @class{g-action} object}
  @begin{short}
    Adds an action to the action group.
  @end{short}

  If the action group already contains an action with the same name as
  @arg{action} then the old action is dropped from the action group.

  The action group takes its own reference on @arg{action}.
  @begin[Warning]{dictionary}
    The function @sym{g-simple-action-group-insert} has been deprecated since
    version 2.38 and should not be used in newly-written code. Use the function
    @fun{g-action-map-add-action}.
  @end{dictionary}
  @see-class{g-simple-action-group}
  @see-function{g-action-map-add-action}"
  (group (g-object g-simple-action-group))
  (action (g-object g-action)))

(export 'g-simple-action-group-insert)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_remove ()                        deprecated
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_remove" g-simple-action-group-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[group]{a @class{g-simple-action-group} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Removes the named action from the action group.
  @end{short}

  If no action of this name is in the action group then nothing happens.
  @begin[Warning]{dictionary}
    The function @sym{g-simple-action-group-remove} has been deprecated since
    version 2.38 and should not be used in newly-written code. Use the function
    @fun{g-action-map-remove-action}.
  @end{dictionary}
  @see-class{g-simple-action-group}
  @see-function{g-action-map-remove-action}"
  (group (g-object g-simple-action-group))
  (name :string))

(export 'g-simple-action-group-remove)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_add_entries ()                   deprecated
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-group-add-entries))

(defun g-simple-action-group-add-entries (group entries)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[group]{a @class{g-simple-action-group} object}
  @argument[entries]{a list of descriptions for the actions}
  @begin{short}
    A convenience function for creating multiple @class{g-simple-action}
    instances and adding them to the action group.
  @end{short}
  @begin[Note]{dictionary}
    In the Lisp implementation this function calls the function
    @fun{g-action-map-add-action-entries}. See the documentation of the
    function @fun{g-action-map-add-action-entries} for more information about
    the parameters in the list to describe an action.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{g-simple-action-group-add-entries} has been deprecated
    since version 2.38 and should not be used in newly-written code. Use the
    function @fun{g-action-map-add-action-entries}.
  @end{dictionary}
  @see-class{g-simple-action-group}
  @see-function{g-action-map-add-action-entries}"
  (g-action-map-add-action-entries group entries))

(export 'g-simple-action-group-add-entries)

;;; --- End of file gio.simple-action-group.lisp -------------------------------
