;;; ----------------------------------------------------------------------------
;;; gio.simple-action-group.lisp
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
;;; GSimpleActionGroup
;;;
;;; A simple GActionGroup implementation
;;;
;;; Synopsis
;;;
;;;     GSimpleActionGroup
;;;
;;;     g_simple_action_group_new
;;;     g_simple_action_group_lookup
;;;     g_simple_action_group_insert
;;;     g_simple_action_group_remove
;;;     g_simple_action_group_add_entries
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GSimpleActionGroup
;;;
;;; Implemented Interfaces
;;;
;;; GSimpleActionGroup implements GActionGroup and GActionMap.
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
 "@version{2013-8-9}
  @begin{short}
    @sym{g-simple-action-group} is a hash table filled with @class{g-action}
    objects, implementing the @class{g-action-group} and @class{g-action-map}
    interfaces.
  @end{short}

  Since 2.28
  @see-class{g-action}
  @see-class{g-action-map}
  @see-class{g-action-group}")

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-group-new))

(defun g-simple-action-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-9}
  @return{A new @class{g-simple-action-group} object.}
  @begin{short}
    Creates a new, empty, @class{g-simple-action-group}.
  @end{short}

  Since 2.28
  @see-class{g-simple-action-group}"
  (make-instance 'g-simple-action-group))

(export 'g-simple-action-group-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_lookup" g-simple-action-group-lookup)
    (g-object g-action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-9}
  @argument[simple]{a @class{g-simple-action-group} object}
  @argument[action-name]{the name of an action}
  @return{A @class{g-action}, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{action-name} in the group.
  @end{short}

  If no such action exists, returns @code{nil}.

  Since 2.28
  @see-class{g-action}
  @see-class{g-simple-action-group}"
  (simple (g-object g-simple-action-group))
  (action-name :string))

(export 'g-simple-action-group-lookup)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_insert" g-simple-action-group-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[simple]{a @class{g-simple-action-group} object}
  @argument[action]{a @class{g-action} object}
  @begin{short}
    Adds an action to the action group.
  @end{short}

  If the action group already contains an action with the same name as action
  then the old action is dropped from the group.

  The action group takes its own reference on action.

  Since 2.28"
  (simple (g-object g-simple-action-group))
  (action (g-object g-action)))

(export 'g-simple-action-group-insert)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_remove" g-simple-action-group-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[simple]{a @class{g-simple-action-group} object}
  @argument[action-name]{the name of the action}
  @begin{short}
    Removes the named action from the action group.
  @end{short}

  If no action of this name is in the group then nothing happens.

  Since 2.28"
  (simple (g-object g-simple-action-group))
  (action-name :string))

(export 'g-simple-action-group-remove)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_add_entries ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-group-add-entries))

(defun g-simple-action-group-add-entries (simple entries)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-18}
  @argument[simple]{a @class{g-simple-action-group} object}
  @argument[entries]{a pointer to the first item in an array of
    @sym{g-action-entry} structs}
  @begin{short}
    A convenience function for creating multiple @class{g-simple-action}
    instances and adding them to the action group.
  @end{short}

  Since 2.30"
  (g-action-map-add-action-entries simple entries))

(export 'g-simple-action-group-add-entries)

;;; --- End of file gio.simple-action-group.lisp -------------------------------
