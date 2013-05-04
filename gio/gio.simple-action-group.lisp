;;; ----------------------------------------------------------------------------
;;; gio.simple-action-group.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.36.1. The latest version of this documentation can be found
;;; on-line at <http://library.gnome.org/devel/gio/unstable/>. 
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
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
 "@version{2013-5-1}
  @begin{short}
    @sym{g-simple-action-group} is a hash table filled with @class{g-action}
    objects, implementing the @class{g-action-group} and @class{g-action-map}
    interfaces.
  @end{short}

  Since 2.28")

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-group-new))

(defun g-simple-action-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @return{A new @class{g-simple-action-group} object.}
  @begin{short}
    Creates a new, empty, @class{g-simple-action-group}.
  @end{short}

  Since 2.28"
  (make-instance 'g-simple-action-group))

(export 'g-simple-action-group-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_lookup" g-simple-action-group-lookup)
    (g-object g-action)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[simple]{a @class{g-simple-action-group} object}
  @argument[action-name]{the name of an action}
  @return{A @class{g-action}, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{action-name} in the group.
  @end{short}

  If no such action exists, returns @code{nil}.

  Since 2.28"
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

(defcfun ("g_simple_action_group_add_entries"
          %g-simple-action-group-add-entries) :void
  (simple (g-object g-simple-action-group))
  (entries :pointer)
  (n-entries :int)
  (user-data :pointer))

(defun g-simple-action-group-add-entries (simple entries)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[simple]{a @class{g-simple-action-group} object}
  @argument[entries]{a pointer to the first item in an array of
    @class{g-action-entry} structs}
  @begin{short}
    A convenience function for creating multiple @class{g-simple-action}
    instances and adding them to the action group.
  @end{short}

  Since 2.30"
  (with-foreign-boxed-array (n-entries entries-ptr g-action-entry entries)
    (%g-simple-action-group-add-entries simple
                                        entries-ptr
                                        n-entries
                                        (null-pointer))))

(export 'g-simple-action-group-add-entries)

;;; --- End of file gio.simple-action-group.lisp -------------------------------
