;;; ----------------------------------------------------------------------------
;;; gio.simple-action-group.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.32.3. The latest version of this documentation can be found
;;; on-line at http://library.gnome.org/devel/gio/unstable/.
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
;;;
;;; Description
;;;
;;; GSimpleActionGroup is a hash table filled with GAction objects, implementing
;;; the GActionGroup and GActionMap interfaces.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimpleActionGroup
;;;
;;; typedef struct _GSimpleActionGroup GSimpleActionGroup;
;;;
;;; The GSimpleActionGroup structure contains private data and should only be
;;; accessed using the provided API.
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(define-g-object-class "GSimpleActionGroup" g-simple-action-group
  (:superclass g-object
   :export t
   :interfaces ("GActionGroup" "GActionMap")
   :type-initializer "g_simple_action_group_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_new ()
;;;
;;; GSimpleActionGroup * g_simple_action_group_new (void);
;;;
;;; Creates a new, empty, GSimpleActionGroup.
;;;
;;; Returns :
;;;     a new GSimpleActionGroup
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-group-new))

(defun g-simple-action-group-new ()
  (make-instance 'g-simple-action-group))

(export 'g-simple-action-group-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_lookup ()
;;;
;;; GAction * g_simple_action_group_lookup (GSimpleActionGroup *simple,
;;;                                         const gchar *action_name);
;;;
;;; Looks up the action with the name action_name in the group.
;;;
;;; If no such action exists, returns NULL.
;;;
;;; simple :
;;;     a GSimpleActionGroup
;;;
;;; action_name :
;;;     the name of an action
;;;
;;; Returns :
;;;     a GAction, or NULL
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_lookup" g-simple-action-group-lookup)
    (g-object g-action)
  (simple (g-object g-simple-action-group))
  (action-name :string))

(export 'g-simple-action-group-lookup)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_insert ()
;;;
;;; void g_simple_action_group_insert (GSimpleActionGroup *simple,
;;;                                    GAction *action);
;;;
;;; Adds an action to the action group.
;;;
;;; If the action group already contains an action with the same name as action
;;; then the old action is dropped from the group.
;;;
;;; The action group takes its own reference on action.
;;;
;;; simple :
;;;     a GSimpleActionGroup
;;;
;;; action :
;;;     a GAction
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_insert" g-simple-action-group-insert) :void
  (simple (g-object g-simple-action-group))
  (action (g-object g-action)))

(export 'g-simple-action-group-insert)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_remove ()
;;;
;;; void g_simple_action_group_remove (GSimpleActionGroup *simple,
;;;                                    const gchar *action_name);
;;;
;;; Removes the named action from the action group.
;;;
;;; If no action of this name is in the group then nothing happens.
;;;
;;; simple :
;;;     a GSimpleActionGroup
;;;
;;; action_name :
;;;     the name of the action
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_remove" g-simple-action-group-remove) :void
  (simple (g-object g-simple-action-group))
  (action-name :string))

(export 'g-simple-action-group-remove)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_add_entries ()
;;;
;;; void g_simple_action_group_add_entries (GSimpleActionGroup *simple,
;;;                                         const GActionEntry *entries,
;;;                                         gint n_entries,
;;;                                         gpointer user_data);
;;;
;;; A convenience function for creating multiple GSimpleAction instances and
;;; adding them to the action group.
;;;
;;; simple :
;;;     a GSimpleActionGroup
;;;
;;; entries :
;;;     a pointer to the first item in an array of GActionEntry structs
;;;
;;; n_entries :
;;;     the length of entries, or -1
;;;
;;; user_data :
;;;     the user data for signal connections
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_group_add_entries"
          %g-simple-action-group-add-entries) :void
  (simple (g-object g-simple-action-group))
  (entries :pointer)
  (n-entries :int)
  (user-data :pointer))

(defun g-simple-action-group-add-entries (simple entries)
  (with-foreign-boxed-array (n-entries entries-ptr g-action-entry entries)
    (%g-simple-action-group-add-entries simple
                                        entries-ptr
                                        n-entries
                                        (null-pointer))))

(export 'g-simple-action-group-add-entries)

;;; --- End of file gio.simple-action-group.lisp -------------------------------
