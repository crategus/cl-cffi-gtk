;;; ----------------------------------------------------------------------------
;;; gio.action-map.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.32.3. The latest version of this documentation can be found
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
;;;
;;; Description
;;;
;;; The GActionMap interface is implemented by GActionGroup implementations that
;;; operate by containing a number of named GAction instances, such as
;;; GSimpleActionGroup.
;;;
;;; One useful application of this interface is to map the names of actions from
;;; various action groups to unique, prefixed names (e.g. by prepending "app."
;;; or "win."). This is the motivation for the 'Map' part of the interface name.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GActionMap
;;;
;;; typedef struct _GActionMap GActionMap;
;;; ----------------------------------------------------------------------------

(define-g-interface "GActionMap" g-action-map
  (:export t
   :type-initializer "g_action_map_get_type"))

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
;;;
;;; GAction * g_action_map_lookup_action (GActionMap *action_map,
;;;                                       const gchar *action_name);
;;;
;;; Looks up the action with the name action_name in action_map.
;;;
;;; If no such action exists, returns NULL.
;;;
;;; action_map :
;;;     a GActionMap
;;;
;;; action_name :
;;;     the name of an action
;;;
;;; Returns :
;;;     a GAction, or NULL
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_lookup_action" g-action-map-lookup-action)
    (g-object g-action)
  (action-map (g-object g-action-map))
  (action-name :string))

(export 'g-action-map-lookup-action)

;;; ----------------------------------------------------------------------------
;;; struct GActionEntry
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct g-action-entry "GActionEntry"
  (name :string)
  (activate :pointer)
  (parameter-type :string)
  (state :string)
  (change-state :pointer))

(export (boxed-related-symbols 'g-action-entry))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry atdoc:*class-name-alias*) "Struct"
      (documentation 'g-action-entry 'type)
 "@version{2013-2-25}
  @begin{short}
    The @sym{g-action-entry} structure defines a single action. It is for use
    with @fun{g-action-map-add-action-entries}.
  @end{short}

  The order of the items in the structure are intended to reflect frequency
  of use. It is permissible to use an incomplete initialiser in order to leave
  some of the later values as @code{nil}. All values after name are optional.
  Additional optional fields may be added in the future.

  See @fun{g-action-map-add-action-entries} for an example.
  @begin{pre}
(define-g-boxed-cstruct g-action-entry \"GActionEntry\"
  (name :string)
  (activate :pointer)
  (parameter-type :string)
  (state :string)
  (change-state :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[name]{the name of the action}
    @entry[activate]{the callback to connect to the \"activate\" signal of
      the action}
    @entry[parameter-type]{the type of the parameter that must be
      passed to the activate function for this action, given as a single
      @symbol{g-variant} type string (or @code{nil} for no parameter)}
    @entry[state]{the initial state for this action, given in
      @symbol{g-variant} text format. The state is parsed with no extra type
      information, so type tags must be added to the string if they are
      necessary.}
    @entry[change-state]{the callback to connect to the \"change-state\"
      signal of the action}
  @end{table}
  @see-slot{g-action-entry-name}
  @see-slot{g-action-entry-activate}
  @see-slot{g-action-entry-parameter-type}
  @see-slot{g-action-entry-state}
  @see-slot{g-action-entry-change-state}
  @see-constructor{copy-g-action-entry}
  @see-constructor{make-g-action-entry}
  @see-function{g-action-map-add-action-entries}
")

;;; --- copy-g-action-entry ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-g-action-entry 'function)
 "@version{2013-2-25}
  @argument[instance]{a @class{g-action-entry} struct}
  @begin{short}
    Copy constructor of a @class{g-action-entry} struct.
  @end{short}")

;;; --- make-g-action-entry ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-g-action-entry 'function)
 "@version{2013-2-25}
  @argument[name]{the name of the action}
  @argument[activate]{the callback to connect to the \"activate\" signal of
    the action}
  @argument[parameter-type]{the type of the parameter that must be
      passed to the activate function for this action, given as a single
      @symbol{g-variant} type string (or @code{nil} for no parameter)}
  @argument[state]{the initial state for this action, given in
      @symbol{g-variant} text format. The state is parsed with no extra type
      information, so type tags must be added to the string if they are
      necessary.}
  @argument[change-state]{the callback to connect to the \"change-state\"
      signal of the action}
  @begin{short}
    Creates a @class{g-action-entry} struct.
  @end{short}")

;;; --- g-action-entry-name ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-entry-name 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{name} of the @class{g-action-entry} struct.
  @end{short}")

;;; --- g-action-entry-activate ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry-activate atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-entry-activate 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{activate} of the @class{g-action-entry} struct.
  @end{short}")

;;; --- g-action-entry-parameter-type ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry-parameter-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-entry-parameter-type 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{parameter-type} of the @class{g-action-entry}
    struct.
  @end{short}")

;;; --- g-action-entry-state ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-entry-state 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{state} of the @class{g-action-entry} struct.
  @end{short}")

;;; --- g-action-entry-change-state --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry-change-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-entry-change-state 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{change-state} of the @class{g-action-entry}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action_entries ()
;;;
;;; void g_action_map_add_action_entries (GActionMap *action_map,
;;;                                       const GActionEntry *entries,
;;;                                       gint n_entries,
;;;                                       gpointer user_data);
;;;
;;; A convenience function for creating multiple GSimpleAction instances and
;;; adding them to a GActionMap.
;;;
;;; Each action is constructed as per one GActionEntry.
;;;
;;; Example 20. Using g_action_map_add_action_entries()
;;;
;;; static void
;;; activate_quit (GSimpleAction *simple,
;;;                GVariant      *parameter,
;;;                gpointer       user_data)
;;; {
;;;   exit (0);
;;; }
;;;
;;; static void
;;; activate_print_string (GSimpleAction *simple,
;;;                        GVariant      *parameter,
;;;                        gpointer       user_data)
;;; {
;;;   g_print ("%s\n", g_variant_get_string (parameter, NULL));
;;; }
;;;
;;; static GActionGroup *
;;; create_action_group (void)
;;; {
;;;   const GActionEntry entries[] = {
;;;     { "quit",         activate_quit              },
;;;     { "print-string", activate_print_string, "s" }
;;;   };
;;;   GSimpleActionGroup *group;
;;;
;;;   group = g_simple_action_group_new ();
;;;   g_action_map_add_action_entries (G_ACTION_MAP (group),
;;;                                    entries, G_N_ELEMENTS (entries), NULL);
;;;
;;;   return G_ACTION_GROUP (group);
;;; }
;;;
;;; action_map :
;;;     a GActionMap
;;;
;;; entries :
;;;     a pointer to the first item in an array of GActionEntry structs
;;;
;;; n_entries :
;;;     the length of entries, or -1 if entries is NULL-terminated
;;;
;;; user_data :
;;;     the user data for signal connections
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_add_action_entries" %g-action-map-add-action-entries)
    :void
  (action-map (g-object g-action-map))
  (entries :pointer)
  (n-entries :int)
  (user-data :pointer))

(defun g-action-map-add-action-entries (action-map entries)
  (with-foreign-boxed-array (n-entries entries-ptr g-action-entry entries)
    (%g-action-map-add-action-entries action-map
                                      entries-ptr
                                      n-entries
                                      (null-pointer))))

(export 'g-action-map-add-action-entries)

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action ()
;;;
;;; void g_action_map_add_action (GActionMap *action_map,
;;;                               GAction *action);
;;;
;;; Adds an action to the action_map.
;;;
;;; If the action map already contains an action with the same name as action
;;; then the old action is dropped from the action map.
;;;
;;; The action map takes its own reference on action.
;;;
;;; action_map :
;;;     a GActionMap
;;;
;;; action :
;;;     a GAction
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_add_action" %g-action-map-add-action) :void
  (action-map :pointer)
  (action :pointer))

(defun g-action-map-add-action (action-map action)
  (%g-action-map-add-action (pointer action-map) (pointer action)))

(export 'g-action-map-add-action)

;;; ----------------------------------------------------------------------------
;;; g_action_map_remove_action ()
;;;
;;; void g_action_map_remove_action (GActionMap *action_map,
;;;                                  const gchar *action_name);
;;;
;;; Removes the named action from the action map.
;;;
;;; If no action of this name is in the map then nothing happens.
;;;
;;; action_map :
;;;     a GActionMap
;;;
;;; action_name :
;;;     the name of the action
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_remove_action" g-action-map-remove-action) :void
  (action-map (g-object g-action-map))
  (action-name :string))

(export 'g-action-map-remove-action)

;;; --- End of file gio.action-map.lisp ----------------------------------------
