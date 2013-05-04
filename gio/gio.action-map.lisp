;;; ----------------------------------------------------------------------------
;;; gio.action-map.lisp
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
(setf (documentation 'g-action-map 'type)
 "@version{2013-5-1}
  @begin{short}
    The @sym{g-action-map} interface is implemented by @class{g-action-group}
    implementations that operate by containing a number of named
    @class{g-action} instances, such as @class{g-simple-action-group}.
  @end{short}

  One useful application of this interface is to map the names of actions from
  various action groups to unique, prefixed names (e.g. by prepending \"app\".
  or \"win\".). This is the motivation for the 'Map' part of the interface
  name.")

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
 "@version{2013-5-1}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[action-name]{the name of an action}
  @return{A @class{g-action}, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{action-name} in @arg{action-map}.
  @end{short}

  If no such action exists, returns @code{nil}.

  Since 2.32"
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

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-entry atdoc:*class-name-alias*) "CStruct"
      (documentation 'g-action-entry 'type)
 "@version{2013-5-1}
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
    @entry[name]{The name of the action.}
    @entry[activate]{The callback to connect to the \"activate\" signal of
      the action.}
    @entry[parameter-type]{The type of the parameter that must be
      passed to the activate function for this action, given as a single
      @symbol{g-variant} type string (or @code{nil} for no parameter).}
    @entry[state]{The initial state for this action, given in
      @symbol{g-variant} text format. The state is parsed with no extra type
      information, so type tags must be added to the string if they are
      necessary.}
    @entry[change-state]{The callback to connect to the \"change-state\"
      signal of the action.}
  @end{table}
  @see-slot{g-action-entry-name}
  @see-slot{g-action-entry-activate}
  @see-slot{g-action-entry-parameter-type}
  @see-slot{g-action-entry-state}
  @see-slot{g-action-entry-change-state}
  @see-constructor{copy-g-action-entry}
  @see-constructor{make-g-action-entry}
  @see-function{g-action-map-add-action-entries}")

(export (boxed-related-symbols 'g-action-entry))

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_add_action_entries" %g-action-map-add-action-entries)
    :void
  (action-map (g-object g-action-map))
  (entries :pointer)
  (n-entries :int)
  (user-data :pointer))

(defun g-action-map-add-action-entries (action-map entries)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[entries]{a pointer to the first item in an array of
    @class{g-action-entry} structs}
  @argument[n-entries]{the length of entries, or -1 if entries is
    @code{NULL}-terminated}
  @begin{short}
    A convenience function for creating multiple @class{g-simple-action}
    instances and adding them to a @class{g-action-map} object.
  @end{short}

  Each action is constructed as per one @class{g-action-entry}.

  @b{Example :} Using the function @fun{g-action-map-add-action-entries}
  @begin{pre}
 static void
 activate_quit (GSimpleAction *simple,
                GVariant      *parameter,
                gpointer       user_data)
 {
   exit (0);
 @}

 static void
 activate_print_string (GSimpleAction *simple,
                        GVariant      *parameter,
                        gpointer       user_data)
 {
   g_print (\"%s\n\", g_variant_get_string (parameter, NULL));
 @}

 static GActionGroup *
 create_action_group (void)
 {
   const GActionEntry entries[] = {
     { \"quit\",         activate_quit              @},
     { \"print-string\", activate_print_string, \"s\" @}
   @};
   GSimpleActionGroup *group;

   group = g_simple_action_group_new ();
   g_action_map_add_action_entries (G_ACTION_MAP (group),
                                    entries, G_N_ELEMENTS (entries), NULL);

   return G_ACTION_GROUP (group);
 @}
  @end{pre}
  Since 2.32"
  (with-foreign-boxed-array (n-entries entries-ptr g-action-entry entries)
    (%g-action-map-add-action-entries action-map
                                      entries-ptr
                                      n-entries
                                      (null-pointer))))

(export 'g-action-map-add-action-entries)

;;; ----------------------------------------------------------------------------
;;; g_action_map_add_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_add_action" %g-action-map-add-action) :void
  (action-map :pointer)
  (action :pointer))

(defun g-action-map-add-action (action-map action)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[action]{a @class{g-action} object}
  @begin{short}
    Adds an @arg{action} to the @arg{action-map}.
  @end{short}

  If the action map already contains an action with the same name as action
  then the old action is dropped from the action map.

  The action map takes its own reference on action.

  Since 2.32"
  (%g-action-map-add-action (pointer action-map) (pointer action)))

(export 'g-action-map-add-action)

;;; ----------------------------------------------------------------------------
;;; g_action_map_remove_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_map_remove_action" g-action-map-remove-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[action-map]{a @class{g-action-map} object}
  @argument[action-name]{the name of the action}
  @begin{short}
    Removes the named action from the action map.
  @end{short}

  If no action of this name is in the map then nothing happens.

  Since 2.32"
  (action-map (g-object g-action-map))
  (action-name :string))

(export 'g-action-map-remove-action)

;;; --- End of file gio.action-map.lisp ----------------------------------------
