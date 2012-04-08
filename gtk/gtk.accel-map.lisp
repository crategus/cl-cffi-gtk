;;; ----------------------------------------------------------------------------
;;; gtk.accel-map.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; Accelerator Maps
;;; 
;;; Loadable keyboard accelerator specifications
;;; 	
;;; Synopsis
;;; 
;;;     GtkAccelMap
;;;
;;;     gtk_accel_map_add_entry
;;;     gtk_accel_map_lookup_entry
;;;     gtk_accel_map_change_entry
;;;     gtk_accel_map_load
;;;     gtk_accel_map_save
;;;     gtk_accel_map_foreach
;;;     gtk_accel_map_load_fd
;;;     gtk_accel_map_save_fd
;;;     gtk_accel_map_load_scanner
;;;     gtk_accel_map_add_filter
;;;     gtk_accel_map_foreach_unfiltered
;;;     gtk_accel_map_get
;;;     gtk_accel_map_lock_path
;;;     gtk_accel_map_unlock_path
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkAccelMap
;;; 
;;; Signals
;;; 
;;;   "changed"                                        : Has Details
;;; 
;;; Description
;;; 
;;; Accelerator maps are used to define runtime configurable accelerators.
;;; Functions for manipulating them are are usually used by higher level
;;; convenience mechanisms like GtkUIManager and are thus considered
;;; "low-level". You'll want to use them if you're manually creating menus
;;; that should have user-configurable accelerators.
;;; 
;;; Accelerator is uniquely defined by:
;;; 
;;;     * accelerator path
;;;     * accelerator key
;;;     * accelerator modifiers
;;; 
;;; The accelerator path must consist of
;;; "<WINDOWTYPE>/Category1/Category2/.../Action", where WINDOWTYPE should be
;;; a unique application-specific identifier that corresponds to the kind of
;;; window the accelerator is being used in, e.g. "Gimp-Image",
;;; "Abiword-Document" or "Gnumeric-Settings". The "Category1/.../Action"
;;; portion is most appropriately chosen by the action the accelerator triggers,
;;; i.e. for accelerators on menu items, choose the item's menu path, e.g.
;;; "File/Save As", "Image/View/Zoom" or "Edit/Select All". So a full valid
;;; accelerator path may look like:
;;; "<Gimp-Toolbox>/File/Dialogs/Tool Options...".
;;; 
;;; All accelerators are stored inside one global GtkAccelMap that can be
;;; obtained using gtk_accel_map_get(). See Monitoring changes for additional
;;; details.
;;; 
;;; Manipulating accelerators
;;; 
;;; New accelerators can be added using gtk_accel_map_add_entry(). To search for
;;; specific accelerator, use gtk_accel_map_lookup_entry(). Modifications of
;;; existing accelerators should be done using gtk_accel_map_change_entry().
;;; 
;;; In order to avoid having some accelerators changed, they can be locked using
;;; gtk_accel_map_lock_path(). Unlocking is done using
;;; gtk_accel_map_unlock_path().
;;; 
;;; Saving and loading accelerator maps
;;; 
;;; Accelerator maps can be saved to and loaded from some external resource. For
;;; simple saving and loading from file, gtk_accel_map_save() and
;;; gtk_accel_map_load() are provided. Saving and loading can also be done by
;;; providing file descriptor to gtk_accel_map_save_fd() and
;;; gtk_accel_map_load_fd().
;;; 
;;; Monitoring changes
;;; 
;;; GtkAccelMap object is only useful for monitoring changes of accelerators.
;;; By connecting to "changed" signal, one can monitor changes of all
;;; accelerators. It is also possible to monitor only single accelerator path
;;; by using it as a detail of the "changed" signal.
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;; 
;;; void user_function (GtkAccelMap    *object,
;;;                     gchar          *accel_path,
;;;                     guint           accel_key,
;;;                     GdkModifierType accel_mods,
;;;                     gpointer        user_data)       : Has Details
;;; 
;;; Notifies of a change in the global accelerator map. The path is also used
;;; as the detail for the signal, so it is possible to connect to
;;; changed::accel_path.
;;; 
;;; object :
;;; 	the global accel map object
;;; 
;;; accel_path :
;;; 	the path of the accelerator that changed
;;; 
;;; accel_key :
;;; 	the key value for the new accelerator
;;; 
;;; accel_mods :
;;; 	the modifier mask for the new accelerator
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccelMap
;;; 
;;; typedef struct _GtkAccelMap GtkAccelMap;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelMap" gtk-accel-map
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_accel_map_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkAccelMapForeach ()
;;; 
;;; void (*GtkAccelMapForeach) (gpointer data,
;;;                             const gchar *accel_path,
;;;                             guint accel_key,
;;;                             GdkModifierType accel_mods,
;;;                             gboolean changed);
;;; 
;;; data :
;;; 	User data passed to gtk_accel_map_foreach() or
;;;     gtk_accel_map_foreach_unfiltered()
;;; 
;;; accel_path :
;;; 	Accel path of the current accelerator
;;; 
;;; accel_key :
;;; 	Key of the current accelerator
;;; 
;;; accel_mods :
;;; 	Modifiers of the current accelerator
;;; 
;;; changed :
;;; 	Changed flag of the accelerator (if TRUE, accelerator has changed
;;;     during runtime and would need to be saved during an accelerator dump)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_entry ()
;;; 
;;; void gtk_accel_map_add_entry (const gchar *accel_path,
;;;                               guint accel_key,
;;;                               GdkModifierType accel_mods);
;;; 
;;; Registers a new accelerator with the global accelerator map. This function
;;; should only be called once per accel_path with the canonical accel_key and
;;; accel_mods for this path. To change the accelerator during runtime
;;; programatically, use gtk_accel_map_change_entry().
;;; 
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first with
;;; g_intern_static_string().
;;; 
;;; accel_path :
;;; 	valid accelerator path
;;; 
;;; accel_key :
;;; 	the accelerator key
;;; 
;;; accel_mods :
;;; 	the accelerator modifiers
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lookup_entry ()
;;; 
;;; gboolean gtk_accel_map_lookup_entry (const gchar *accel_path,
;;;                                      GtkAccelKey *key);
;;; 
;;; Looks up the accelerator entry for accel_path and fills in key.
;;; 
;;; accel_path :
;;; 	a valid accelerator path
;;; 
;;; key :
;;; 	the accelerator key to be filled in (optional).
;;; 
;;; Returns :
;;; 	TRUE if accel_path is known, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_change_entry ()
;;; 
;;; gboolean gtk_accel_map_change_entry (const gchar *accel_path,
;;;                                      guint accel_key,
;;;                                      GdkModifierType accel_mods,
;;;                                      gboolean replace);
;;; 
;;; Changes the accel_key and accel_mods currently associated with accel_path.
;;; Due to conflicts with other accelerators, a change may not always be
;;; possible, replace indicates whether other accelerators may be deleted to
;;; resolve such conflicts. A change will only occur if all conflicts could be
;;; resolved (which might not be the case if conflicting accelerators are
;;; locked). Successful changes are indicated by a TRUE return value.
;;; 
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first with
;;; g_intern_static_string().
;;; 
;;; accel_path :
;;; 	a valid accelerator path
;;; 
;;; accel_key :
;;; 	the new accelerator key
;;; 
;;; accel_mods :
;;; 	the new accelerator modifiers
;;; 
;;; replace :
;;; 	TRUE if other accelerators may be deleted upon conflicts
;;; 
;;; Returns :
;;; 	TRUE if the accelerator could be changed, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load ()
;;; 
;;; void gtk_accel_map_load (const gchar *file_name);
;;; 
;;; Parses a file previously saved with gtk_accel_map_save() for accelerator
;;; specifications, and propagates them accordingly.
;;; 
;;; file_name :
;;; 	a file containing accelerator specifications, in the GLib file name
;;;     encoding.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save ()
;;; 
;;; void gtk_accel_map_save (const gchar *file_name);
;;; 
;;; Saves current accelerator specifications (accelerator path, key and
;;; modifiers) to file_name. The file is written in a format suitable to be
;;; read back in by gtk_accel_map_load().
;;; 
;;; file_name :
;;; 	the name of the file to contain accelerator specifications, in the GLib
;;;     file name encoding.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach ()
;;; 
;;; void gtk_accel_map_foreach (gpointer data, GtkAccelMapForeach foreach_func)
;;; 
;;; Loops over the entries in the accelerator map whose accel path doesn't match
;;; any of the filters added with gtk_accel_map_add_filter(), and execute
;;; foreach_func on each. The signature of foreach_func is that of
;;; GtkAccelMapForeach, the changed parameter indicates whether this accelerator
;;; was changed during runtime (thus, would need saving during an accelerator
;;; map dump).
;;; 
;;; data :
;;; 	data to be passed into foreach_func. [allow-none]
;;; 
;;; foreach_func :
;;; 	function to be executed for each accel map entry which is not filtered
;;;     out.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_fd ()
;;; 
;;; void gtk_accel_map_load_fd (gint fd);
;;; 
;;; Filedescriptor variant of gtk_accel_map_load().
;;; 
;;; Note that the file descriptor will not be closed by this function.
;;; 
;;; fd :
;;; 	a valid readable file descriptor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save_fd ()
;;; 
;;; void gtk_accel_map_save_fd (gint fd);
;;; 
;;; Filedescriptor variant of gtk_accel_map_save().
;;; 
;;; Note that the file descriptor will not be closed by this function.
;;; 
;;; fd :
;;; 	a valid writable file descriptor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_scanner ()
;;; 
;;; void gtk_accel_map_load_scanner (GScanner *scanner);
;;; 
;;; GScanner variant of gtk_accel_map_load().
;;; 
;;; scanner :
;;; 	a GScanner which has already been provided with an input file
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_filter ()
;;; 
;;; void gtk_accel_map_add_filter (const gchar *filter_pattern);
;;; 
;;; Adds a filter to the global list of accel path filters.
;;; 
;;; Accel map entries whose accel path matches one of the filters are skipped
;;; by gtk_accel_map_foreach().
;;; 
;;; This function is intended for GTK+ modules that create their own menus, but
;;; don't want them to be saved into the applications accelerator map dump.
;;; 
;;; filter_pattern :
;;; 	a pattern (see GPatternSpec)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach_unfiltered ()
;;; 
;;; void gtk_accel_map_foreach_unfiltered (gpointer data,
;;;                                        GtkAccelMapForeach foreach_func);
;;; 
;;; Loops over all entries in the accelerator map, and execute foreach_func on
;;; each. The signature of foreach_func is that of GtkAccelMapForeach, the
;;; changed parameter indicates whether this accelerator was changed during
;;; runtime (thus, would need saving during an accelerator map dump).
;;; 
;;; data :
;;; 	data to be passed into foreach_func
;;; 
;;; foreach_func :
;;; 	function to be executed for each accel map entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_get ()
;;; 
;;; GtkAccelMap * gtk_accel_map_get (void);
;;; 
;;; Gets the singleton global GtkAccelMap object. This object is useful only
;;; for notification of changes to the accelerator map via the ::changed signal;
;;; it isn't a parameter to the other accelerator map functions.
;;; 
;;; Returns :
;;; 	the global GtkAccelMap object.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lock_path ()
;;; 
;;; void gtk_accel_map_lock_path (const gchar *accel_path);
;;; 
;;; Locks the given accelerator path. If the accelerator map doesn't yet contain
;;; an entry for accel_path, a new one is created.
;;; 
;;; Locking an accelerator path prevents its accelerator from being changed
;;; during runtime. A locked accelerator path can be unlocked by
;;; gtk_accel_map_unlock_path(). Refer to gtk_accel_map_change_entry() for
;;; information about runtime accelerator changes.
;;; 
;;; If called more than once, accel_path remains locked until
;;; gtk_accel_map_unlock_path() has been called an equivalent number of times.
;;; 
;;; Note that locking of individual accelerator paths is independent from
;;; locking the GtkAccelGroup containing them. For runtime accelerator changes
;;; to be possible both the accelerator path and its GtkAccelGroup have to be
;;; unlocked.
;;; 
;;; accel_path :
;;; 	a valid accelerator path
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_unlock_path ()
;;; 
;;; void gtk_accel_map_unlock_path (const gchar *accel_path);
;;; 
;;; Undoes the last call to gtk_accel_map_lock_path() on this accel_path. Refer
;;; to gtk_accel_map_lock_path() for information about accelerator path locking.
;;; 
;;; accel_path :
;;; 	a valid accelerator path
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.accel.map.lisp -----------------------------------------

