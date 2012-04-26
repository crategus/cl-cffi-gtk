;;; ----------------------------------------------------------------------------
;;; gtk.accel-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.4. See http://www.gtk.org.
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
;;; Accelerator Groups
;;; 
;;; Groups of global keyboard accelerators for an entire GtkWindow
;;;     
;;; Synopsis
;;; 
;;;     GtkAccelGroup
;;;
;;;     gtk_accel_group_new
;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;     gtk_accel_group_disconnect
;;;     gtk_accel_group_disconnect_key
;;;     gtk_accel_group_query
;;;     gtk_accel_group_activate
;;;     gtk_accel_group_lock
;;;     gtk_accel_group_unlock
;;;     gtk_accel_group_get_is_locked
;;;     gtk_accel_group_from_accel_closure
;;;     gtk_accel_group_get_modifier_mask
;;;     gtk_accel_groups_activate
;;;     gtk_accel_groups_from_object
;;;     gtk_accel_group_find
;;;
;;;     GtkAccelKey
;;;
;;;     gtk_accelerator_valid
;;;     gtk_accelerator_parse
;;;     gtk_accelerator_name
;;;     gtk_accelerator_get_label
;;;     gtk_accelerator_set_default_mod_mask
;;;     gtk_accelerator_get_default_mod_mask
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkAccelGroup
;;; 
;;; Properties
;;; 
;;;   "is-locked"                gboolean              : Read
;;;   "modifier-mask"            GdkModifierType       : Read
;;; 
;;; Signals
;;; 
;;;   "accel-activate"                                 : Has Details
;;;   "accel-changed"                                  : Has Details
;;; 
;;; Description
;;; 
;;; A GtkAccelGroup represents a group of keyboard accelerators, typically
;;; attached to a toplevel GtkWindow (with gtk_window_add_accel_group()).
;;; Usually you won't need to create a GtkAccelGroup directly; instead, when
;;; using GtkUIManager, GTK+ automatically sets up the accelerators for your
;;; menus in the ui manager's GtkAccelGroup.
;;; 
;;; Note that accelerators are different from mnemonics. Accelerators are
;;; shortcuts for activating a menu item; they appear alongside the menu item
;;; they're a shortcut for. For example "Ctrl+Q" might appear alongside the
;;; "Quit" menu item. Mnemonics are shortcuts for GUI elements such as text
;;; entries or buttons; they appear as underlined characters. See
;;; gtk_label_new_with_mnemonic(). Menu items can have both accelerators and
;;; mnemonics, of course.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-locked" property
;;; 
;;;   "is-locked"                gboolean              : Read
;;; 
;;; Is the accel group locked.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "modifier-mask" property
;;; 
;;;   "modifier-mask"            GdkModifierType       : Read
;;; 
;;; Modifier Mask.
;;; 
;;; Default value: GDK_SHIFT_MASK | GDK_CONTROL_MASK | GDK_MOD1_MASK |
;;;                GDK_SUPER_MASK | GDK_HYPER_MASK   |GDK_META_MASK
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-activate" signal
;;; 
;;; gboolean user_function (GtkAccelGroup  *accel_group,
;;;                         GObject        *acceleratable,
;;;                         guint           keyval,
;;;                         GdkModifierType modifier,
;;;                         gpointer        user_data)          : Has Details
;;; 
;;; The accel-activate signal is an implementation detail of GtkAccelGroup and
;;; not meant to be used by applications.
;;; 
;;; accel_group :
;;;     the GtkAccelGroup which received the signal
;;; 
;;; acceleratable :
;;;     the object on which the accelerator was activated
;;; 
;;; keyval :
;;;     the accelerator keyval
;;; 
;;; modifier :
;;;     the modifier combination of the accelerator
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the accelerator was activated
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-changed" signal
;;; 
;;; void user_function (GtkAccelGroup  *accel_group,
;;;                     guint           keyval,
;;;                     GdkModifierType modifier,
;;;                     GClosure       *accel_closure,
;;;                     gpointer        user_data)          : Has Details
;;; 
;;; The accel-changed signal is emitted when a GtkAccelGroupEntry is added to
;;; or removed from the accel group.
;;; 
;;; Widgets like GtkAccelLabel which display an associated accelerator should
;;; connect to this signal, and rebuild their visual representation if the
;;; accel_closure is theirs.
;;; 
;;; accel_group :
;;;     the GtkAccelGroup which received the signal
;;; 
;;; keyval :
;;;     the accelerator keyval
;;; 
;;; modifier :
;;;     the modifier combination of the accelerator
;;; 
;;; accel_closure :
;;;     the GClosure of the accelerator
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelGroup
;;; 
;;; struct GtkAccelGroup;
;;;
;;; An object representing and maintaining a group of accelerators.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelGroup" gtk-accel-group
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_accel_group_get_type")
  ((is-locked
    gtk-accel-group-is-locked
    "is-locked" "gboolean" t nil)
   (modifier-mask
    gtk-accel-group-modifier-mask
    "modifier-mask" "GdkModifierType" t nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_new ()
;;; 
;;; GtkAccelGroup * gtk_accel_group_new (void);
;;; 
;;; Creates a new GtkAccelGroup.
;;; 
;;; Returns :
;;;     a new GtkAccelGroup object
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-accel-group-new))

(defun gtk-accel-group-new ()
  (make-instance 'gtk-accel-group))

(export 'gtk-accel-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_connect ()
;;; 
;;; void gtk_accel_group_connect (GtkAccelGroup *accel_group,
;;;                               guint accel_key,
;;;                               GdkModifierType accel_mods,
;;;                               GtkAccelFlags accel_flags,
;;;                               GClosure *closure);
;;; 
;;; Installs an accelerator in this group. When accel_group is being activated
;;; in response to a call to gtk_accel_groups_activate(), closure will be
;;; invoked if the accel_key and accel_mods from gtk_accel_groups_activate()
;;; match those of this connection.
;;; 
;;; The signature used for the closure is that of GtkAccelGroupActivate.
;;; 
;;; Note that, due to implementation details, a single closure can only be
;;; connected to one accelerator group.
;;; 
;;; accel_group :
;;;     the accelerator group to install an accelerator in
;;; 
;;; accel_key :
;;;     key value of the accelerator
;;; 
;;; accel_mods :
;;;     modifier combination of the accelerator
;;; 
;;; accel_flags :
;;;     a flag mask to configure this accelerator
;;; 
;;; closure :
;;;     closure to be executed upon accelerator activation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_connect_by_path ()
;;; 
;;; void gtk_accel_group_connect_by_path (GtkAccelGroup *accel_group,
;;;                                       const gchar *accel_path,
;;;                                       GClosure *closure);
;;; 
;;; Installs an accelerator in this group, using an accelerator path to look up
;;; the appropriate key and modifiers (see gtk_accel_map_add_entry()). When
;;; accel_group is being activated in response to a call to
;;; gtk_accel_groups_activate(), closure will be invoked if the accel_key and
;;; accel_mods from gtk_accel_groups_activate() match the key and modifiers for
;;; the path.
;;; 
;;; The signature used for the closure is that of GtkAccelGroupActivate.
;;; 
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first with
;;; g_intern_static_string().
;;; 
;;; accel_group :
;;;     the accelerator group to install an accelerator in
;;; 
;;; accel_path :
;;;     path used for determining key and modifiers
;;; 
;;; closure :
;;;     closure to be executed upon accelerator activation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkAccelGroupActivate ()
;;; 
;;; gboolean (*GtkAccelGroupActivate) (GtkAccelGroup *accel_group,
;;;                                    GObject *acceleratable,
;;;                                    guint keyval,
;;;                                    GdkModifierType modifier);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkAccelGroupFindFunc ()
;;; 
;;; gboolean (*GtkAccelGroupFindFunc) (GtkAccelKey *key,
;;;                                    GClosure *closure,
;;;                                    gpointer data);
;;; 
;;; data :
;;;     . [closure]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_disconnect ()
;;; 
;;; gboolean gtk_accel_group_disconnect (GtkAccelGroup *accel_group,
;;;                                      GClosure *closure);
;;; 
;;; Removes an accelerator previously installed through
;;; gtk_accel_group_connect().
;;; 
;;; Since 2.20 closure can be NULL.
;;; 
;;; accel_group :
;;;     the accelerator group to remove an accelerator from
;;; 
;;; closure :
;;;     the closure to remove from this accelerator group, or NULL to remove
;;;     all closures
;;; 
;;; Returns :
;;;     TRUE if the closure was found and got disconnected
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_disconnect_key ()
;;; 
;;; gboolean gtk_accel_group_disconnect_key (GtkAccelGroup *accel_group,
;;;                                          guint accel_key,
;;;                                          GdkModifierType accel_mods);
;;; 
;;; Removes an accelerator previously installed through
;;; gtk_accel_group_connect().
;;; 
;;; accel_group :
;;;     the accelerator group to install an accelerator in
;;; 
;;; accel_key :
;;;     key value of the accelerator
;;; 
;;; accel_mods :
;;;     modifier combination of the accelerator
;;; 
;;; Returns :
;;;     TRUE if there was an accelerator which could be removed, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_query ()
;;; 
;;; GtkAccelGroupEntry * gtk_accel_group_query (GtkAccelGroup *accel_group,
;;;                                             guint accel_key,
;;;                                             GdkModifierType accel_mods,
;;;                                             guint *n_entries);
;;; 
;;; Queries an accelerator group for all entries matching accel_key and
;;; accel_mods.
;;; 
;;; accel_group :
;;;     the accelerator group to query
;;; 
;;; accel_key :
;;;     key value of the accelerator
;;; 
;;; accel_mods :
;;;     modifier combination of the accelerator
;;; 
;;; n_entries :
;;;     location to return the number of entries found, or NULL
;;; 
;;; Returns :
;;;     an array of n_entries GtkAccelGroupEntry elements, or NULL. The array
;;;     is owned by GTK+ and must not be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_activate ()
;;; 
;;; gboolean gtk_accel_group_activate (GtkAccelGroup *accel_group,
;;;                                    GQuark accel_quark,
;;;                                    GObject *acceleratable,
;;;                                    guint accel_key,
;;;                                    GdkModifierType accel_mods);
;;; 
;;; Finds the first accelerator in accel_group that matches accel_key and
;;; accel_mods, and activates it.
;;; 
;;; accel_group :
;;;     a GtkAccelGroup
;;; 
;;; accel_quark :
;;;     the quark for the accelerator name
;;; 
;;; acceleratable :
;;;     the GObject, usually a GtkWindow, on which to activate the accelerator
;;; 
;;; accel_key :
;;;     accelerator keyval from a key event
;;; 
;;; accel_mods :
;;;     keyboard state mask from a key event
;;; 
;;; Returns :
;;;     TRUE if an accelerator was activated and handled this keypress
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_lock ()
;;; 
;;; void gtk_accel_group_lock (GtkAccelGroup *accel_group);
;;; 
;;; Locks the given accelerator group.
;;; 
;;; Locking an acelerator group prevents the accelerators contained within it
;;; to be changed during runtime. Refer to gtk_accel_map_change_entry() about
;;; runtime accelerator changes.
;;; 
;;; If called more than once, accel_group remains locked until
;;; gtk_accel_group_unlock() has been called an equivalent number of times.
;;; 
;;; accel_group :
;;;     a GtkAccelGroup
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_unlock ()
;;; 
;;; void gtk_accel_group_unlock (GtkAccelGroup *accel_group);
;;; 
;;; Undoes the last call to gtk_accel_group_lock() on this accel_group.
;;; 
;;; accel_group :
;;;     a GtkAccelGroup
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_get_is_locked ()
;;; 
;;; gboolean gtk_accel_group_get_is_locked (GtkAccelGroup *accel_group);
;;; 
;;; Locks are added and removed using gtk_accel_group_lock() and
;;; gtk_accel_group_unlock().
;;; 
;;; accel_group :
;;;     a GtkAccelGroup
;;; 
;;; Returns :
;;;     TRUE if there are 1 or more locks on the accel_group, FALSE otherwise.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_from_accel_closure ()
;;; 
;;; GtkAccelGroup * gtk_accel_group_from_accel_closure  (GClosure *closure);
;;; 
;;; Finds the GtkAccelGroup to which closure is connected;
;;; see gtk_accel_group_connect().
;;; 
;;; closure :
;;;     a GClosure
;;; 
;;; Returns :
;;;     the GtkAccelGroup to which closure is connected, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_get_modifier_mask ()
;;; 
;;; GdkModifierType gtk_accel_group_get_modifier_mask
;;;                                                 (GtkAccelGroup *accel_group)
;;; 
;;; Gets a GdkModifierType representing the mask for this accel_group. For
;;; example, GDK_CONTROL_MASK, GDK_SHIFT_MASK, etc.
;;; 
;;; accel_group :
;;;     a GtkAccelGroup
;;; 
;;; Returns :
;;;     the modifier mask for this accel group
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_groups_activate ()
;;; 
;;; gboolean gtk_accel_groups_activate (GObject *object,
;;;                                     guint accel_key,
;;;                                     GdkModifierType accel_mods);
;;; 
;;; Finds the first accelerator in any GtkAccelGroup attached to object that
;;; matches accel_key and accel_mods, and activates that accelerator.
;;; 
;;; object :
;;;     the GObject, usually a GtkWindow, on which to activate the accelerator
;;; 
;;; accel_key :
;;;     accelerator keyval from a key event
;;; 
;;; accel_mods :
;;;     keyboard state mask from a key event
;;; 
;;; Returns :
;;;     TRUE if an accelerator was activated and handled this keypress
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_groups_from_object ()
;;; 
;;; GSList * gtk_accel_groups_from_object (GObject *object);
;;; 
;;; Gets a list of all accel groups which are attached to object.
;;; 
;;; object :
;;;     a GObject, usually a GtkWindow
;;; 
;;; Returns :
;;;     a list of all accel groups which are attached to object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_find ()
;;; 
;;; GtkAccelKey * gtk_accel_group_find (GtkAccelGroup *accel_group,
;;;                                     GtkAccelGroupFindFunc find_func,
;;;                                     gpointer data);
;;; 
;;; Finds the first entry in an accelerator group for which find_func returns
;;; TRUE and returns its GtkAccelKey.
;;; 
;;; accel_group :
;;;     a GtkAccelGroup
;;; 
;;; find_func :
;;;     a function to filter the entries of accel_group with
;;; 
;;; data :
;;;     data to pass to find_func
;;; 
;;; Returns :
;;;     the key of the first entry passing find_func. The key is owned by GTK+
;;;     and must not be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelKey
;;; 
;;; struct GtkAccelKey {
;;;   guint           accel_key;
;;;   GdkModifierType accel_mods;
;;;   guint           accel_flags : 16;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_valid ()
;;; 
;;; gboolean gtk_accelerator_valid (guint keyval, GdkModifierType modifiers);
;;; 
;;; Determines whether a given keyval and modifier mask constitute a valid
;;; keyboard accelerator. For example, the GDK_KEY_a keyval plus
;;; GDK_CONTROL_MASK is valid - this is a "Ctrl+a" accelerator. But, you can't,
;;; for instance, use the GDK_KEY_Control_L keyval as an accelerator.
;;; 
;;; keyval :
;;;     a GDK keyval
;;; 
;;; modifiers :
;;;     modifier mask
;;; 
;;; Returns :
;;;     TRUE if the accelerator is valid
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_parse ()
;;; 
;;; void gtk_accelerator_parse (const gchar *accelerator,
;;;                             guint *accelerator_key,
;;;                             GdkModifierType *accelerator_mods);
;;; 
;;; Parses a string representing an accelerator. The format looks like
;;; "<Control>a" or "<Shift><Alt>F1" or "<Release>z" (the last one is for
;;; key release).
;;; 
;;; The parser is fairly liberal and allows lower or upper case, and also
;;; abbreviations such as "<Ctl>" and "<Ctrl>". Key names are parsed using
;;; gdk_keyval_from_name(). For character keys the name is not the symbol, but
;;; the lowercase name, e.g. one would use "<Ctrl>minus" instead of "<Ctrl>-".
;;; 
;;; If the parse fails, accelerator_key and accelerator_mods will be set to 0
;;; (zero).
;;; 
;;; accelerator :
;;;     string representing an accelerator
;;; 
;;; accelerator_key :
;;;     return location for accelerator keyval, or NULL
;;; 
;;; accelerator_mods :
;;;     return location for accelerator modifier mask, NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_name ()
;;; 
;;; gchar * gtk_accelerator_name (guint accelerator_key,
;;;                               GdkModifierType accelerator_mods);
;;; 
;;; Converts an accelerator keyval and modifier mask into a string parseable
;;; by gtk_accelerator_parse(). For example, if you pass in GDK_KEY_q and
;;; GDK_CONTROL_MASK, this function returns "<Control>q".
;;; 
;;; If you need to display accelerators in the user interface, see
;;; gtk_accelerator_get_label().
;;; 
;;; accelerator_key :
;;;     accelerator keyval
;;; 
;;; accelerator_mods :
;;;     accelerator modifier mask
;;; 
;;; Returns :
;;;     a newly-allocated accelerator name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_label ()
;;; 
;;; gchar * gtk_accelerator_get_label (guint accelerator_key,
;;;                                    GdkModifierType accelerator_mods);
;;; 
;;; Converts an accelerator keyval and modifier mask into a string which can be
;;; used to represent the accelerator to the user.
;;; 
;;; accelerator_key :
;;;     accelerator keyval
;;; 
;;; accelerator_mods :
;;;     accelerator modifier mask
;;; 
;;; Returns :
;;;     a newly-allocated string representing the accelerator
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_set_default_mod_mask ()
;;; 
;;; void gtk_accelerator_set_default_mod_mask (GdkModifierType default_mod_mask)
;;; 
;;; Sets the modifiers that will be considered significant for keyboard
;;; accelerators. The default mod mask is GDK_CONTROL_MASK | GDK_SHIFT_MASK |
;;; GDK_MOD1_MASK | GDK_SUPER_MASK | GDK_HYPER_MASK | GDK_META_MASK, that is,
;;; Control, Shift, Alt, Super, Hyper and Meta. Other modifiers will by default
;;; be ignored by GtkAccelGroup. You must include at least the three modifiers
;;; Control, Shift and Alt in any value you pass to this function.
;;; 
;;; The default mod mask should be changed on application startup, before using
;;; any accelerator groups.
;;; 
;;; default_mod_mask :
;;;     accelerator modifier mask
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_default_mod_mask ()
;;; 
;;; GdkModifierType gtk_accelerator_get_default_mod_mask (void);
;;; 
;;; Gets the value set by gtk_accelerator_set_default_mod_mask().
;;; 
;;; Returns :
;;;     the default accelerator modifier mask
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.accel-group.lisp ---------------------------------------
