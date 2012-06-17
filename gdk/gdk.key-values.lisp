;;; ----------------------------------------------------------------------------
;;; gdk.key-values.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
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
;;; Key Values
;;; 
;;; Functions for manipulating keyboard codes
;;; 
;;; Synopsis
;;; 
;;;     GdkKeymap
;;;     GdkKeymapKey
;;;
;;;     gdk_keymap_get_default
;;;     gdk_keymap_get_for_display
;;;     gdk_keymap_lookup_key
;;;     gdk_keymap_translate_keyboard_state
;;;     gdk_keymap_get_entries_for_keyval
;;;     gdk_keymap_get_entries_for_keycode
;;;     gdk_keymap_get_direction
;;;     gdk_keymap_have_bidi_layouts
;;;     gdk_keymap_get_caps_lock_state
;;;     gdk_keymap_add_virtual_modifiers
;;;     gdk_keymap_map_virtual_modifiers
;;; 
;;;     gdk_keyval_name
;;;     gdk_keyval_from_name
;;; 
;;;     gdk_keyval_convert_case
;;;     gdk_keyval_to_upper
;;;     gdk_keyval_to_lower
;;;     gdk_keyval_is_upper
;;;     gdk_keyval_is_lower
;;; 
;;;     gdk_keyval_to_unicode
;;;     gdk_unicode_to_keyval
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkKeymap
;;; 
;;; Signals
;;; 
;;;   "direction-changed"                              : Run Last
;;;   "keys-changed"                                   : Run Last
;;;   "state-changed"                                  : Run Last
;;; 
;;; Description
;;; 
;;; Key values are the codes which are sent whenever a key is pressed or
;;; released. They appear in the keyval field of the GdkEventKey structure,
;;; which is passed to signal handlers for the "key-press-event" and
;;; "key-release-event" signals. The complete list of key values can be found
;;; in the <gdk/gdkkeysyms.h> header file. <gdk/gdkkeysyms.h> is not included
;;; in <gdk/gdk.h>, it must be included independently, because the file is
;;; quite large.
;;; 
;;; Key values are regularly updated from the upstream X.org X11 implementation,
;;; so new values are added regularly. They will be prefixed with GDK_ rather
;;; than XF86XK_ or XK_ (for older symbols).
;;; 
;;; Key values can be converted into a string representation using
;;; gdk_keyval_name(). The reverse function, converting a string to a key value,
;;; is provided by gdk_keyval_from_name().
;;; 
;;; The case of key values can be determined using gdk_keyval_is_upper() and
;;; gdk_keyval_is_lower(). Key values can be converted to upper or lower case
;;; using gdk_keyval_to_upper() and gdk_keyval_to_lower().
;;; 
;;; When it makes sense, key values can be converted to and from Unicode
;;; characters with gdk_keyval_to_unicode() and gdk_unicode_to_keyval().
;;; 
;;; One GdkKeymap object exists for each user display. gdk_keymap_get_default()
;;; returns the GdkKeymap for the default display; to obtain keymaps for other
;;; displays, use gdk_keymap_get_for_display(). A keymap is a mapping from
;;; GdkKeymapKey to key values. You can think of a GdkKeymapKey as a
;;; representation of a symbol printed on a physical keyboard key. That is, it
;;; contains three pieces of information. First, it contains the hardware
;;; keycode; this is an identifying number for a physical key. Second, it
;;; contains the level of the key. The level indicates which symbol on the key
;;; will be used, in a vertical direction. So on a standard US keyboard, the
;;; key with the number "1" on it also has the exclamation point ("!") character
;;; on it. The level indicates whether to use the "1" or the "!" symbol. The
;;; letter keys are considered to have a lowercase letter at level 0, and an
;;; uppercase letter at level 1, though only the uppercase letter is printed.
;;; Third, the GdkKeymapKey contains a group; groups are not used on standard
;;; US keyboards, but are used in many other countries. On a keyboard with
;;; groups, there can be 3 or 4 symbols printed on a single key. The group
;;; indicates movement in a horizontal direction. Usually groups are used for
;;; two different languages. In group 0, a key might have two English
;;; characters, and in group 1 it might have two Hebrew characters. The Hebrew
;;; characters will be printed on the key next to the English characters.
;;; 
;;; In order to use a keymap to interpret a key event, it's necessary to first
;;; convert the keyboard state into an effective group and level. This is done
;;; via a set of rules that varies widely according to type of keyboard and
;;; user configuration. The function gdk_keymap_translate_keyboard_state()
;;; accepts a keyboard state -- consisting of hardware keycode pressed, active
;;; modifiers, and active group -- applies the appropriate rules, and returns
;;; the group/level to be used to index the keymap, along with the modifiers
;;; which did not affect the group and level. i.e. it returns "unconsumed
;;; modifiers." The keyboard group may differ from the effective group used for
;;; keymap lookups because some keys don't have multiple groups - e.g. the Enter
;;; key is always in group 0 regardless of keyboard state.
;;; 
;;; Note that gdk_keymap_translate_keyboard_state() also returns the keyval,
;;; i.e. it goes ahead and performs the keymap lookup in addition to telling
;;; you which effective group/level values were used for the lookup. GdkEventKey
;;; already contains this keyval, however, so you don't normally need to call
;;; gdk_keymap_translate_keyboard_state() just to get the keyval.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "direction-changed" signal
;;; 
;;; void user_function (GdkKeymap *keymap, gpointer   user_data)      : Run Last
;;; 
;;; The ::direction-changed signal gets emitted when the direction of the
;;; keymap changes.
;;; 
;;; keymap :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "keys-changed" signal
;;; 
;;; void user_function (GdkKeymap *keymap, gpointer   user_data)      : Run Last
;;; 
;;; The ::keys-changed signal is emitted when the mapping represented by keymap
;;; changes.
;;; 
;;; keymap :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "state-changed" signal
;;; 
;;; void user_function (GdkKeymap *keymap, gpointer   user_data)      : Run Last
;;; 
;;; The ::state-changed signal is emitted when the state of the keyboard
;;; changes, e.g when Caps Lock is turned on or off.
;;; See gdk_keymap_get_caps_lock_state().
;;; 
;;; keymap :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkKeymap
;;; 
;;; struct GdkKeymap;
;;; 
;;; A GdkKeymap defines the translation from keyboard state (including a
;;; hardware key, a modifier mask, and active keyboard group) to a keyval. This
;;; translation has two phases. The first phase is to determine the effective
;;; keyboard group and level for the keyboard state; the second phase is to
;;; look up the keycode/group/level triplet in the keymap and see what keyval
;;; it corresponds to.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkKeymap" gdk-keymap
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_keymap_get_type")
  ((:cffi direction
          gdk-keymap-direction pango:pango-direction
          "gdk_keymap_get_direction" nil)
   (:cffi has-bidi-layouts
          gdk-keymap-has-bidi-layouts :boolean
          "gdk_keymap_have_bidi_layouts" nil)
   (:cffi caps-lock-state
          gdk-keymap-caps-lock-state :boolean
          "gdk_keymap_get_caps_lock_state" nil)))
          
;;; ----------------------------------------------------------------------------
;;; struct GdkKeymapKey
;;; 
;;; struct GdkKeymapKey {
;;;   guint keycode;
;;;   gint  group;
;;;   gint  level;
;;; };
;;; 
;;; A GdkKeymapKey is a hardware key that can be mapped to a keyval.
;;; 
;;; guint keycode;
;;;     the hardware keycode. This is an identifying number for a physical key.
;;; 
;;; gint group;
;;;     indicates movement in a horizontal direction. Usually groups are used
;;;     for two different languages. In group 0, a key might have two English
;;;     characters, and in group 1 it might have two Hebrew characters. The
;;;     Hebrew characters will be printed on the key next to the English
;;;     characters.
;;; 
;;; gint level;
;;;     indicates which symbol on the key will be used, in a vertical direction.
;;;     So on a standard US keyboard, the key with the number "1" on it also has
;;;     the exclamation point ("!") character on it. The level indicates whether
;;;     to use the "1" or the "!" symbol. The letter keys are considered to have
;;;     a lowercase letter at level 0, and an uppercase letter at level 1,
;;;     though only the uppercase letter is printed.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-keymap-key nil
  (keycode :uint :initform 0)
  (group :int :initform 0)
  (level :int :initform 0))
  
;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_default ()
;;; 
;;; GdkKeymap * gdk_keymap_get_default (void);
;;; 
;;; Returns the GdkKeymap attached to the default display.
;;; 
;;; Returns :
;;;     the GdkKeymap attached to the default display.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_default" gdk-keymap-get-default)
    (g-object gdk-keymap))

(export 'gdk-keymap-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_for_display ()
;;; 
;;; GdkKeymap * gdk_keymap_get_for_display (GdkDisplay *display);
;;; 
;;; Returns the GdkKeymap attached to display.
;;; 
;;; display :
;;;     the GdkDisplay.
;;; 
;;; Returns :
;;;     the GdkKeymap attached to display.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_for_display" gdk-keymap-get-for-display)
    (g-object gdk-keymap)
  (display (g-object gdk-display)))

(export 'gdk-keymap-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_lookup_key ()
;;; 
;;; guint gdk_keymap_lookup_key (GdkKeymap *keymap, const GdkKeymapKey *key);
;;; 
;;; Looks up the keyval mapped to a keycode/group/level triplet. If no keyval
;;; is bound to key, returns 0. For normal user input, you want to use
;;; gdk_keymap_translate_keyboard_state() instead of this function, since the
;;; effective group/level may not be the same as the current keyboard state.
;;; 
;;; keymap :
;;;     a GdkKeymap or NULL to use the default keymap
;;; 
;;; key :
;;;     a GdkKeymapKey with keycode, group, and level initialized
;;; 
;;; Returns :
;;;     a keyval, or 0 if none was mapped to the given key
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_lookup_key" %gdk-keymap-lookup-key) :uint
  (keymap (g-object gdk-keymap))
  (key (g-boxed-foreign gdk-keymap-key)))

(export 'gdk-keymap-lookup-key)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_translate_keyboard_state ()
;;; 
;;; gboolean gdk_keymap_translate_keyboard_state
;;;                                        (GdkKeymap *keymap,
;;;                                         guint hardware_keycode,
;;;                                         GdkModifierType state,
;;;                                         gint group,
;;;                                         guint *keyval,
;;;                                         gint *effective_group,
;;;                                         gint *level,
;;;                                         GdkModifierType *consumed_modifiers);
;;; 
;;; Translates the contents of a GdkEventKey into a keyval, effective group, and
;;; level. Modifiers that affected the translation and are thus unavailable for
;;; application use are returned in consumed_modifiers. See the section called
;;; “Description” for an explanation of groups and levels. The effective_group
;;; is the group that was actually used for the translation; some keys such as
;;; Enter are not affected by the active keyboard group. The level is derived
;;; from state. For convenience, GdkEventKey already contains the translated
;;; keyval, so this function isn't as useful as you might think.
;;; 
;;; Note
;;; 
;;; consumed_modifiers gives modifiers that should be masked out from state
;;; when comparing this key press to a hot key. For instance, on a US keyboard,
;;; the plus symbol is shifted, so when comparing a key press to a 
;;; <Control>plus accelerator <Shift> should be masked out.
;;; 
;;; /* We want to ignore irrelevant modifiers like ScrollLock */
;;; #define ALL_ACCELS_MASK (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_MOD1_MASK)
;;; gdk_keymap_translate_keyboard_state (keymap, event->hardware_keycode,
;;;                                      event->state, event->group,
;;;                                      &keyval, NULL, NULL, &consumed);
;;; if (keyval == GDK_PLUS &&
;;;     (event->state & ~consumed & ALL_ACCELS_MASK) == GDK_CONTROL_MASK)
;;;   /* Control was pressed */
;;; 
;;; An older interpretation consumed_modifiers was that it contained all
;;; modifiers that might affect the translation of the key; this allowed
;;; accelerators to be stored with irrelevant consumed modifiers, by doing:
;;; 
;;; /* XXX Don't do this XXX */
;;; if (keyval == accel_keyval &&
;;;    (event->state & ~consumed & ALL_ACCELS_MASK) == (accel_mods & ~consumed))
;;;   /* Accelerator was pressed */
;;; 
;;; However, this did not work if multi-modifier combinations were used in the
;;; keymap, since, for instance, <Control> would be masked out even if only
;;; <Control><Alt> was used in the keymap. To support this usage as well as well
;;; as possible, all single modifier combinations that could affect the key for
;;; any combination of modifiers will be returned in consumed_modifiers;
;;; multi-modifier combinations are returned only when actually found in state.
;;; When you store accelerators, you should always store them with consumed
;;; modifiers removed. Store <Control>plus, not <Control><Shift>plus,
;;; 
;;; Note that passing NULL for keymap is deprecated and will stop to work in
;;; GTK+ 3.0. Use gdk_keymap_get_for_display() instead.
;;; 
;;; keymap :
;;;     a GdkKeymap, or NULL to use the default.
;;; 
;;; hardware_keycode :
;;;     a keycode
;;; 
;;; state :
;;;     a modifier state
;;; 
;;; group :
;;;     active keyboard group
;;; 
;;; keyval :
;;;     return location for keyval, or NULL.
;;; 
;;; effective_group :
;;;     return location for effective group, or NULL.
;;; 
;;; level :
;;;     return location for level, or NULL.
;;; 
;;; consumed_modifiers :
;;;     return location for modifiers that were used to determine the group
;;;     or level, or NULL.
;;; 
;;; Returns :
;;;     TRUE if there was a keyval bound to the keycode/state/group
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_translate_keyboard_state"
          %gdk-keymap-translate-keyboard-state) :boolean
  (keymap (g-object gdk-keymap))
  (hardware-keycode :uint)
  (state gdk-modifier-type)
  (group :int)
  (keyval (:pointer :uint))
  (effective-group (:pointer :int))
  (level (:pointer :int))
  (consumed-modifiers (:pointer gdk-modifier-type)))

(defun keymap-translate-keyboard-state (keymap hardware-keycode state group)
  (with-foreign-objects ((keyval :uint)
                         (effective-group :int)
                         (level :int)
                         (modifiers 'gdk-modifier-type))
    (if (%gdk-keymap-translate-keyboard-state keymap
                                              hardware-keycode
                                              state
                                              group
                                              keyval
                                              effective-group
                                              level
                                              modifiers)
        (values (mem-ref keyval :uint)
                (mem-ref effective-group :int)
                (mem-ref level :int)
                (mem-ref modifiers 'gdk-modifier-type)))))

(export 'gdk-keymap-translate-keyboard-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keyval ()
;;; 
;;; gboolean gdk_keymap_get_entries_for_keyval (GdkKeymap *keymap,
;;;                                             guint keyval,
;;;                                             GdkKeymapKey **keys,
;;;                                             gint *n_keys);
;;; 
;;; Obtains a list of keycode/group/level combinations that will generate
;;; keyval. Groups and levels are two kinds of keyboard mode; in general, the
;;; level determines whether the top or bottom symbol on a key is used, and the
;;; group determines whether the left or right symbol is used. On US keyboards,
;;; the shift key changes the keyboard level, and there are no groups. A group
;;; switch key might convert a keyboard between Hebrew to English modes, for
;;; example. GdkEventKey contains a group field that indicates the active
;;; keyboard group. The level is computed from the modifier mask. The returned
;;; array should be freed with g_free().
;;; 
;;; keymap :
;;;     a GdkKeymap, or NULL to use the default keymap
;;; 
;;; keyval :
;;;     a keyval, such as GDK_a, GDK_Up, GDK_Return, etc.
;;; 
;;; keys :
;;;     return location for an array of GdkKeymapKey
;;; 
;;; n_keys :
;;;     return location for number of elements in returned array
;;; 
;;; Returns :
;;;     TRUE if keys were found and returned
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_entries_for_keyval"
          %gdk-keymap-get-entries-for-keyval) :boolean
  (keymap (g-object gdk-keymap))
  (keyval :uint)
  (keys (:pointer (:pointer gdk-keymap-key-cstruct)))
  (n-keys (:pointer :int)))

(defun gdk-keymap-get-entries-for-keyval (keymap keyval)
  (with-foreign-objects ((keys :pointer) (n-keys :int))
    (when (%gdk-keymap-get-entries-for-keyval keymap keyval keys n-keys)
      (let ((keys (mem-ref keys :pointer))
            (n-keys (mem-ref n-keys :int)))
        (prog1
          (iter (for i from 0 below n-keys)
                (for keymap-key = 
                  (convert-from-foreign
                    (inc-pointer
                             keys
                             (* i
                                (foreign-type-size 'gdk-keymap-key-cstruct)))
                    '(g-boxed-foreign gdk-keymap-key)))
                (collect keymap-key))
          (g-free keys))))))

(export 'gdk-keymap-get-entries-for-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keycode ()
;;; 
;;; gboolean gdk_keymap_get_entries_for_keycode (GdkKeymap *keymap,
;;;                                              guint hardware_keycode,
;;;                                              GdkKeymapKey **keys,
;;;                                              guint **keyvals,
;;;                                              gint *n_entries);
;;; 
;;; Returns the keyvals bound to hardware_keycode. The Nth GdkKeymapKey in keys
;;; is bound to the Nth keyval in keyvals. Free the returned arrays with
;;; g_free(). When a keycode is pressed by the user, the keyval from this list
;;; of entries is selected by considering the effective keyboard group and
;;; level. See gdk_keymap_translate_keyboard_state().
;;; 
;;; keymap :
;;;     a GdkKeymap or NULL to use the default keymap
;;; 
;;; hardware_keycode :
;;;     a keycode
;;; 
;;; keys :
;;;     return location for array of GdkKeymapKey, or NULL
;;; 
;;; keyvals :
;;;     return location for array of keyvals, or NULL
;;; 
;;; n_entries :
;;;     length of keys and keyvals
;;; 
;;; Returns :
;;;     TRUE if there were any entries
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_entries_for_keycode"
          %gdk-keymap-get-entries-for-keycode) :boolean
  (keymap (g-object gdk-keymap))
  (hardware-keycode :uint)
  (keys (:pointer (:pointer gdk-keymap-key-cstruct)))
  (keyvals (:pointer (:pointer :uint)))
  (n-entries (:pointer :int)))

(defun gdk-keymap-get-entries-for-keycode (keymap hardware-keycode)
  (with-foreign-objects ((keys :pointer) (keyvals :pointer) (n-keys :int))
    (when (%gdk-keymap-get-entries-for-keycode keymap
                                               hardware-keycode
                                               keys
                                               keyvals
                                               n-keys)
      (let ((keys (mem-ref keys :pointer))
            (keyvals (mem-ref keyvals :pointer))
            (n-keys (mem-ref n-keys :int)))
        (prog1
          (iter (for i from 0 below n-keys)
                (for keyval = (mem-aref keyvals :uint))
                (for keymap-key =
                  (convert-from-foreign
                    (inc-pointer
                             keys
                             (* i
                                (foreign-type-size 'gdk-keymap-key-cstruct)))
                    '(g-boxed-foreign gdk-keymap-key)))
                (collect keymap-key into r-keys)
                (collect keyval into r-keyvals)
                (finally (return (values r-keys r-keyvals))))
          (g-free keys)
          (g-free keyvals))))))

(export 'gdk-keymap-get-entries-for-keymap)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_direction ()
;;; 
;;; PangoDirection gdk_keymap_get_direction (GdkKeymap *keymap);
;;; 
;;; Returns the direction of effective layout of the keymap.
;;; 
;;; Note that passing NULL for keymap is deprecated and will stop to work in
;;; GTK+ 3.0. Use gdk_keymap_get_for_display() instead.
;;; 
;;; Returns the direction of the keymap.
;;; 
;;; keymap :
;;;     a GdkKeymap or NULL to use the default keymap
;;; 
;;; Returns :
;;;     PANGO_DIRECTION_LTR or PANGO_DIRECTION_RTL if it can determine the
;;;     direction. PANGO_DIRECTION_NEUTRAL otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_have_bidi_layouts ()
;;; 
;;; gboolean gdk_keymap_have_bidi_layouts (GdkKeymap *keymap);
;;; 
;;; Determines if keyboard layouts for both right-to-left and left-to-right
;;; languages are in use.
;;; 
;;; Note that passing NULL for keymap is deprecated and will stop to work in
;;; GTK+ 3.0. Use gdk_keymap_get_for_display() instead.
;;; 
;;; keymap :
;;;     a GdkKeymap or NULL to use the default keymap
;;; 
;;; Returns :
;;;     TRUE if there are layouts in both directions, FALSE otherwise
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_caps_lock_state ()
;;; 
;;; gboolean gdk_keymap_get_caps_lock_state (GdkKeymap *keymap);
;;; 
;;; Returns whether the Caps Lock modifer is locked.
;;; 
;;; keymap :
;;;     a GdkKeymap
;;; 
;;; Returns :
;;;     TRUE if Caps Lock is on
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_add_virtual_modifiers ()
;;; 
;;; void gdk_keymap_add_virtual_modifiers (GdkKeymap *keymap,
;;;                                        GdkModifierType *state);
;;; 
;;; Adds virtual modifiers (i.e. Super, Hyper and Meta) which correspond to the
;;; real modifiers (i.e Mod2, Mod3, ...) in modifiers. are set in state to their
;;; non-virtual counterparts (i.e. Mod2, Mod3,...) and set the corresponding
;;; bits in state.
;;; 
;;; GDK already does this before delivering key events, but for compatibility
;;; reasons, it only sets the first virtual modifier it finds, whereas this
;;; function sets all matching virtual modifiers.
;;; 
;;; This function is useful when matching key events against accelerators.
;;; 
;;; keymap :
;;;     a GdkKeymap
;;; 
;;; state :
;;;     pointer to the modifier mask to change
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_map_virtual_modifiers ()
;;; 
;;; gboolean gdk_keymap_map_virtual_modifiers (GdkKeymap *keymap,
;;;                                            GdkModifierType *state);
;;; 
;;; Maps the virtual modifiers (i.e. Super, Hyper and Meta) which are set in
;;; state to their non-virtual counterparts (i.e. Mod2, Mod3,...) and set the
;;; corresponding bits in state.
;;; 
;;; This function is useful when matching key events against accelerators.
;;; 
;;; keymap :
;;;     a GdkKeymap
;;; 
;;; state :
;;;     pointer to the modifier state to map
;;; 
;;; Returns :
;;;     TRUE if no virtual modifiers were mapped to the same non-virtual
;;;     modifier. Note that FALSE is also returned if a virtual modifier is
;;;     mapped to a non-virtual modifier that was already set in state.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_name ()
;;; 
;;; gchar * gdk_keyval_name (guint keyval);
;;; 
;;; Converts a key value into a symbolic name.
;;; 
;;; The names are the same as those in the <gdk/gdkkeysyms.h> header file but
;;; without the leading "GDK_KEY_".
;;; 
;;; Converts a key value into a symbolic name. The names are the same as those
;;; in the <gdk/gdkkeysyms.h> header file but without the leading "GDK_".
;;; 
;;; keyval :
;;;     a key value
;;; 
;;; Returns :
;;;     a string containing the name of the key, or NULL if keyval is not a
;;;     valid key. The string should not be modified.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_name" gdk-keyval-name) (:string :free-from-foreign nil)
  (keyval :uint))

(export 'gdk-keyval-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_from_name ()
;;; 
;;; guint gdk_keyval_from_name (const gchar *keyval_name);
;;; 
;;; Converts a key name to a key value.
;;; 
;;; The names are the same as those in the <gdk/gdkkeysyms.h> header file but
;;; without the leading "GDK_KEY_".
;;; 
;;; Converts a key name to a key value.
;;; 
;;; keyval_name :
;;;     a key name
;;; 
;;; Returns :
;;;     the corresponding key value, or GDK_KEY_VoidSymbol if the key name
;;;     is not a valid key
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_from_name" gdk-keyval-from-name) :uint
  (keyval-name :string))

(export 'gdk-keyval-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_convert_case ()
;;; 
;;; void gdk_keyval_convert_case (guint symbol, guint *lower, guint *upper);
;;; 
;;; Obtains the upper- and lower-case versions of the keyval symbol.
;;; Examples of keyvals are GDK_a, GDK_Enter, GDK_F1, etc.
;;; 
;;; symbol :
;;;     a keyval
;;; 
;;; lower :
;;;     return location for lowercase version of symbol
;;; 
;;; upper :
;;;     return location for uppercase version of symbol
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_upper ()
;;; 
;;; guint gdk_keyval_to_upper (guint keyval);
;;; 
;;; Converts a key value to upper case, if applicable.
;;; 
;;; keyval :
;;;     a key value.
;;; 
;;; Returns :
;;;     the upper case form of keyval, or keyval itself if it is already in
;;;     upper case or it is not subject to case conversion.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_to_upper" gdk-keyval-to-upper) :uint
  (keyval :uint))

(export 'gdk-keyval-to-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_lower ()
;;; 
;;; guint gdk_keyval_to_lower (guint keyval);
;;; 
;;; Converts a key value to lower case, if applicable.
;;; 
;;; keyval :
;;;     a key value.
;;; 
;;; Returns :
;;;     the lower case form of keyval, or keyval itself if it is already in
;;;     lower case or it is not subject to case conversion.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_to_lower" gdk-keyval-to-lower) :uint
  (keyval :uint))

(export 'gdk-keyval-to-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_upper ()
;;; 
;;; gboolean gdk_keyval_is_upper (guint keyval);
;;; 
;;; Returns TRUE if the given key value is in upper case.
;;; 
;;; keyval :
;;;     a key value.
;;; 
;;; Returns :
;;;     TRUE if keyval is in upper case, or if keyval is not subject to
;;;     case conversion.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_is_upper" gdk-keyval-is-upper) :boolean
  (keyval :uint))

(export 'gdk-keyval-is-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_lower ()
;;; 
;;; gboolean gdk_keyval_is_lower (guint keyval);
;;; 
;;; Returns TRUE if the given key value is in lower case.
;;; 
;;; keyval :
;;;     a key value.
;;; 
;;; Returns :
;;;     TRUE if keyval is in lower case, or if keyval is not subject
;;;     to case conversion.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_is_lower" gdk-keyval-is-lower) :boolean
  (keyval :uint))

(export 'gdk-keyval-is-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_unicode ()
;;; 
;;; guint32 gdk_keyval_to_unicode (guint keyval);
;;; 
;;; Convert from a GDK key symbol to the corresponding ISO10646 (Unicode)
;;; character.
;;; 
;;; keyval :
;;;     a GDK key symbol
;;; 
;;; Returns :
;;;     the corresponding unicode character, or 0 if there is no corresponding
;;;     character.
;;; ----------------------------------------------------------------------------

(define-foreign-type unichar ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod translate-from-foreign (value (type unichar))
  (code-char value))

(defmethod translate-to-foreign (value (type unichar))
  (char-code value))

;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_to_unicode" gdk-keyval-to-unicode) unichar
  (keyval :uint))

(export 'gdk-keyval-to-unicode)

;;; ----------------------------------------------------------------------------
;;; gdk_unicode_to_keyval ()
;;; 
;;; guint gdk_unicode_to_keyval (guint32 wc);
;;; 
;;; Convert from a ISO10646 character to a key symbol.
;;; 
;;; wc :
;;;     a ISO10646 encoded character
;;; 
;;; Returns :
;;;     the corresponding GDK key symbol, if one exists. or, if there is no
;;;     corresponding symbol, wc | 0x01000000
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_unicode_to_keyval" gdk-unicode-to-keyval) :uint
  (char unichar))

(export 'gdk-unicode-to-keyval)

;;; --- End of file gdk.key-values.lisp ----------------------------------------
