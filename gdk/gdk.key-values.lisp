;;; ----------------------------------------------------------------------------
;;; gdk.key-values.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;;     gdk_keymap_get_num_lock_state
;;;     gdk_keymap_get_modifier_state
;;;     gdk_keymap_add_virtual_modifiers
;;;     gdk_keymap_map_virtual_modifiers
;;;     gdk_keymap_get_modifier_mask
;;;
;;;     gdk_keyval_name
;;;     gdk_keyval_from_name
;;;     gdk_keyval_convert_case
;;;     gdk_keyval_to_upper
;;;     gdk_keyval_to_lower
;;;     gdk_keyval_is_upper
;;;     gdk_keyval_is_lower
;;;     gdk_keyval_to_unicode
;;;     gdk_unicode_to_keyval
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkKeymap
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkKeymap" gdk-keymap
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_keymap_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-keymap 'type)
 "@version{2013-6-13}
  @begin{short}
    A @sym{gdk-keymap} defines the translation from keyboard state (including a
    hardware key, a modifier mask, and active keyboard group) to a keyval. This
    translation has two phases. The first phase is to determine the effective
    keyboard group and level for the keyboard state; the second phase is to look
    up the keycode/group/level triplet in the keymap and see what keyval it
    corresponds to.
  @end{short}
  Key values are the codes which are sent whenever a key is pressed or
  released. They appear in the @code{keyval} field of the @class{gdk-event-key}
  structure, which is passed to signal handlers for the \"key-press-event\" and
  \"key-release-event\" signals. The complete list of key values can be found in
  the @code{<gdk/gdkkeysyms.h>} header file.

  Key values are regularly updated from the upstream X.org X11 implementation,
  so new values are added regularly. They will be prefixed with @code{GDK_KEY_}
  rather than @code{XF86XK_} or @code{XK_} (for older symbols).

  Key values can be converted into a string representation using the
  @fun{gdk-keyval-name} function. The reverse function, converting a string to
  a key value, is provided by the @fun{gdk-keyval-from-name}.

  The case of key values can be determined using the functions
  @fun{gdk-keyval-is-upper} and @fun{gdk-keyval-is-lower}. Key values can be
  converted to upper or lower case using the functions @fun{gdk-keyval-to-upper}
  and @fun{gdk-keyval-to-lower}.

  When it makes sense, key values can be converted to and from Unicode
  characters with the functions @fun{gdk-keyval-to-unicode} and
  @fun{gdk-unicode-to-keyval}.

  One @sym{gdk-keymap} object exists for each user display. The
  @fun{gdk-keymap-get-default} functions returns the GdkKeymap for the default
  display; to obtain keymaps for other displays, use the
  @fun{gdk-keymap-get-for-display} function. A keymap is a mapping from
  @class{gdk-keymap-key} to key values. You can think of a
  @class{gdk-keymap-key} as a representation of a symbol printed on a physical
  keyboard key. That is, it contains three pieces of information. First, it
  contains the hardware keycode; this is an identifying number for a physical
  key. Second, it contains the level of the key. The level indicates which
  symbol on the key will be used, in a vertical direction. So on a standard US
  keyboard, the key with the number \"1\" on it also has the exclamation point
  (\"!\") character on it. The level indicates whether to use the \"1\" or the
  \"!\" symbol. The letter keys are considered to have a lowercase letter at
  level 0, and an uppercase letter at level 1, though only the uppercase letter
  is printed. Third, the @class{gdk-keymap-key} contains a group; groups are
  not used on standard US keyboards, but are used in many other countries. On a
  keyboard with groups, there can be 3 or 4 symbols printed on a single key. The
  group indicates movement in a horizontal direction. Usually groups are used
  for two different languages. In group 0, a key might have two English
  characters, and in group 1 it might have two Hebrew characters. The Hebrew
  characters will be printed on the key next to the English characters.

  In order to use a keymap to interpret a key event, it is necessary to first
  convert the keyboard state into an effective group and level. This is done
  via a set of rules that varies widely according to type of keyboard and user
  configuration. The function @fun{gdk-keymap-translate-keyboard-state} accepts
  a keyboard state - consisting of hardware keycode pressed, active modifiers,
  and active group - applies the appropriate rules, and returns the
  group/level to be used to index the keymap, along with the modifiers which
  did not affect the group and level. i. e. it returns \"unconsumed modifiers.\"
  The keyboard group may differ from the effective group used for keymap
  lookups because some keys do not have multiple groups - e. g. the Enter key is
  always in group 0 regardless of keyboard state.

  Note that the function @fun{gdk-keymap-translate-keyboard-state} also returns
  the keyval, i. e. it goes ahead and performs the keymap lookup in addition to
  telling you which effective group/level values were used for the lookup.
  @class{gdk-event-key} already contains this keyval, however, so you do not
  normally need to call the function @fun{gdk-keymap-translate-keyboard-state}
  just to get the keyval.
  @begin[Signal Details]{dictionary}
    @subheading{The \"direction-changed\" signal}
      @begin{pre}
 lambda (keymap)   : Run Last
      @end{pre}
      The \"direction-changed\" signal gets emitted when the direction of the
      keymap changes.
      @begin[code]{table}
        @entry[keymap]{The object on which the signal is emitted.}
      @end{table}
      Since 2.0

    @subheading{The \"keys-changed\" signal}
      @begin{pre}
 lambda (keymap)   : Run Last
      @end{pre}
      The \"keys-changed\" signal is emitted when the mapping represented by
      keymap changes.
      @begin[code]{table}
        @entry[keymap]{The object on which the signal is emitted.}
      @end{table}
      Since 2.2

    @subheading{The \"state-changed\" signal}
      @begin{pre}
 lambda (keymap)   : Run Last
      @end{pre}
      The \"state-changed\" signal is emitted when the state of the keyboard
      changes, e. g when Caps Lock is turned on or off. See the function
      @fun{gdk-keymap-get-caps-lock-state}.
      @begin[code]{table}
        @entry[keymap]{The object on which the signal is emitted.}
      @end{table}
      Since 2.16
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; struct GdkKeymapKey
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-keymap-key nil
  (keycode :uint :initform 0)
  (group :int :initform 0)
  (level :int :initform 0))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-keymap-key 'type)
 "@version{2013-6-13}
  @begin{short}
    A @sym{gdk-keymap-key} is a hardware key that can be mapped to a keyval.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gdk-keymap-key nil
  (keycode :uint :initform 0)
  (group :int :initform 0)
  (level :int :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[keycode]{The hardware keycode. This is an identifying number for a
      physical key.}
    @entry[group]{Indicates movement in a horizontal direction. Usually groups
      are used for two different languages. In group 0, a key might have two
      English characters, and in group 1 it might have two Hebrew characters.
      The Hebrew characters will be printed on the key next to the English
      characters.}
    @entry[level]{Indicates which symbol on the key will be used, in a vertical
      direction. So on a standard US keyboard, the key with the number \"1\" on
      it also has the exclamation point (\"!\") character on it. The level
      indicates whether to use the \"1\" or the \"!\" symbol. The letter keys
      are considered to have a lowercase letter at level 0, and an uppercase
      letter at level 1, though only the uppercase letter is printed.}
  @end{table}
  @see-constructor{copy-gdk-keymap-key}
  @see-constructor{make-gdk-keymap-key}
  @see-slot{gdk-keymap-key-keycode}
  @see-slot{gdk-keymap-key-group}
  @see-slot{gdk-keymap-key-level}")

(export (boxed-related-symbols 'gdk-keymap-key))

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for GdkKeymapKey
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-keymap-key 'function)
 "@version{2013-9-26}
  @argument[instance]{a @class{gdk-keymap-key} structure}
  Copy constructor of a @class{gdk-keymap-key} structure.
  @see-class{gdk-keymap-key}
  @see-function{make-gdk-keymap-key}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-keymap-key 'function)
 "@version{2013-11-29}
  Creates a @class{gdk-keymap-key} structure.
  @see-class{gdk-keymap-key}
  @see-function{copy-gdk-geometry}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkKeymapKey structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key-keycode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-keymap-key-keycode 'function)
 "@version{2013-11-29}
  Accessor of the slot @code{keycode} of the @class{gdk-keymap} structure.
  @see-class{gdk-keymap-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-keymap-key-group 'function)
 "@version{2013-11-29}
  Accessor of the slot @code{group} of the @class{gdk-keymap} structure.
  @see-class{gdk-keymap-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key-level atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-keymap-key-level 'function)
 "@version{2013-11-29}
  Accessor of the slot @code{level} of the @class{gdk-keymap} structure.
  @see-class{gdk-keymap-key}")

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_default" gdk-keymap-get-default)
    (g-object gdk-keymap)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @return{The @class{gdk-keymap} attached to the default display.}
  Returns the @class{gdk-keymap} attached to the default display.")

(export 'gdk-keymap-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_for_display" gdk-keymap-get-for-display)
    (g-object gdk-keymap)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[display]{the @class{gdk-display} object}
  @return{The @class{gdk-keymap} attached to @arg{display}.}
  @short{Returns the @class{gdk-keymap} attached to @arg{display}.}

  Since 2.2"
  (display (g-object gdk-display)))

(export 'gdk-keymap-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_lookup_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_lookup_key" gdk-keymap-lookup-key) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-9-26}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[key]{a @class{gdk-keymap-key} with keycode, group, and level
    initialized}
  @return{A keyval, or 0 if none was mapped to the given key.}
  @begin{short}
    Looks up the keyval mapped to a keycode/group/level triplet.
  @end{short}
  If no keyval is bound to @arg{key}, returns 0. For normal user input, you want
  to use the function @fun{gdk-keymap-translate-keyboard-state} instead of this
  function, since the effective group/level may not be the same as the current
  keyboard state.
  @see-class{gdk-keymap}
  @see-class{gdk-keymap-key}
  @see-function{gdk-keymap-translate-keyboard-state}"
  (keymap (g-object gdk-keymap))
  (key (g-boxed-foreign gdk-keymap-key)))

(export 'gdk-keymap-lookup-key)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_translate_keyboard_state ()
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

(defun gdk-keymap-translate-keyboard-state (keymap hardware-keycode state group)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[hardware-keycode]{a keycode}
  @argument[state]{a modifier state}
  @argument[group]{active keyboard group}
  @begin{return}
    @code{keyval} -- keyval, or NULL @br{}
    @code{effective-group} -- effective group, or NULL @br{}
    @code{level} -- level, or NULL @br{}
    @code{consumed-modifiers} -- modifiers that were used to determine the
      group or level, or NULL
  @end{return}
  @begin{short}
    Translates the contents of a @class{gdk-event-key} into a keyval, effective
    group, and level.
  @end{short}
  Modifiers that affected the translation and are thus unavailable for
  application use are returned in @arg{consumed-modifiers}. See the section
  called 'Description' for an explanation of groups and levels. The
  @arg{effective-group} is the group that was actually used for the translation;
  some keys such as Enter are not affected by the active keyboard group. The
  level is derived from state. For convenience, @class{gdk-event-key} already
  contains the translated keyval, so this function is not as useful as you might
  think.

  @subheading{Note}
    @arg{consumed-modifiers} gives modifiers that should be masked out from
    state when comparing this key press to a hot key. For instance, on a US
    keyboard, the plus symbol is shifted, so when comparing a key press to a
    <Control>plus accelerator <Shift> should be masked out.
    @begin{pre}
   /* We want to ignore irrelevant modifiers like ScrollLock */
   #define ALL_ACCELS_MASK (GDK_CONTROL_MASK | GDK_SHIFT_MASK |
                            GDK_MOD1_MASK)
   gdk_keymap_translate_keyboard_state (keymap, event->hardware_keycode,
                                        event->state, event->group,
                                        &keyval, NULL, NULL, &consumed);
   if (keyval == GDK_PLUS &&
       (event->state & ~consumed & ALL_ACCELS_MASK) == GDK_CONTROL_MASK)
     /* Control was pressed */
    @end{pre}
    An older interpretation consumed_modifiers was that it contained all
    modifiers that might affect the translation of the key; this allowed
    accelerators to be stored with irrelevant consumed modifiers, by doing:
    @begin{pre}
   /* XXX Don't do this XXX */
   if (keyval == accel_keyval &&
       (event->state & ~consumed & ALL_ACCELS_MASK) ==
                                                    (accel_mods & ~consumed))
     /* Accelerator was pressed */
    @end{pre}
    However, this did not work if multi-modifier combinations were used in the
    keymap, since, for instance, <Control> would be masked out even if only
    <Control><Alt> was used in the keymap. To support this usage as well as well
    as possible, all single modifier combinations that could affect the key for
    any combination of modifiers will be returned in consumed_modifiers;
    multi-modifier combinations are returned only when actually found in state.
    When you store accelerators, you should always store them with consumed
    modifiers removed. Store <Control>plus, not <Control><Shift>plus."
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_entries_for_keyval"
          %gdk-keymap-get-entries-for-keyval) :boolean
  (keymap (g-object gdk-keymap))
  (keyval :uint)
  (keys (:pointer (:pointer (:struct gdk-keymap-key-cstruct))))
  (n-keys (:pointer :int)))

(defun gdk-keymap-get-entries-for-keyval (keymap keyval)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[keyval]{a keyval, such as GDK_a, GDK_Up, GDK_Return, etc.}
  @begin{return}
    @code{keys} -- a list of @class{gdk-keymap-key} @br{}
  @end{return}
  @begin{short}
    Obtains a list of keycode/group/level combinations that will generate
    keyval.
  @end{short}
  Groups and levels are two kinds of keyboard mode; in general, the level
  determines whether the top or bottom symbol on a key is used, and the
  group determines whether the left or right symbol is used. On US keyboards,
  the shift key changes the keyboard level, and there are no groups. A group
  switch key might convert a keyboard between Hebrew to English modes, for
  example. @class{gdk-event-key} contains a group field that indicates the
  active keyboard group. The level is computed from the modifier mask."
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
                         (foreign-type-size '(:struct gdk-keymap-key-cstruct))))
                    '(g-boxed-foreign gdk-keymap-key)))
                (collect keymap-key))
          (g-free keys))))))

(export 'gdk-keymap-get-entries-for-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keycode ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_entries_for_keycode"
          %gdk-keymap-get-entries-for-keycode) :boolean
  (keymap (g-object gdk-keymap))
  (hardware-keycode :uint)
  (keys (:pointer (:pointer (:struct gdk-keymap-key-cstruct))))
  (keyvals (:pointer (:pointer :uint)))
  (n-entries (:pointer :int)))

(defun gdk-keymap-get-entries-for-keycode (keymap hardware-keycode)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[hardware-keycode]{a keycode}
  @begin{return}
    @code{keys} -- list of @class{gdk-keymap-key}, or @code{nil} @br{}
    @code{keyvals} --list of keyvals, or @code{nil} @br{}
  @end{return}
  @begin{short}
    Returns the keyvals bound to @arg{hardware-keycode}.
  @end{short}
  The Nth @class{gdk-keymap-key} in keys is bound to the Nth keyval in keyvals.
  When a keycode is pressed by the user, the keyval from this list
  of entries is selected by considering the effective keyboard group and
  level. See the function @fun{gdk-keymap-translate-keyboard-state}."
  (with-foreign-objects ((keys :pointer) (keyvals :pointer) (n-keys :int))
    (when (%gdk-keymap-get-entries-for-keycode keymap
                                               hardware-keycode
                                               keys
                                               keyvals
                                               n-keys)
      (let ((keys (mem-ref keys :pointer))
            (keyvals (mem-ref keyvals :pointer))
            (n-keys (mem-ref n-keys :int)))
        (iter (for i from 0 below n-keys)
              (for keyval = (mem-aref keyvals :uint i))
              (for keymap-key =
                (convert-from-foreign
                  (inc-pointer
                    keys
                    (* i
                       (foreign-type-size '(:struct gdk-keymap-key-cstruct))))
                  '(g-boxed-foreign gdk-keymap-key)))
              (collect keymap-key into r-keys)
              (collect keyval into r-keyvals)
              (finally
                (g-free keys)
                (g-free keyvals)
                (return (values r-keys r-keyvals))))))))

(export 'gdk-keymap-get-entries-for-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_direction ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_direction" gdk-keymap-get-direction) pango-direction
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @begin{return}
    @code{:ltr} or @code{:rtl} if it can determine the direction.
    @code{:neutral} otherwise.
  @end{return}
  Returns the direction of effective layout of the @arg{keymap}."
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-get-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_have_bidi_layouts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_have_bidi_layouts" gdk-keymap-have-bidi-layouts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if there are layouts in both directions,
    @code{nil} otherwise.}
  @begin{short}
    Determines if keyboard layouts for both right-to-left and left-to-right
    languages are in use.
  @end{short}

  Since 2.12"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-have-bidi-layouts)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_caps_lock_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_caps_lock_state" gdk-keymap-get-caps-lock-state)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if Caps Lock is on.}
  @short{Returns whether the Caps Lock modifer is locked.}

  Since 2.16"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-get-caps-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_num_lock_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_num_lock_state" gdk-keymap-get-num-lock-state)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if Num Lock is on.}
  @short{Returns whether the Num Lock modifer is locked.}

  Since 3.0"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-get-num-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_modifier_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_modifier_state" gdk-keymap-get-modifier-state)
    gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{The current modifier state.}
  @short{Returns the current modifier state.}

  Since 3.4"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-get-modifier-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_add_virtual_modifiers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_add_virtual_modifiers" gdk-keymap-add-virtual-modifiers)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[state]{pointer to the modifier mask to change}
  @begin{short}
    Adds virtual modifiers (i. e. Super, Hyper and Meta) which correspond to the
    real modifiers (i. e Mod2, Mod3, ...) in modifiers and set the corresponding
    bits in state.
  @end{short}

  GDK already does this before delivering key events, but for compatibility
  reasons, it only sets the first virtual modifier it finds, whereas this
  function sets all matching virtual modifiers.

  This function is useful when matching key events against accelerators.

  Since 2.20"
  (keymap (g-object gdk-keymap))
  (state gdk-modifier-type))

(export 'gdk-keymap-add-virtual-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_map_virtual_modifiers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_map_virtual_modifiers" gdk-keymap-map-virtual-modifiers)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[state]{pointer to the modifier state to map}
  @begin{return}
    @em{True} if no virtual modifiers were mapped to the same non-virtual
    modifier. Note that @code{nil} is also returned if a virtual modifier is
    mapped to a non-virtual modifier that was already set in state.
  @end{return}
  @begin{short}
    Maps the virtual modifiers (i. e. Super, Hyper and Meta) which are set in
    state to their non-virtual counterparts (i. e. Mod2, Mod3,...) and set the
    corresponding bits in state.
  @end{short}

  This function is useful when matching key events against accelerators.

  Since 2.20"
  (keymap (g-object gdk-keymap))
  (state gdk-modifier-type))

(export 'gdk-keymap-map-virtual-modifiers)

;;; ----------------------------------------------------------------------------
;;; Enum GdkModifierIntent
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkModifierIntent" gdk-modifier-intent
  (:export t
   :type-initializer "gdk_modifier_intent_get_type")
  (:primary-accelerator 0)
  (:context-menu 1)
  (:extend-selection 2)
  (:modify-selection 3)
  (:no-text-input 4)
  (:shift-group 5)
  (:default-mod-mask 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-modifier-intent atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-modifier-intent atdoc:*external-symbols*)
 "@version{2013-9-26}
  @begin{short}
    This enum is used with the functions @fun{gdk-keymap-get-modifier-mask} in
    order to determine what modifiers the currently used windowing system
    backend uses for particular purposes.
  @end{short}
  For example, on X11/Windows, the Control key is used for invoking menu
  shortcuts (accelerators), whereas on Apple computers it is the Command key,
  which correspond to the values @code{:control-mask} and @code{:mod2-mask}
  of the @symbol{gdk-modifier-type} enumeration, respectively.
  @begin{pre}
(define-g-enum \"GdkModifierIntent\" gdk-modifier-intent
  (:export t
   :type-initializer \"gdk_modifier_intent_get_type\")
  (:primary-accelerator 0)
  (:context-menu 1)
  (:extend-selection 2)
  (:modify-selection 3)
  (:no-text-input 4)
  (:shift-group 5)
  (:default-mod-mask 6))
  @end{pre}
  @begin[code]{table}
    @entry[primary-accelerator]{The primary modifier used to invoke menu
      accelerators.}
    @entry[:context-menu]{The modifier used to invoke context menus. Note that
      mouse button 3 always triggers context menus. When this modifier is not
      0, it additionally triggers context menus when used with mouse button 1.}
    @entry[:extend-selection]{The modifier used to extend selections using
      <modifier>-click or <modifier>-cursor-key.}
    @entry[:modify-selection]{The modifier used to modify selections, which in
      most cases means toggling the clicked item into or out of the selection.}
    @entry[:no-text-input]{When any of these modifiers is pressed, the key event
      cannot produce a symbol directly. This is meant to be used for input
      methods, and for use cases like typeahead search.}
    @entry[:shift-group]{The modifier that switches between keyboard groups
      (AltGr on X11/Windows and Option/Alt on OS X).}
    @entry[:default-mod-mask]{The set of modifier masks accepted
      as modifiers in accelerators. Needed because Command is mapped to MOD2 on
      OSX, which is widely used, but on X11 MOD2 is NumLock and using that for a
      mod key is problematic at best.
      Ref: https://bugzilla.gnome.org/show_bug.cgi?id=736125.}
  @end{table}
  Since 3.4
  @see-symbol{gdk-modifier-type}
  @see-function{gdk-keymap-get-modifier-mask}")

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_modifier_mask ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_modifier_mask" gdk-keymap-get-modifier-mask)
   gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2013-9-26}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[intent]{the use case for the modifier mask}
  @return{The modifier mask used for @arg{intent}.}
  @begin{short}
    Returns the modifier mask the keymap's windowing system backend uses for a
    particular purpose.
  @end{short}

  Note that this function always returns real hardware modifiers, not virtual
  ones (e. g. it will return @code{:mod1-mask} rather than @code{:meta-mask} if
  the backend maps @code{MOD1} to @code{META}), so there are use cases where
  the return value of this function has to be transformed by the function
  @fun{gdk-keymap-add-virtual-modifiers} in order to contain the expected
  result.

  Since 3.4
  @see-class{gdk-keymap}
  @see-symbol{gdk-modifier-type}
  @see-function{gdk-keymap-add-virtual-modifiers}"
  (keymap (g-object gdk-keymap))
  (intent gdk-modifier-intent))

(export 'gdk-keymap-get-modifier-mask)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_name" gdk-keyval-name) (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keyval]{a key value}
  @begin{return}
   A string containing the name of the key, or NULL if keyval is not a
   valid key. The string should not be modified.
  @end{return}
  @begin{short}
    Converts a key value into a symbolic name.
  @end{short}

  The names are the same as those in the @code{<gdk/gdkkeysyms.h>} header file
  but without the leading @code{\"GDK_KEY_\"}."
  (keyval :uint))

(export 'gdk-keyval-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_from_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_from_name" gdk-keyval-from-name) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-5-13}
  @argument[keyval-name]{a key name}
  @begin{return}
    The corresponding key value, or @code{GDK_KEY_VoidSymbol} if the key name is
    not a valid key.
  @end{return}
  @begin{short}
    Converts a key name to a key value.
  @end{short}

  The names are the same as those in the @code{<gdk/gdkkeysyms.h>} header file
  but without the leading @code{\"GDK_KEY_\"}."
  (keyval-name :string))

(export 'gdk-keyval-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_convert_case ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_convert_case" %gdk-keyval-convert-case) :void
  (symbol :uint)
  (lower (:pointer :uint))
  (upper (:pointer :uint)))

(defun gdk-keyval-convert-case (symbol)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[symbol]{a keyval}
  @begin{return}
    @code{lower} --  lowercase version of symbol @br{}
    @code{upper} -- uppercase version of symbol
  @end{return}
  Obtains the upper-case and lower-case versions of the @arg{keyval} symbol.
  Examples of keyvals are GDK_KEY_a, GDK_KEY_Enter, GDK_KEY_F1, etc."
  (with-foreign-objects ((lower :uint) (upper :uint))
    (%gdk-keyval-convert-case symbol lower upper)
    (values (mem-ref lower :uint)
            (mem-ref upper :uint))))

(export 'gdk-keyval-convert-case)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_upper ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_to_upper" gdk-keyval-to-upper) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keyval]{a key value}
  @return{The upper case form of @arg{keyval}, or @arg{keyval} itself if it is
    already in upper case or it is not subject to case conversion.}
  Converts a key value to upper case, if applicable."
  (keyval :uint))

(export 'gdk-keyval-to-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_to_lower" gdk-keyval-to-lower) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keyval]{a key value}
  @return{The lower case form of @arg{keyval}, or @arg{keyval} itself if it is
    already in lower case or it is not subject to case conversion.}
  Converts a key value to lower case, if applicable."
  (keyval :uint))

(export 'gdk-keyval-to-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_upper ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_is_upper" gdk-keyval-is-upper) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keyval]{a key value}
  @return{@em{True} if @arg{keyval} is in upper case, or if @arg{keyval} is not
    subject to case conversion.}
  Returns @em{true} if the given key value is in upper case."
  (keyval :uint))

(export 'gdk-keyval-is-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_is_lower" gdk-keyval-is-lower) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-12}
  @argument[keyval]{a key value}
  @return{@em{True} if @arg{keyval} is in lower case, or if @arg{keyval} is not
    subject to case conversion.}
  Returns @em{true} if the given key value is in lower case."
  (keyval :uint))

(export 'gdk-keyval-is-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_unicode ()
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
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[keyval]{a GDK key symbol}
  @return{The corresponding unicode character, or @code{#\Nul} if there is no
    corresponding character.}
  @begin{short}
    Convert from a GDK key symbol to the corresponding ISO10646 (Unicode)
    character.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (mapcar 'gdk-keyval-to-unicode '(65 66 67 68 69 70 71))
=> (#\A #\B #\C #\D #\E #\F #\G)
    @end{pre}
  @end{dictionary}"
  (keyval :uint))

(export 'gdk-keyval-to-unicode)

;;; ----------------------------------------------------------------------------
;;; gdk_unicode_to_keyval ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_unicode_to_keyval" gdk-unicode-to-keyval) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-6-13}
  @argument[wc]{a ISO10646 encoded character}
  @return{The corresponding GDK key symbol, if one exists, or, if there is no
    corresponding symbol, @code{@arg{wc} | 0x01000000}.}
  @short{Convert from a ISO10646 character to a key symbol.}
  @begin[Examples]{dictionary}
    @begin{pre}
 (mapcar 'gdk-unicode-to-keyval '(#\a #\b #\c))
=> (97 98 99)
    @end{pre}
  @end{dictionary}"
  (char unichar))

(export 'gdk-unicode-to-keyval)

;;; --- gdk.key-values.lisp ----------------------------------------------------
