;;; ----------------------------------------------------------------------------
;;; gdk.key-values.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     Functions for manipulating keyboard codes
;;;
;;; Types and Values
;;;
;;;     GdkKeymap
;;;     GdkKeymapKey
;;;
;;; Functions
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
;;;     gdk_keymap_get_scroll_lock_state
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
;;;
;;; Signals
;;;
;;;     void    direction-changed    Run Last
;;;     void    keys-changed         Run Last
;;;     void    state-changed        Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkKeymap
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
 "@version{2021-3-31}
  @begin{short}
    A @sym{gdk-keymap} object defines the translation from keyboard state,
    including a hardware key, a modifier mask, and active keyboard group, to a
    keyval.
  @end{short}
  This translation has two phases. The first phase is to determine the effective
  keyboard group and level for the keyboard state. The second phase is to look
  up the keycode/group/level triplet in the keymap and see what keyval it
  corresponds to.

  Key values are the codes which are sent whenever a key is pressed or released.
  They appear in the @code{keyval} field of the @class{gdk-event-key}
  structure, which is passed to signal handlers for the \"key-press-event\" and
  \"key-release-event\" signals. The complete list of key values can be found
  in the @file{gdk/gdkkeysyms.h} header file.

  Key values are regularly updated from the upstream X.org X11 implementation,
  so new values are added regularly. They will be prefixed with GDK_KEY_
  rather than XF86XK_ or XK_ (for older symbols).

  Key values can be converted into a string representation using the function
  @fun{gdk-keyval-name}. The reverse function, converting a string to a key
  value, is provided by the function @fun{gdk-keyval-from-name}.

  The case of key values can be determined using the functions
  @fun{gdk-keyval-is-upper} and @fun{gdk-keyval-is-lower}. Key values can be
  converted to upper or lower case using the functions @fun{gdk-keyval-to-upper}
  and @fun{gdk-keyval-to-lower}.

  When it makes sense, key values can be converted to and from Unicode
  characters with the functions @fun{gdk-keyval-to-unicode} and
  @fun{gdk-unicode-to-keyval}.

  @subheading{Groups}
  One @sym{gdk-keymap} object exists for each user display. To obtain the keymap
  for the display, use the function @fun{gdk-keymap-for-display}. A keymap is a
  mapping from a keycode, group, and level to key values. You can think of these
  values as a representation of a symbol printed on a physical keyboard key.
  That is, it contains three pieces of information. First, it contains the
  hardware keycode. This is an identifying number for a physical key. Second, it
  contains the \"level\" of the key. The level indicates which symbol on the key
  will be used, in a vertical direction. So on a standard US keyboard, the key
  with the number \"1\" on it also has the exclamation point \"!\" character on
  it. The level indicates whether to use the \"1\" or the \"!\" symbol. The
  letter keys are considered to have a lowercase letter at level 0, and an
  uppercase letter at level 1, though only the uppercase letter is printed.
  Third, there is the group. Groups are not used on standard US keyboards, but
  are used in many other countries. On a keyboard with groups, there can be 3
  or 4 symbols printed on a single key. The group indicates movement in a
  horizontal direction. Usually groups are used for two different languages. In
  group 0, a key might have two English characters, and in group 1 it might have
  two Hebrew characters. The Hebrew characters will be printed on the key next
  to the English characters.

  In order to use a keymap to interpret a key event, it is necessary to first
  convert the keyboard state into an effective group and level. This is done
  via a set of rules that varies widely according to type of keyboard and user
  configuration. The function @fun{gdk-keymap-translate-keyboard-state} accepts
  a keyboard state - consisting of hardware keycode pressed, active modifiers,
  and active group - applies the appropriate rules, and returns the group/level
  to be used to index the keymap, along with the modifiers which did not affect
  the group and level, i.e. it returns \"unconsumed modifiers\". The keyboard
  group may differ from the effective group used for keymap lookups because
  some keys do not have multiple groups - e.g. the Enter key is always in group
  0 regardless of keyboard state.

  Note that the function @fun{gdk-keymap-translate-keyboard-state} also returns
  the keyval, i.e. it goes ahead and performs the keymap lookup in addition to
  telling you which effective group/level values were used for the lookup.
  The @class{gdk-event-key} event already contains this keyval, however, so you
  do not normally need to call the function
  @fun{gdk-keymap-translate-keyboard-state} just to get the keyval.
  @begin[Signal Details]{dictionary}
    @subheading{The \"direction-changed\" signal}
      @begin{pre}
 lambda (keymap)    :run-last
      @end{pre}
      The signal gets emitted when the direction of the keymap changes.
      @begin[code]{table}
        @entry[keymap]{The @sym{gdk-keymap} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"keys-changed\" signal}
      @begin{pre}
 lambda (keymap)    :run-last
      @end{pre}
      The signal is emitted when the mapping represented by keymap changes.
      @begin[code]{table}
        @entry[keymap]{The @sym{gdk-keymap} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"state-changed\" signal}
      @begin{pre}
 lambda (keymap)    :run-last
      @end{pre}
      The signal is emitted when the state of the keyboard changes, e.g. when
      Caps Lock is turned on or off. See the function
      @fun{gdk-keymap-caps-lock-state}.
      @begin[code]{table}
        @entry[keymap]{The @sym{gdk-keymap} object on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-class{gdk-event-key}")

;;; ----------------------------------------------------------------------------
;;; struct GdkKeymapKey
;;; ----------------------------------------------------------------------------

;(define-g-boxed-cstruct gdk-keymap-key nil
;  (keycode :uint :initform 0)
;  (group :int :initform 0)
;  (level :int :initform 0))

;; not exported

(defcstruct gdk-keymap-key
  (keycode :uint)
  (group :int)
  (level :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'gdk-keymap-key atdoc:*external-symbols*)
 "@version{2021-3-31}
  @begin{short}
    A @sym{gdk-keymap-key} instance is a hardware key that can be mapped to a
    keyval.
  @end{short}
  @begin{pre}
(defcstruct gdk-keymap-key
  (keycode :uint)
  (group :int)
  (level :int))
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
      it also has the exclamation point \"!\" character on it. The level
      indicates whether to use the \"1\" or the \"!\" symbol. The letter keys
      are considered to have a lowercase letter at level 0, and an uppercase
      letter at level 1, though only the uppercase letter is printed.}
  @end{table}
  @see-slot{gdk-keymap-key-keycode}
  @see-slot{gdk-keymap-key-group}
  @see-slot{gdk-keymap-key-level}
  @see-class{gdk-keymap}")

;;; ----------------------------------------------------------------------------
;;; Accessors of the GdkKeymapKey structure
;;; ----------------------------------------------------------------------------

;;; --- gdk-keymap-key-keycode -------------------------------------------------

;; not exported

(defun (setf gdk-keymap-key-keycode) (value instance)
  (setf (foreign-slot-value instance '(:struct gdk-keymap-key) 'keycode) value))

(defun gdk-keymap-key-keycode (instance)
  (foreign-slot-value instance '(:struct gdk-keymap-key) 'keycode))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key-keycode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-keymap-key-keycode 'function)
 "@version{2021-3-31}
  @syntax[]{(gdk-keymap-key-keycode instance) => keycode}
  @syntax[]{(setf (gdk-keymap-key-keycode instance) keycode)}
  @argument[instance]{a @class{gdk-keymap-key} instance}
  @argument[keycode]{an unsigend integer with the hardware keycode}
  @begin{short}
    Accessor of the @code{keycode} slot of the @class{gdk-keymap-key} structure.
  @end{short}
  @see-class{gdk-keymap-key}")

;;; --- gdk-keymap-key-group ---------------------------------------------------

;; not exported

(defun (setf gdk-keymap-key-group) (value instance)
  (setf (foreign-slot-value instance '(:struct gdk-keymap-key) 'group) value))

(defun gdk-keymap-key-group (instance)
  (foreign-slot-value instance '(:struct gdk-keymap-key) 'group))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-keymap-key-group 'function)
 "@version{2021-3-31}
  @syntax[]{(gdk-keymap-key-group instance) => group}
  @syntax[]{(setf (gdk-keymap-key-group instance) group)}
  @argument[instance]{a @class{gdk-keymap-key} instance}
  @argument[group]{an unsigned integer which indicates movement in a horizontal
    direction}
  @begin{short}
    Accessor of the @code{group} slot of the @class{gdk-keymap-key} structure.
  @end{short}
  @see-class{gdk-keymap-key}")

;;; --- gdk-keymap-key-level ---------------------------------------------------

;; not exported

(defun (setf gdk-keymap-key-level) (value instance)
  (setf (foreign-slot-value instance '(:struct gdk-keymap-key) 'level) value))

(defun gdk-keymap-key-level (instance)
  (foreign-slot-value instance '(:struct gdk-keymap-key) 'level))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-keymap-key-level atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-keymap-key-level 'function)
 "@version{2021-3-31}
  @syntax[]{(gdk-keymap-key-level instance) => level}
  @syntax[]{(setf (gdk-keymap-key-level instance) level)}
  @argument[instance]{a @class{gdk-keymap-key} instance}
  @argument[level]{an integer which indicates which symbol on the key will be
    used, in a vertical direction}
  @begin{short}
    Accessor of the @code{level} slot of the @class{gdk-keymap-key} structure.
  @end{short}
  @see-class{gdk-keymap-key}")

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_default () -> gdk-keymap-default
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_default" gdk-keymap-default) (g-object gdk-keymap)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-31}
  @return{The @class{gdk-keymap} object attached to the default display.}
  @begin{short}
    Returns the keymap attached to the default display.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-keymap-default} has been deprecated since version
    3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-keymap-for-display} instead.
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-function{gdk-keymap-for-display}")

(export 'gdk-keymap-default)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_for_display () -> gdk-keymap-for-display
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_for_display" gdk-keymap-for-display)
    (g-object gdk-keymap)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-31}
  @argument[display]{a @class{gdk-display} object}
  @return{The @class{gdk-keymap} object attached to @arg{display}.}
  @short{Returns the keymap attached to the display.}
  @see-class{gdk-keymap}
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-keymap-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_lookup_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_lookup_key" %gdk-keymap-lookup-key) :uint
  (keymap (g-object gdk-keymap))
  (key (:pointer (:struct gdk-keymap-key))))

(defun gdk-keymap-lookup-key (keymap keycode group level)
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[keycode]{an unsigned integer with the hardware keycode}
  @argument[group]{an integer which indicates movement in a horizontal
    direction}
  @argument[level]{an integer which indicates which symbol on the key will be
    used}
  @return{An unsigned integer with the keyval, or 0 if none was mapped to the
    given key.}
  @begin{short}
    Looks up the keyval mapped to a keycode/group/level triplet.
  @end{short}
  If no keyval is bound to @arg{key}, returns 0. For normal user input, you want
  to use the function @fun{gdk-keymap-translate-keyboard-state} instead of this
  function, since the effective group/level may not be the same as the current
  keyboard state.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-keyval-name (gdk-keymap-lookup-key keymap  35 0 0)) => \"plus\"
(gdk-keyval-name (gdk-keymap-lookup-key keymap  35 0 1)) => \"asterisk\"
(gdk-keyval-name (gdk-keymap-lookup-key keymap  35 0 2)) => \"asciitilde\"
(gdk-keyval-name (gdk-keymap-lookup-key keymap  35 0 3)) => \"macron\"
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-function{gdk-keymap-translate-keyboard-state}"
  (with-foreign-object (key '(:struct gdk-keymap-key))
    (setf (gdk-keymap-key-keycode key) keycode)
    (setf (gdk-keymap-key-group key) group)
    (setf (gdk-keymap-key-level key) level)
    (%gdk-keymap-lookup-key keymap key)))

(export 'gdk-keymap-lookup-key)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_translate_keyboard_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_translate_keyboard_state"
          %gdk-keymap-translate-keyboard-state) :boolean
  (keymap (g-object gdk-keymap))
  (keycode :uint)
  (state gdk-modifier-type)
  (group :int)
  (keyval (:pointer :uint))
  (effective (:pointer :int))
  (level (:pointer :int))
  (consumed (:pointer gdk-modifier-type)))

(defun gdk-keymap-translate-keyboard-state (keymap keycode state group)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[keycode]{an unsigned integer with the keycode}
  @argument[state]{a @symbol{gdk-modifier-type} modifier state}
  @argument[group]{an integer with the active keyboard group}
  @begin{return}
    @arg{keyval} -- an unsigned integer with the keyval @br{}
    @arg{effective} -- an integer with the effective group @br{}
    @arg{level} -- an integer with the level @br{}
    @arg{consumed} -- the @symbol{gdk-modifier-type} flags that were
    used to determine the group or level
  @end{return}
  @begin{short}
    Translates the contents of @arg{keycode}, @arg{state}, and @arg{group} into
    a keyval, effective group, and level.
  @end{short}
  Modifiers that affected the translation and are thus unavailable for
  application use are returned in @arg{consumed}. The return value
  @arg{effective} is the group that was actually used for the translation. Some
  keys such as Enter are not affected by the active keyboard group. The level is
  derived from state. For convenience, the @class{gdk-event-key} event already
  contains the translated keyval, so this function is not as useful as you might
  think.

  The @arg{consumed} flags gives modifiers that should be masked out from
  @arg{state} when comparing this key press to a hot key. For instance, on a
  US keyboard, the @code{plus} symbol is shifted, so when comparing a key press
  to a @code{<Control>plus} accelerator @code{<Shift>} should be masked out.
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
  @see-class{gdk-keymap}
  @see-class{gdk-event-key}
  @see-symbol{gdk-modifier-type}"
  (with-foreign-objects ((keyval :uint)
                         (effective :int)
                         (level :int)
                         (consumed 'gdk-modifier-type))
    (when (%gdk-keymap-translate-keyboard-state keymap
                                                keycode
                                                state
                                                group
                                                keyval
                                                effective
                                                level
                                                consumed)
      (values (mem-ref keyval :uint)
              (mem-ref effective :int)
              (mem-ref level :int)
              (mem-ref consumed 'gdk-modifier-type)))))

(export 'gdk-keymap-translate-keyboard-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keyval () -> gdk-keymap-entries-for-keyval
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_entries_for_keyval"
          %gdk-keymap-entries-for-keyval) :boolean
  (keymap (g-object gdk-keymap))
  (keyval :uint)
  (keys :pointer)
  (n-keys (:pointer :int)))

(defun gdk-keymap-entries-for-keyval (keymap keyval)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[keyval]{a keyval, such as @code{GDK_a}, @code{GDK_Up},
    @code{GDK_Return}, etc.}
  @return{A list of keycode, group, and level values.}
  @begin{short}
    Obtains a list of keycode/group/level combinations that will generate
    @arg{keyval}.
  @end{short}
  Groups and levels are two kinds of keyboard mode. In general, the level
  determines whether the top or bottom symbol on a key is used, and the
  group determines whether the left or right symbol is used. On US keyboards,
  the shift key changes the keyboard level, and there are no groups. A group
  switch key might convert a keyboard between Hebrew to English modes, for
  example. A @class{gdk-event-key} event contains a group field that indicates
  the active keyboard group. The level is computed from the modifier mask.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-keymap-entries-for-keyval keymap (gdk-keyval-from-name \"plus\"))
=> ((35 0 0))
(gdk-keymap-entries-for-keyval keymap (gdk-keyval-from-name \"asciitilde\"))
=> ((35 0 2))
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-class{gdk-event-key}"
  (with-foreign-objects ((keys :pointer) (n-keys :int))
    (when (%gdk-keymap-entries-for-keyval keymap keyval keys n-keys)
      (let ((keys (mem-ref keys :pointer))
            (n-keys (mem-ref n-keys :int)))
        (loop for i from 0 below n-keys
              for key = (mem-aptr keys '(:struct gdk-keymap-key) i)
              collect (list (gdk-keymap-key-keycode key)
                            (gdk-keymap-key-group key)
                            (gdk-keymap-key-level key))
              finally (g-free keys))))))

(export 'gdk-keymap-entries-for-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keycode () -> gdk-keymap-entries-for-keycode
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_entries_for_keycode"
          %gdk-keymap-entries-for-keycode) :boolean
  (keymap (g-object gdk-keymap))
  (hardware-keycode :uint)
  (keys :pointer)
  (keyvals :pointer)
  (n-entries :pointer))

(defun gdk-keymap-entries-for-keycode (keymap keycode)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[keycode]{an integer with a hardware keycode}
  @begin{return}
    A list of  keyval, keycode, group, and level values.
  @end{return}
  @begin{short}
    Returns the keyvals bound to @arg{keycode}.
  @end{short}
  When a keycode is pressed by the user, the keyval from this list
  of entries is selected by considering the effective keyboard group and
  level. See the function @fun{gdk-keymap-translate-keyboard-state}.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-keymap-entries-for-keycode keymap 35)
=> ((43 35 0 0) (42 35 0 1) (126 35 0 2) (175 35 0 3))
(gdk-keymap-entries-for-keycode keymap 35)
=> ((43 35 0 0) (42 35 0 1) (126 35 0 2) (175 35 0 3))
(mapcar #'gdk-keyval-name (mapcar #'first *))
=> (\"plus\" \"asterisk\" \"asciitilde\" \"macron\")
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-function{gdk-keymap-translate-keyboard-state}"
  (with-foreign-objects ((keys :pointer) (keyvals :pointer) (n-keys :int))
    (when (%gdk-keymap-entries-for-keycode keymap keycode keys keyvals n-keys)
        (let ((keys (mem-ref keys :pointer))
              (keyvals (mem-ref keyvals :pointer))
              (n-keys (mem-ref n-keys :int)))
          (loop for i from 0 below n-keys
                for keyval = (mem-aref keyvals :uint i)
                for key = (mem-aptr keys '(:struct gdk-keymap-key) i)
                collect (list keyval
                              (gdk-keymap-key-keycode key)
                              (gdk-keymap-key-group key)
                              (gdk-keymap-key-level key))
                finally (g-free keys)
                        (g-free keyvals))))))

(export 'gdk-keymap-entries-for-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_direction () -> gdk-keymap-direction
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_direction" gdk-keymap-direction) pango-direction
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @begin{return}
    The value @code{:ltr} or @code{:rtl} if it can determine the direction.
    The value @code{:neutral} otherwise.
  @end{return}
  @begin{short}
    Returns the Pango direction of the effective layout of the keymap.
  @end{short}
  @see-class{gdk-keymap}
  @see-symbol{pango-direction}"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_have_bidi_layouts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_have_bidi_layouts" gdk-keymap-have-bidi-layouts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if there are layouts in both directions, @em{false}
    otherwise.}
  @begin{short}
    Determines if keyboard layouts for both right-to-left and left-to-right
    languages are in use.
  @end{short}
  @see-class{gdk-keymap}"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-have-bidi-layouts)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_caps_lock_state () -> gdk-keymap-caps-lock-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_caps_lock_state" gdk-keymap-caps-lock-state) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if Caps Lock is on.}
  @short{Returns whether the Caps Lock modifer is locked.}
  @see-class{gdk-keymap}"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-caps-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_num_lock_state () -> gdk-keymap-num-lock-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_num_lock_state" gdk-keymap-num-lock-state) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if Num Lock is on.}
  @short{Returns whether the Num Lock modifer is locked.}
  @see-class{gdk-keymap}"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-num-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_scroll_lock_state () -> gdk-keymap-scroll-lock-state
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defcfun ("gdk_keymap_get_scroll_lock_state" gdk-keymap-scroll-lock-state)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{@em{True} if Scroll Lock is on.}
  @short{Returns whether the Scroll Lock modifer is locked.}

  Since 3.18
  @see-class{gdk-keymap}"
  (keymap (g-object gdk-keymap)))

#+gtk-3-18
(export 'gdk-keymap-scroll-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_modifier_state () -> gdk-keymap-modifier-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_modifier_state" gdk-keymap-modifier-state)
    gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2021-4-1}
  @argument[keymap]{a @class{gdk-keymap} object}
  @return{The current @symbol{gdk-modifier-type} modifier state.}
  @short{Returns the current modifier state.}
  @see-class{gdk-keymap}
  @see-symbol{gdk-modifier-type}"
  (keymap (g-object gdk-keymap)))

(export 'gdk-keymap-modifier-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_add_virtual_modifiers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_add_virtual_modifiers" %gdk-keymap-add-virtual-modifiers)
    :void
  (keymap (g-object gdk-keymap))
  (state (:pointer gdk-modifier-type)))

(defun gdk-keymap-add-virtual-modifiers (keymap state)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[state]{the @symbol{gdk-modifier-type} flags}
  @return{The @symbol{gdk-modfier-type} flags.}
  @begin{short}
    Adds virtual modifiers (i.e. Super, Hyper and Meta) which correspond to the
    real modifiers (i.e. Mod2, Mod3, ...) in modifiers and set the corresponding
    bits in state.
  @end{short}

  GDK already does this before delivering key events, but for compatibility
  reasons, it only sets the first virtual modifier it finds, whereas this
  function sets all matching virtual modifiers.

  This function is useful when matching key events against accelerators.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-keymap-add-virtual-modifiers keymap :mod4-mask)
=> (:MOD4-MASK :SUPER-MASK :HYPER-MASK)
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-symbol{gdk-modifier-type}"
  (with-foreign-object (modifier 'gdk-modifier-type)
    (setf (mem-ref modifier 'gdk-modifier-type) state)
    (%gdk-keymap-add-virtual-modifiers keymap modifier)
    (mem-ref modifier 'gdk-modifier-type)))

(export 'gdk-keymap-add-virtual-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_map_virtual_modifiers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_map_virtual_modifiers" %gdk-keymap-map-virtual-modifiers)
    :boolean
  (keymap (g-object gdk-keymap))
  (state (:pointer gdk-modifier-type)))

(defun gdk-keymap-map-virtual-modifiers (keymap state)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[state]{the @symbol{gdk-modifier-type} flags}
  @begin{return}
    @arg{modifier} -- a @symbol{gdk-mofiier-type} value @br{}
    @em{True} if no virtual modifiers were mapped to the same non-virtual
    modifier. Note that @code{nil} is also returned if a virtual modifier is
    mapped to a non-virtual modifier that was already set in state.
  @end{return}
  @begin{short}
    Maps the virtual modifiers (i.e. Super, Hyper and Meta) which are set in
    state to their non-virtual counterparts (i.e. Mod2, Mod3,...) and set the
    corresponding bits in state.
  @end{short}

  This function is useful when matching key events against accelerators.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-keymap-map-virtual-modifiers keymap :super-mask)
=> (:MOD4-MASK :SUPER-MASK)
=> T
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-symbol{gdk-modifier-type}"
  (with-foreign-object (modifier 'gdk-modifier-type)
    (setf (mem-ref modifier 'gdk-modifier-type) state)
    (values (mem-ref modifier 'gdk-modifier-type)
            (%gdk-keymap-map-virtual-modifiers keymap modifier))))

(export 'gdk-keymap-map-virtual-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_modifier_mask () -> gdk-keymap-modifier-mask
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keymap_get_modifier_mask" gdk-keymap-modifier-mask)
    gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keymap]{a @class{gdk-keymap} object}
  @argument[intent]{the @symbol{gdk-modifier-intent} use case for the
    modifier mask}
  @return{The @symbol{gdk-modifier-type} modifier mask used for @arg{intent}.}
  @begin{short}
    Returns the modifier mask the keymap's windowing system backend uses for a
    particular purpose.
  @end{short}

  Note that this function always returns real hardware modifiers, not virtual
  ones (e.g. it will return @code{:mod1-mask} rather than @code{:meta-mask} if
  the backend maps @code{MOD1} to @code{META}), so there are use cases where
  the return value of this function has to be transformed by the function
  @fun{gdk-keymap-add-virtual-modifiers} in order to contain the expected
  result.
  @see-class{gdk-keymap}
  @see-symbol{gdk-modifier-type}
  @see-symbol{gdk-modifier-intent}
  @see-function{gdk-keymap-add-virtual-modifiers}"
  (keymap (g-object gdk-keymap))
  (intent gdk-modifier-intent))

(export 'gdk-keymap-modifier-mask)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_name" gdk-keyval-name) (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keyval]{an unsigned integer with a key value}
  @begin{return}
   A string containing the name of the key, or @code{nil} if @arg{keyval} is
   not a valid key.
  @end{return}
  @begin{short}
    Converts a key value into a symbolic name.
  @end{short}

  The names are the same as those in the @code{<gdk/gdkkeysyms.h>} header file
  but without the leading @code{\"GDK_KEY_\"}.
  @see-class{gdk-keymap}"
  (keyval :uint))

(export 'gdk-keyval-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_from_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_from_name" gdk-keyval-from-name) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[name]{a string with the key name}
  @begin{return}
    An unsigned integer with the corresponding key value, or the value of
    @code{GDK_KEY_VoidSymbol} if the key name is not a valid key.
  @end{return}
  @begin{short}
    Converts a key name to a key value.
  @end{short}

  The names are the same as those in the @code{<gdk/gdkkeysyms.h>} header file
  but without the leading @code{\"GDK_KEY_\"}.
  @see-class{gdk-keymap}"
  (name :string))

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
 "@version{2021-4-2}
  @argument[symbol]{an unsigned integer with the keyval}
  @begin{return}
    @arg{lower} -- an unsigned integer with the lowercase version of symbol@br{}
    @arg{upper} -- an unsigned integer with the uppercase version of symbol
  @end{return}
  @begin{short}
    Obtains the upper-case and lower-case versions of the @arg{keyval} symbol.
  @end{short}
  Examples of keyvals are GDK_KEY_a, GDK_KEY_Enter, GDK_KEY_F1, etc.
  @see-class{gdk-keymap}"
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
 "@version{2021-4-2}
  @argument[keyval]{an unsigned integer with a key value}
  @return{The upper case form of @arg{keyval}, or @arg{keyval} itself if it is
    already in upper case or it is not subject to case conversion.}
  @begin{short}
    Converts a key value to upper case, if applicable.
  @end{short}
  @see-class{gdk-keymap}
  @see-function{gdk-keyval-to-lower}"
  (keyval :uint))

(export 'gdk-keyval-to-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_to_lower" gdk-keyval-to-lower) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keyval]{an unsigned integer with a key value}
  @return{The lower case form of @arg{keyval}, or @arg{keyval} itself if it is
    already in lower case or it is not subject to case conversion.}
  @begin{short}
    Converts a key value to lower case, if applicable.
  @end{short}
  @see-class{gdk-keymap}
  @see-function{gdk-keyval-to-upper}"
  (keyval :uint))

(export 'gdk-keyval-to-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_upper ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_is_upper" gdk-keyval-is-upper) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keyval]{an unsigned integer with a key value}
  @return{@em{True} if @arg{keyval} is in upper case, or if @arg{keyval} is not
    subject to case conversion.}
  @begin{short}
    Returns @em{true} if the given key value is in upper case.
  @end{short}
  @see-class{gdk-keymap}
  @see-function{gdk-keyval-is-lower}"
  (keyval :uint))

(export 'gdk-keyval-is-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyval_is_lower" gdk-keyval-is-lower) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @argument[keyval]{an unsigned integer with a key value}
  @return{@em{True} if @arg{keyval} is in lower case, or if @arg{keyval} is not
    subject to case conversion.}
  @begin{short}
    Returns @em{true} if the given key value is in lower case.
  @end{short}
  @see-class{gdk-keymap}
  @see-function{gdk-keyval-is-upper}"
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
 "@version{2021-5-19}
  @argument[keyval]{an unsigned integer with a GDK key symbol}
  @return{The corresponding unicode character, or @code{#\\Nul} if there is no
    corresponding character.}
  @begin{short}
    Convert from a GDK key symbol to the corresponding ISO10646 (Unicode)
    character.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(mapcar 'gdk-keyval-to-unicode '(65 66 67 68 69 70 71))
=> (#\\A #\\B #\\C #\\D #\\E #\\F #\\G)
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-function{gdk-unicode-to-keyval}"
  (keyval :uint))

(export 'gdk-keyval-to-unicode)

;;; ----------------------------------------------------------------------------
;;; gdk_unicode_to_keyval ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_unicode_to_keyval" gdk-unicode-to-keyval) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-5-19}
  @argument[unichar]{a ISO10646 encoded character}
  @return{An unsigned integer with the corresponding GDK key symbol, if one
    exists, or, if there is no corresponding symbol, @code{@arg{unichar} |
    0x01000000}.}
  @short{Convert from a ISO10646 character to a key symbol.}
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar 'gdk-unicode-to-keyval '(#\\a #\\b #\\c))
=> (97 98 99)
    @end{pre}
  @end{dictionary}
  @see-class{gdk-keymap}
  @see-function{gdk-keyval-to-unicode}"
  (unichar unichar))

(export 'gdk-unicode-to-keyval)

;;; --- gdk.key-values.lisp ----------------------------------------------------
