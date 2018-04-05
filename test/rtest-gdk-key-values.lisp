(in-package :gtk-testsuite)

(def-suite gdk-key-values :in gdk-suite)
(in-suite gdk-key-values)

;;;     GdkKeymap
;;;     GdkKeymapKey

;;;   gdk_keymap_get_default

(test gdk-keymap-get-default
  (is (eq 'gdk-keymap (type-of (gdk-keymap-get-default)))))

;;;   gdk_keymap_get_for_display

(test gdk-keymap-get-for-display
  (let ((display (gdk-display-get-default)))
    (is (eq 'gdk-keymap (type-of (gdk-keymap-get-for-display display))))))

;;;   gdk_keymap_lookup_key

#+nil
(test gdk-keymap-lookup-key
  (let ((keymap (gdk-keymap-get-default))
        (key (make-gdk-keymap-key :keycode 97 :group 0 :level 0)))
    (is-false (gdk-keymap-lookup-key keymap key))))


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

