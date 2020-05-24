(def-suite gdk-key-values :in gdk-suite)
(in-suite gdk-key-values)

;;;     GdkKeymap
;;;     GdkKeymapKey

;;;   gdk_keymap_get_default

(test gdk-keymap-get-default
  (is (eq 'gdk-keymap (type-of (gdk-keymap-get-default)))))

;;;   gdk_keymap_get_for_display

(test gdk-keymap-get-for-display
  (let ((display (gdk-display-default)))
    (is (eq 'gdk-keymap (type-of (gdk-keymap-get-for-display display))))))

;;;   gdk_keymap_lookup_key

; TODO: This test generates a warning
;
;   bare references to struct types are deprecated.
;   Please use (:POINTER (:STRUCT GDK::GDK-KEYMAP-KEY-CSTRUCT)) or
;   (:STRUCT GDK::GDK-KEYMAP-KEY-CSTRUCT) instead.

(test gdk-keymap-lookup-key
  (let ((keymap (gdk-keymap-get-default))
        (key (make-gdk-keymap-key :keycode 67 :group 0 :level 0)))
    (is (= 65470 (gdk-keymap-lookup-key keymap key)))))

;;;     gdk_keymap_translate_keyboard_state

;;;     gdk_keymap_get_entries_for_keyval

(test gdk-keymap-get-entries-for-keyvals
  (let ((keymap (gdk-keymap-get-default)))
    (is (eq 'gdk-keymap-key (type-of (first (gdk-keymap-get-entries-for-keyval keymap 65470)))))))

;;;     gdk_keymap_get_entries_for_keycode

(test gdk-keymap-get-entries-for-keycode
  (let ((keymap (gdk-keymap-get-default)))
    (is (eq 'gdk-keymap-key (type-of (first (gdk-keymap-get-entries-for-keycode keymap 67)))))))

;;;     gdk_keymap_get_direction

(test gdk-keymap-get-direction
  (is (eq :ltr (gdk-keymap-get-direction (gdk-keymap-get-default)))))

;;;     gdk_keymap_have_bidi_layouts

(test gdk-keymap-have-bidi-layouts
  (is-false (gdk-keymap-have-bidi-layouts (gdk-keymap-get-default))))

;;;     gdk_keymap_get_caps_lock_state

(test gdk-keymap-caps-lock-state
  (is-false (gdk-keymap-caps-lock-state (gdk-keymap-get-default))))

;;;     gdk_keymap_get_num_lock_state

(test gdk-keymap-num-lock-state
  (is-false (gdk-keymap-num-lock-state (gdk-keymap-get-default))))

;;;     gdk_keymap_get_scroll_lock_state

(test gdk-keymap-scroll-lock-state
  (is-false (gdk-keymap-scroll-lock-state (gdk-keymap-get-default))))

;;;     gdk_keymap_get_modifier_state

(test gdk-keymap-modifier-state
  (is (equal '() (gdk-keymap-modifier-state (gdk-keymap-get-default)))))

;;;     gdk_keymap_add_virtual_modifiers
;;;     gdk_keymap_map_virtual_modifiers
;;;     gdk_keymap_get_modifier_mask

;;;     gdk_keyval_name

(test gdk-keyval-name
  (is (string= "A" (gdk-keyval-name 65)))
  (is (string= "B" (gdk-keyval-name 66))))

;;;     gdk_keyval_from_name

(test gdk-keyval-from-name
  (is (= 65 (gdk-keyval-from-name "A")))
  (is (= 66 (gdk-keyval-from-name "B"))))

;;;     gdk_keyval_convert_case

(test gdk-keyval-convert-case
  (is (= 97 (gdk-keyval-convert-case 65))))

;;;     gdk_keyval_to_upper

(test gdk-keyval-to-upper
  (is (= 65 (gdk-keyval-to-upper 97))))

;;;     gdk_keyval_to_lower

(test gdk-keyval-to-lower
  (is (= 97 (gdk-keyval-to-lower 65))))

;;;     gdk_keyval_is_upper

(test gdk-keyval-is-upper
  (is-true  (gdk-keyval-is-upper (gdk-keyval-from-name "A")))
  (is-false (gdk-keyval-is-upper (gdk-keyval-from-name "a"))))

;;;     gdk_keyval_is_lower

(test gdk-keyval-is-lower
  (is-true  (gdk-keyval-is-upper (gdk-keyval-from-name "A")))
  (is-false (gdk-keyval-is-upper (gdk-keyval-from-name "a"))))

;;;     gdk_keyval_to_unicode

(test gdk-keyval-to-unicode
  (is (eq #\A  (gdk-keyval-to-unicode (gdk-keyval-from-name "A")))))

;;;     gdk_unicode_to_keyval

(test gdk-unicode-to-keyval
  (is (eq 65 (gdk-unicode-to-keyval #\A))))

