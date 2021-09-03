(def-suite gdk-window :in gdk-suite)
(in-suite gdk-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkWindowType

(test gdk-window-type
  ;; Check the type
  (is (g-type-is-enum "GtkWindowType"))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowType")
          (gtype (foreign-funcall "gdk_window_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-window-type (registered-enum-type "GtkWindowType")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_ROOT" "GDK_WINDOW_TOPLEVEL" "GDK_WINDOW_CHILD"
               "GDK_WINDOW_TEMP" "GDK_WINDOW_FOREIGN" "GDK_WINDOW_OFFSCREEN"
               "GDK_WINDOW_SUBSURFACE")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkWindowType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkWindowType"))))
  ;; Check the nick names
  (is (equal '("root" "toplevel" "child" "temp" "foreign" "offscreen"
               "subsurface")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkWindowType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowType"
                             GDK-WINDOW-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_window_type_get_type")
                             (:ROOT 0)
                             (:TOPLEVEL 1)
                             (:CHILD 2)
                             (:TEMP 3)
                             (:FOREIGN 4)
                             (:OFFSCREEN 5)
                             (:SUBSURFACE 6))
             (get-g-type-definition "GdkWindowType"))))

;;;     GdkWindowWindowClass

(test gdk-window-window-class
  ;; Check the type
  (is (g-type-is-enum "GdkWindowWindowClass"))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowWindowClass")
          (gtype (foreign-funcall "gdk_window_window_class_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-window-window-class
          (registered-enum-type "GdkWindowWindowClass")))
  ;; Check the names
  (is (equal '("GDK_INPUT_OUTPUT" "GDK_INPUT_ONLY")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkWindowWindowClass"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkWindowWindowClass"))))
  ;; Check the nick names
  (is (equal '("input-output" "input-only")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkWindowWindowClass"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowWindowClass"
                             GDK-WINDOW-WINDOW-CLASS
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_window_window_class_get_type")
                             (:INPUT-OUTPUT 0)
                             (:INPUT-ONLY 1))
             (get-g-type-definition "GdkWindowWindowClass"))))

;;;     GdkWindowHints

(test gdk-window-hints
  ;; Check the type
  (is (g-type-is-flags "GdkWindowHints"))
  ;; Check the registered name
  (is (eq 'gdk-window-hints
          (registered-flags-type "GdkWindowHints")))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowHints")
          (gtype (foreign-funcall "gdk_window_hints_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_HINT_POS" "GDK_HINT_MIN_SIZE" "GDK_HINT_MAX_SIZE"
               "GDK_HINT_BASE_SIZE" "GDK_HINT_ASPECT" "GDK_HINT_RESIZE_INC"
               "GDK_HINT_WIN_GRAVITY" "GDK_HINT_USER_POS" "GDK_HINT_USER_SIZE")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkWindowHints"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64 128 256)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkWindowHints"))))
  ;; Check the nick names
  (is (equal '("pos" "min-size" "max-size" "base-size" "aspect" "resize-inc"
               "win-gravity" "user-pos" "user-size")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkWindowHints"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWindowHints"
                              GDK-WINDOW-HINTS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_window_hints_get_type")
                              (:POS 1)
                              (:MIN-SIZE 2)
                              (:MAX-SIZE 4)
                              (:BASE-SIZE 8)
                              (:ASPECT 16)
                              (:RESIZE-INC 32)
                              (:WIN-GRAVITY 64)
                              (:USER-POS 128)
                              (:USER-SIZE 256))
             (get-g-type-definition "GdkWindowHints"))))

;;;     GdkGravity

(test gdk-gravity
  ;; Check the type
  (is (g-type-is-enum "GdkGravity"))
  ;; Check the type initializer
  (is (eq (gtype "GdkGravity")
          (gtype (foreign-funcall "gdk_gravity_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-gravity (registered-enum-type "GdkGravity")))
  ;; Check the names
  (is (equal '("GDK_GRAVITY_NORTH_WEST" "GDK_GRAVITY_NORTH"
               "GDK_GRAVITY_NORTH_EAST" "GDK_GRAVITY_WEST" "GDK_GRAVITY_CENTER"
               "GDK_GRAVITY_EAST" "GDK_GRAVITY_SOUTH_WEST" "GDK_GRAVITY_SOUTH"
               "GDK_GRAVITY_SOUTH_EAST" "GDK_GRAVITY_STATIC")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkGravity"))))
  ;; Check the values
  (is (equal '(1 2 3 4 5 6 7 8 9 10)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkGravity"))))
  ;; Check the nick names
  (is (equal '("north-west" "north" "north-east" "west" "center" "east"
               "south-west" "south" "south-east" "static")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkGravity"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkGravity"
                             GDK-GRAVITY
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_gravity_get_type")
                             (:NORTH-WEST 1)
                             (:NORTH 2)
                             (:NORTH-EAST 3)
                             (:WEST 4)
                             (:CENTER 5)
                             (:EAST 6)
                             (:SOUTH-WEST 7)
                             (:SOUTH 8)
                             (:SOUTH-EAST 9)
                             (:STATIC 10))
             (get-g-type-definition "GdkGravity"))))

;;;     GdkGeometry

(test gdk-geometry-structure
  ;; Slot names of the structure
  (is (equal '(GDK::TITLE GDK::EVENT-MASK GDK::X GDK::Y GDK::WIDTH GDK::HEIGHT
               GDK::WCLASS GDK::VISUAL GDK::WINDOW-TYPE GDK::CURSOR
               GDK::WMCLASS-NAME GDK::WMCLASS-CLASS GDK::OVERRIDE-REDIRECT
               GDK::TYPE-HINT)
             (foreign-slot-names '(:struct gdk-window-attr)))))

(test gdk-geometry-values
  (with-foreign-object (ptr '(:struct gdk-geometry))
    ;; Initialize the slots
    (setf (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::min-width) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::min-height) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-width) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-height) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-height) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::base-width) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::base-height) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::width-increment) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::height-increment) 0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::min-aspect) 0.0d0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-aspect) 0.0d0
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::win-gravity) 0)
    ;; Return a list with the coordinates
    (with-foreign-slots ((gdk::base-width
                          gdk::base-height
                          gdk::min-aspect
                          gdk::max-aspect)
                         ptr (:struct gdk-geometry))
      (is (equal '(0 0 0.0d0 0.0d0)
                 (list gdk::base-width
                       gdk::base-height
                       gdk::min-aspect
                       gdk::max-aspect))))))

(test make-gdk-geometry
  (let ((geometry (make-gdk-geometry :base-width 10
                                     :base-height 20
                                     :min-aspect 1.0d0
                                     :max-aspect 2.0d0)))
    (with-foreign-slots ((gdk::base-width
                          gdk::base-height
                          gdk::min-aspect gdk::max-aspect)
                         geometry (:struct gdk-geometry))
      (is (equal '(10 20 1.0d0 2.0d0)
                 (list gdk::base-width
                       gdk::base-height
                       gdk::min-aspect
                       gdk::max-aspect))))))

;;;     GdkAnchorHints

(test gdk-anchor-hints
  ;; Check the type
  (is (g-type-is-flags "GdkAnchorHints"))
  ;; Check the registered name
  (is (eq 'gdk-anchor-hints
          (registered-flags-type "GdkAnchorHints")))
  ;; Check the type initializer
  (is (eq (gtype "GdkAnchorHints")
          (gtype (foreign-funcall "gdk_anchor_hints_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_ANCHOR_FLIP_X" "GDK_ANCHOR_FLIP_Y" "GDK_ANCHOR_SLIDE_X"
               "GDK_ANCHOR_SLIDE_Y" "GDK_ANCHOR_RESIZE_X" "GDK_ANCHOR_RESIZE_Y"
               "GDK_ANCHOR_FLIP" "GDK_ANCHOR_SLIDE" "GDK_ANCHOR_RESIZE")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkAnchorHints"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 3 12 48)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkAnchorHints"))))
  ;; Check the nick names
  (is (equal '("flip-x" "flip-y" "slide-x" "slide-y" "resize-x" "resize-y"
               "flip" "slide" "resize")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkAnchorHints"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkAnchorHints"
                              GDK-ANCHOR-HINTS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_anchor_hints_get_type")
                              (:FLIP-X 1)
                              (:FLIP-Y 2)
                              (:SLIDE-X 4)
                              (:SLIDE-Y 8)
                              (:RESIZE-X 16)
                              (:RESIZE-Y 32)
                              (:FLIP 3)
                              (:SLIDE 12)
                              (:RESIZE 48))
             (get-g-type-definition "GdkAnchorHints"))))

;;;     GdkWindowEdge

(test gdk-window-edge
  ;; Check the type
  (is (g-type-is-enum "GdkWindowEdge"))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowEdge")
          (gtype (foreign-funcall "gdk_window_edge_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-window-edge (registered-enum-type "GdkWindowEdge")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_EDGE_NORTH_WEST" "GDK_WINDOW_EDGE_NORTH"
               "GDK_WINDOW_EDGE_NORTH_EAST" "GDK_WINDOW_EDGE_WEST"
               "GDK_WINDOW_EDGE_EAST" "GDK_WINDOW_EDGE_SOUTH_WEST"
               "GDK_WINDOW_EDGE_SOUTH" "GDK_WINDOW_EDGE_SOUTH_EAST")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkWindowEdge"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkWindowEdge"))))
  ;; Check the nick names
  (is (equal '("north-west" "north" "north-east" "west" "east" "south-west"
               "south" "south-east")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkWindowEdge"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowEdge"
                             GDK-WINDOW-EDGE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_window_edge_get_type")
                             (:NORTH-WEST 0)
                             (:NORTH 1)
                             (:NORTH-EAST 2)
                             (:WEST 3)
                             (:EAST 4)
                             (:SOUTH-WEST 5)
                             (:SOUTH 6)
                             (:SOUTH-EAST 7))
             (get-g-type-definition "GdkWindowEdge"))))

;;;     GdkWindowTypeHint

(test gdk-window-type-hint
  ;; Check the type
  (is (g-type-is-enum "GdkWindowTypeHint"))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowTypeHint")
          (gtype (foreign-funcall "gdk_window_type_hint_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-window-type-hint (registered-enum-type "GdkWindowTypeHint")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_TYPE_HINT_NORMAL" "GDK_WINDOW_TYPE_HINT_DIALOG"
               "GDK_WINDOW_TYPE_HINT_MENU" "GDK_WINDOW_TYPE_HINT_TOOLBAR"
               "GDK_WINDOW_TYPE_HINT_SPLASHSCREEN"
               "GDK_WINDOW_TYPE_HINT_UTILITY" "GDK_WINDOW_TYPE_HINT_DOCK"
               "GDK_WINDOW_TYPE_HINT_DESKTOP"
               "GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU"
               "GDK_WINDOW_TYPE_HINT_POPUP_MENU" "GDK_WINDOW_TYPE_HINT_TOOLTIP"
               "GDK_WINDOW_TYPE_HINT_NOTIFICATION" "GDK_WINDOW_TYPE_HINT_COMBO"
               "GDK_WINDOW_TYPE_HINT_DND")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkWindowTypeHint"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkWindowTypeHint"))))
  ;; Check the nick names
  (is (equal '("normal" "dialog" "menu" "toolbar" "splashscreen" "utility"
               "dock" "desktop" "dropdown-menu" "popup-menu" "tooltip"
               "notification" "combo" "dnd")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkWindowTypeHint"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowTypeHint"
                             GDK-WINDOW-TYPE-HINT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_window_type_hint_get_type")
                             (:NORMAL 0)
                             (:DIALOG 1)
                             (:MENU 2)
                             (:TOOLBAR 3)
                             (:SPLASHSCREEN 4)
                             (:UTILITY 5)
                             (:DOCK 6)
                             (:DESKTOP 7)
                             (:DROPDOWN-MENU 8)
                             (:POPUP-MENU 9)
                             (:TOOLTIP 10)
                             (:NOTIFICATION 11)
                             (:COMBO 12)
                             (:DND 13))
             (get-g-type-definition "GdkWindowTypeHint"))))

;;;     GdkWindowAttr

(test gdk-window-attr-structure
  ;; Slot names of the structure
  (is (equal '(GDK::TITLE
               GDK::EVENT-MASK
               GDK::X
               GDK::Y
               GDK::WIDTH
               GDK::HEIGHT
               GDK::WCLASS
               GDK::VISUAL
               GDK::WINDOW-TYPE
               GDK::CURSOR
               GDK::WMCLASS-NAME
               GDK::WMCLASS-CLASS
               GDK::OVERRIDE-REDIRECT
               GDK::TYPE-HINT)
             (foreign-slot-names '(:struct gdk-window-attr)))))

(test gdk-window-attr-values
  (with-foreign-object (ptr '(:struct gdk-window-attr))
    ;; Initialize the slots
    (setf (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::title) "title"
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::event-mask) nil
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::x) 10
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::y) 20)
    ;; Return a list with the coordinates
    (with-foreign-slots ((gdk::x gdk::y) ptr (:struct gdk-window-attr))
      (is (equal '(10 20)
                 (list gdk::x gdk::y))))))

(test make-gdk-window-attr
  (let ((attr (make-gdk-window-attr)))
    (with-foreign-slots ((gdk::x gdk::y) attr (:struct gdk-window-attr))
      (is (equal '(0 0)
                 (list gdk::x gdk::y))))))

;;;     GdkWindowAttributesType

(test gdk-window-attributes-type
  ;; Check the type
  (is (g-type-is-flags "GdkWindowAttributesType"))
  ;; Check the registered name
  (is (eq 'gdk-window-attributes-type
          (registered-flags-type "GdkWindowAttributesType")))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowAttributesType")
          (gtype (foreign-funcall "gdk_window_attributes_type_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_WA_TITLE" "GDK_WA_X" "GDK_WA_Y" "GDK_WA_CURSOR"
               "GDK_WA_VISUAL" "GDK_WA_WMCLASS" "GDK_WA_NOREDIR"
               "GDK_WA_TYPE_HINT")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkWindowAttributesType"))))
  ;; Check the values
  (is (equal '(2 4 8 16 32 64 128 256)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkWindowAttributesType"))))
  ;; Check the nick names
  (is (equal '("title" "x" "y" "cursor" "visual" "wmclass" "noredir"
               "type-hint")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkWindowAttributesType"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWindowAttributesType"
                              GDK-WINDOW-ATTRIBUTES-TYPE
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gdk_window_attributes_type_get_type")
                              (:TITLE 2)
                              (:X 4)
                              (:Y 8)
                              (:CURSOR 16)
                              (:VISUAL 32)
                              (:WMCLASS 64)
                              (:NOREDIR 128)
                              (:TYPE-HINT 256))
             (get-g-type-definition "GdkWindowAttributesType"))))

;;;     GdkFullscreenMode

(test gdk-fullscreen-mode
  ;; Check the type
  (is (g-type-is-enum "GdkFullscreenMode"))
  ;; Check the type initializer
  (is (eq (gtype "GdkFullscreenMode")
          (gtype (foreign-funcall "gdk_fullscreen_mode_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-fullscreen-mode (registered-enum-type "GdkFullscreenMode")))
  ;; Check the names
  (is (equal '("GDK_FULLSCREEN_ON_CURRENT_MONITOR"
               "GDK_FULLSCREEN_ON_ALL_MONITORS")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkFullscreenMode"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkFullscreenMode"))))
  ;; Check the nick names
  (is (equal '("current-monitor" "all-monitors")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkFullscreenMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkFullscreenMode"
                             GDK-FULLSCREEN-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_fullscreen_mode_get_type")
                             (:CURRENT-MONITOR 0)
                             (:ALL-MONITORS 1))
             (get-g-type-definition "GdkFullscreenMode"))))

;;;     GdkFilterReturn

(test gdk-filter-return
  ;; Check the type
  (is (g-type-is-enum "GdkFilterReturn"))
  ;; Check the type initializer
  (is (eq (gtype "GdkFilterReturn")
          (gtype (foreign-funcall "gdk_filter_return_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-filter-return (registered-enum-type "GdkFilterReturn")))
  ;; Check the names
  (is (equal '("GDK_FILTER_CONTINUE" "GDK_FILTER_TRANSLATE" "GDK_FILTER_REMOVE")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkFilterReturn"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkFilterReturn"))))
  ;; Check the nick names
  (is (equal '("continue" "translate" "remove")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkFilterReturn"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkFilterReturn"
                             GDK-FILTER-RETURN
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_filter_return_get_type")
                             (:CONTINUE 0)
                             (:TRANSLATE 1)
                             (:REMOVE 2))
             (get-g-type-definition "GdkFilterReturn"))))

;;;     GdkModifierIntent

(test gdk-modifier-intent
  ;; Check the type
  (is (g-type-is-enum "GdkModifierIntent"))
  ;; Check the type initializer
  (is (eq (gtype "GdkModifierIntent")
          (gtype (foreign-funcall "gdk_modifier_intent_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-modifier-intent (registered-enum-type "GdkModifierIntent")))
  ;; Check the names
  (is (equal '("GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR"
               "GDK_MODIFIER_INTENT_CONTEXT_MENU"
               "GDK_MODIFIER_INTENT_EXTEND_SELECTION"
               "GDK_MODIFIER_INTENT_MODIFY_SELECTION"
               "GDK_MODIFIER_INTENT_NO_TEXT_INPUT"
               "GDK_MODIFIER_INTENT_SHIFT_GROUP"
               "GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkModifierIntent"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkModifierIntent"))))
  ;; Check the nick names
  (is (equal '("primary-accelerator" "context-menu" "extend-selection"
               "modify-selection" "no-text-input" "shift-group"
               "default-mod-mask")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkModifierIntent"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkModifierIntent" GDK-MODIFIER-INTENT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_modifier_intent_get_type")
                             (:PRIMARY-ACCELERATOR 0)
                             (:CONTEXT-MENU 1)
                             (:EXTEND-SELECTION 2)
                             (:MODIFY-SELECTION 3)
                             (:NO-TEXT-INPUT 4)
                             (:SHIFT-GROUP 5)
                             (:DEFAULT-MOD-MASK 6))
             (get-g-type-definition "GdkModifierIntent"))))

;;;     GdkWMDecoration

(test gdk-wm-decoration
  ;; Check the type
  (is (g-type-is-flags "GdkWMDecoration"))
  ;; Check the registered name
  (is (eq 'gdk-wm-decoration
          (registered-flags-type "GdkWMDecoration")))
  ;; Check the type initializer
  (is (eq (gtype "GdkWMDecoration")
          (gtype (foreign-funcall "gdk_wm_decoration_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_DECOR_ALL" "GDK_DECOR_BORDER" "GDK_DECOR_RESIZEH"
               "GDK_DECOR_TITLE" "GDK_DECOR_MENU" "GDK_DECOR_MINIMIZE"
               "GDK_DECOR_MAXIMIZE")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkWMDecoration"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkWMDecoration"))))
  ;; Check the nick names
  (is (equal '("all" "border" "resizeh" "title" "menu" "minimize" "maximize")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkWMDecoration"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWMDecoration" GDK-W-M-DECORATION
                              (:EXPORT T)
                              (:ALL 1)
                              (:BORDER 2)
                              (:RESIZEH 4)
                              (:TITLE 8)
                              (:MENU 16)
                              (:MINIMIZE 32)
                              (:MAXIMIZE 64))
             (get-g-type-definition "GdkWMDecoration"))))

;;;     GdkWMFunction

(test gdk-wm-function
  ;; Check the type
  (is (g-type-is-flags "GdkWMFunction"))
  ;; Check the registered name
  (is (eq 'gdk-wm-function
          (registered-flags-type "GdkWMFunction")))
  ;; Check the type initializer
  (is (eq (gtype "GdkWMFunction")
          (gtype (foreign-funcall "gdk_wm_function_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_FUNC_ALL" "GDK_FUNC_RESIZE" "GDK_FUNC_MOVE"
               "GDK_FUNC_MINIMIZE" "GDK_FUNC_MAXIMIZE" "GDK_FUNC_CLOSE")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkWMFunction"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkWMFunction"))))
  ;; Check the nick names
  (is (equal '("all" "resize" "move" "minimize" "maximize" "close")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkWMFunction"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWMFunction" GDK-W-M-FUNCTION
                              (:EXPORT T)
                              (:ALL 1)
                              (:RESIZE 2)
                              (:MOVE 4)
                              (:MINIMIZE 8)
                              (:MAXIMIZE 16)
                              (:CLOSE 32))
             (get-g-type-definition "GdkWMFunction"))))

;;;     GdkWindow

(test gdk-window-class
  ;; Type check
  (is (g-type-is-object "GdkWindow"))
  ;; Check the registered name
  (is (eq 'gdk-window
          (registered-object-type-by-name "GdkWindow")))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindow")
           (gtype (foreign-funcall "gdk_window_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkWindow")))
  ;; Check the children
  (is (or (equal '("GdkX11Window" "GdkWaylandWindow")
                 (mapcar #'g-type-name (g-type-children "GdkWindow")))
          (equal '("GdkX11Window")
                 (mapcar #'g-type-name (g-type-children "GdkWindow")))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkWindow"))))
  ;; Check the class properties
  (is (equal '("cursor")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GdkWindow"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkWindow" GDK-WINDOW
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_window_get_type")
                       ((CURSOR GDK-WINDOW-CURSOR "cursor" "GdkCursor" T T)))
             (get-g-type-definition "GdkWindow"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-window-properties
  (let ((window (gdk-default-root-window))
        (display (gdk-display-default)))
    (is (typep (setf (gdk-window-cursor window)
                     (gdk-cursor-new-from-name display "pointer")) 'gdk-cursor))
    (is (typep (gdk-window-cursor window) 'gdk-cursor))))

;;; --- Signals ----------------------------------------------------------------

;;;     create-surface

;; Does not work, because CairoSurface cannot be created from the Lisp side.

#+nil
(test gdk-window-create-surface-signal
  (let* ((message nil)
         (window (gdk-default-root-window))
         (handler-id (g-signal-connect window "create-surface"
                       (lambda (object width height)
                         (setf message "Signal create-surface")
                         (is (typep object 'gdk-window))
                         (is (integerp width))
                         (is (integerp height))
                         nil))))
    ;; Emit the signal
    (is-true (g-signal-emit window "create-surface" 100 100))
    (is (string= "Signal create-surface" message))
    (is-false (g-signal-handler-disconnect window handler-id))))

;;;     from-embedder

#+nil
(test gdk-window-from-embedder-signal
  (with-foreign-objects ((offscreen-x :double) (offscreen-y :double))
    (let* ((message nil)
           (window (gdk-default-root-window))
           (handler-id (g-signal-connect window "from-embedder"
                         (lambda (object
                                  embedder-x embedder-y offscreen-x offscreen-y)
                           (setf message "Signal from-embedder")
                           (is (typep object 'gdk-window))
                           (is (typep embedder-x 'double-float))
                           (is (typep embedder-y 'double-float))
                           (is (pointerp offscreen-x))
                           (is (pointerp offscreen-y))
                           t))))
      ;; Emit the signal
      (is-false (g-signal-emit window
                               "from-embedder"
                               100 100 offscreen-x offscreen-y))
      (is (string= "Signal from-embedder" message))
      (is (typep (mem-ref offscreen-x :double) 'double-float))
      (is (typep (mem-ref offscreen-y :double) 'double-float))
      (is-false (g-signal-handler-disconnect window handler-id)))))

;;;     moved-to-rect

#+nil
(test gdk-window-move-to-rect-signal
  (with-foreign-objects ((flipped-rect '(g-boxed-foreign gdk-rectangle))
                         (final-rect '(g-boxed-foreign gdk-rectangle)))
    (let* ((message nil)
           (window (gdk-default-root-window))
           (handler-id (g-signal-connect window "moved-to-rect"
                         (lambda (object
                                  flipped-rect final-rect flipped-x flipped-y)
                           (setf message "Signal moved-to-rect")
                           (is (typep object 'gdk-window))
                           (is (pointerp flipped-rect))
                           (is (pointerp final-rect))
                           (is-false flipped-x)
                           (is-false flipped-y)
                           t))))
      ;; Emit the signal
      (is-false (g-signal-emit window
                               "moved-to-rect"
                               flipped-rect final-rect nil nil))
      (is (string= "Signal moved-to-rect" message))
      (is (typep (convert-from-foreign flipped-rect
                                       '(g-boxed-foreign gdk-rectangle))
                 'gdk-rectangle))
      (is (typep (convert-from-foreign final-rect
                                       '(g-boxed-foreign gdk-rectangle))
                 'gdk-rectangle))
      (is-false (g-signal-handler-disconnect window handler-id)))))

;;;     pick-embedded-child

#+nil
(test gdk-window-pick-embedded-child-signal
  (let* ((message nil)
         (window (gdk-default-root-window))
         (handler-id (g-signal-connect window "pick-embedded-child"
                       (lambda (object x y)
                         (setf message "Signal pick-embedded-child")
                         (is (typep object 'gdk-window))
                         (is (typep x 'double-float))
                         (is (typep y 'double-float))
                         nil))))
    ;; Emit the signal
    (is-false (g-signal-emit window "pick-embedded-child" 100.0d0 100.0d0))
    (is (string= "Signal pick-embedded-child" message))
    (is-false (g-signal-handler-disconnect window handler-id))))

;;;     to-embedder

#+nil
(test gdk-window-to-embedder-signal
  (with-foreign-objects ((embedder-x :double) (embedder-y :double))
    (let* ((message nil)
           (window (gdk-default-root-window))
           (handler-id (g-signal-connect window "to-embedder"
                         (lambda (object offscreen-x offscreen-y embedder-x embedder-y)
                           (setf message "Signal to-embedder")
                           (is (typep object 'gdk-window))
                           (is (typep offscreen-x 'double-float))
                           (is (typep offscreen-y 'double-float))
                           (is (pointerp embedder-x))
                           (is (pointerp embedder-y))
                           t))))
      ;; Emit the signal
      (is-false (g-signal-emit window "to-embedder" 100 100 embedder-x embedder-y))
      (is (string= "Signal to-embedder" message))
      (is (typep (mem-ref embedder-x :double) 'double-float))
      (is (typep (mem-ref embedder-y :double) 'double-float))
      (is-false (g-signal-handler-disconnect window handler-id)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-window-new

(test gdk-window-new
  (is (typep (gdk-window-new nil (make-gdk-window-attr) nil) 'gdk-window))
  (is (typep (gdk-window-new nil (make-gdk-window-attr) '(:x :y :title))
             'gdk-window)))

;;;     gdk-window-destroy

(test gdk-window-destroy
  (let ((window (gdk-window-new nil (make-gdk-window-attr) nil)))
    (is-false (gdk-window-destroy window))))

;;;     gdk-window-window-type

(test gdk-window-window-type.1
  (let ((window (gdk-window-new nil
                                (make-gdk-window-attr :window-type :toplevel)
                                nil)))
    (is (eq :toplevel (gdk-window-window-type window)))))

(test gdk-window-window-type.2
  (let ((window (gdk-window-new nil
                                (make-gdk-window-attr :window-type :child)
                                nil)))
    (is (eq :child (gdk-window-window-type window)))))

;;;     gdk-window-display

(test gdk-window-display
  (is (typep (gdk-window-display (gdk-default-root-window)) 'gdk-display)))

;;;     gdk-window-screen

(test gdk-window-screen
  (is (typep (gdk-window-screen (gdk-default-root-window)) 'gdk-screen)))

;;;     gdk-window-visual

(test gdk-window-visual
  (is (typep (gdk-window-visual (gdk-default-root-window)) 'gdk-visual)))

;;;     gdk_window_at_pointer                              deprecated
;;;     gdk_window_show
;;;     gdk_window_show_unraised
;;;     gdk_window_hide
;;;     gdk_window_is_destroyed
;;;     gdk_window_is_visible
;;;     gdk_window_is_viewable
;;;     gdk_window_is_input_only
;;;     gdk_window_is_shaped
;;;     gdk_window_get_state
;;;     gdk_window_withdraw
;;;     gdk_window_iconify
;;;     gdk_window_deiconify
;;;     gdk_window_stick
;;;     gdk_window_unstick
;;;     gdk_window_maximize
;;;     gdk_window_unmaximize
;;;     gdk_window_fullscreen
;;;     gdk_window_fullscreen_on_monitor
;;;     gdk_window_unfullscreen
;;;     gdk_window_get_fullscreen_mode
;;;     gdk_window_set_fullscreen_mode
;;;     gdk_window_set_keep_above
;;;     gdk_window_set_keep_below
;;;     gdk_window_set_opacity
;;;     gdk_window_set_composited                          deprecated
;;;     gdk_window_get_composited                          deprecated
;;;     gdk_window_set_pass_through
;;;     gdk_window_get_pass_through
;;;     gdk_window_move
;;;     gdk_window_resize
;;;     gdk_window_move_resize
;;;     gdk_window_scroll
;;;     gdk_window_move_to_rect
;;;     gdk_window_move_region
;;;     gdk_window_flush                                   deprecated
;;;     gdk_window_has_native
;;;     gdk_window_ensure_native
;;;     gdk_window_reparent
;;;     gdk_window_raise
;;;     gdk_window_lower
;;;     gdk_window_restack
;;;     gdk_window_focus
;;;     gdk_window_register_dnd
;;;     gdk_window_begin_resize_drag
;;;     gdk_window_begin_resize_drag_for_device
;;;     gdk_window_begin_move_drag
;;;     gdk_window_begin_move_drag_for_device
;;;     gdk_window_show_window_menu
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;     gdk_window_get_scale_factor
;;;     gdk_window_set_opaque_region
;;;     gdk_window_create_gl_context
;;;     gdk_window_mark_paint_from_clip
;;;     gdk_window_get_clip_region
;;;     gdk_window_begin_paint_rect                        deprecated
;;;     gdk_window_begin_paint_region                      deprecated
;;;     gdk_window_end_paint                               deprecated
;;;     gdk_window_begin_draw_frame
;;;     gdk_window_end_draw_frame
;;;     gdk_window_get_visible_region
;;;     GdkWindowInvalidateHandlerFunc
;;;     gdk_window_set_invalidate_handler
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     GdkWindowChildFunc
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates                     deprecated
;;;     gdk_window_process_updates                         deprecated
;;;     gdk_window_set_debug_updates                       deprecated
;;;     gdk_window_enable_synchronized_configure           deprecated
;;;     gdk_window_configure_finished                      deprecated
;;;     gdk_window_get_frame_clock
;;;     gdk_window_set_user_data
;;;     gdk_window_set_override_redirect
;;;     gdk_window_set_accept_focus
;;;     gdk_window_get_accept_focus
;;;     gdk_window_set_focus_on_map
;;;     gdk_window_get_focus_on_map
;;;     gdk_window_add_filter
;;;     gdk_window_remove_filter
;;;     gdk_window_shape_combine_region
;;;     gdk_window_set_child_shapes
;;;     gdk_window_merge_child_shapes
;;;     gdk_window_input_shape_combine_region
;;;     gdk_window_set_child_input_shapes
;;;     gdk_window_merge_child_input_shapes
;;;     gdk_window_set_static_gravities                    deprecated
;;;     gdk_window_set_title
;;;     gdk_window_set_background                          deprecated
;;;     gdk_window_set_background_rgba                     deprecated
;;;     gdk_window_set_background_pattern                  deprecated
;;;     gdk_window_get_background_pattern                  deprecated
;;;     gdk_window_set_cursor                              Accessor
;;;     gdk_window_get_cursor                              Accessor
;;;     gdk_window_get_user_data

;;;     gdk-window-geometry

(test gdk-window-geometry
  (let ((window (gdk-window-new nil
                                (make-gdk-window-attr :x 10
                                                      :y 20
                                                      :width 100
                                                      :height 200)
                                nil)))
    (is (equal '(0 0 100 200)
               (multiple-value-list (gdk-window-geometry window)))))
  (let ((window (gdk-window-new nil
                                (make-gdk-window-attr :x 10
                                                      :y 20
                                                      :width 100
                                                      :height 200)
                                '(:x :y))))
    (is (equal '(10 20 100 200)
        (multiple-value-list (gdk-window-geometry window))))))

;;;     gdk-window-set-geometry-hints

(test gdk-window-set-geometry-hints
  (let ((window (gdk-window-new nil (make-gdk-window-attr) nil)))
    (is-false (gdk-window-set-geometry-hints window (make-gdk-geometry) nil))))

;;;     gdk-window-width
;;;     gdk-window-height

(test gdk-window-width/height
  (let ((window (gdk-window-new nil
                                (make-gdk-window-attr :width 100 :height 200)
                                nil)))
    (is (= 100 (gdk-window-width window)))
    (is (= 200 (gdk-window-height window)))))

;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_shadow_width ()
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint

;;;     gdk-window-position

(test gdk-window-position
  (let ((window (gdk-window-new nil (make-gdk-window-attr) nil)))
    (is (equal '(0 0) (multiple-value-list (gdk-window-position window)))))
  (let ((window (gdk-window-new nil (make-gdk-window-attr :x 10 :y 20) '(:x :y))))
    (is (equal '(10 20) (multiple-value-list (gdk-window-position window))))))

;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_get_pointer                             deprecated
;;;     gdk_window_get_device_position
;;;     gdk_window_get_device_position_double
;;;     gdk_window_get_parent
;;;     gdk_window_get_toplevel
;;;     gdk_window_get_children
;;;     gdk_window_get_children_with_user_data
;;;     gdk_window_peek_children
;;;     gdk_window_get_events
;;;     gdk_window_set_events
;;;     gdk_window_set_icon_name
;;;     gdk_window_set_transient_for
;;;     gdk_window_set_role
;;;     gdk_window_set_startup_id
;;;     gdk_window_set_group
;;;     gdk_window_get_group
;;;     gdk_window_set_decorations
;;;     gdk_window_get_decorations
;;;     gdk_window_set_functions

;;;     gdk-default-root-window

(test gdk-default-root-window
  (let ((root-window (gdk-default-root-window)))
    (is (typep root-window 'gdk-window))
    (is-true (integerp (gdk-window-width root-window)))
    (is-true (integerp (gdk-window-height root-window)))))

;;;     gdk_window_get_support_multidevice
;;;     gdk_window_set_support_multidevice
;;;     gdk_window_get_device_cursor
;;;     gdk_window_set_device_cursor
;;;     gdk_window_get_device_events
;;;     gdk_window_set_device_events
;;;     gdk_window_get_source_events
;;;     gdk_window_set_source_events
;;;     gdk_window_get_event_compression
;;;     gdk_window_set_event_compression
;;;     gdk_offscreen_window_get_surface
;;;     gdk_offscreen_window_set_embedder
;;;     gdk_offscreen_window_get_embedder
;;;     gdk_window_geometry_changed

;;;     gdk-window-coords-from-parent

(test gdk-window-coords-from-parent
  (let ((window (gdk-default-root-window)))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk-window-coords-from-parent window 10.0d0 20.0d0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk-window-coords-from-parent window 10.0 20.0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk-window-coords-from-parent window 10 20))))))

;;;     gdk-window-coords-to-parent

(test gdk-window-coords-to-parent
  (let ((window (gdk-default-root-window)))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk-window-coords-to-parent window 10.0d0 20.0d0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk-window-coords-to-parent window 10.0 20.0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk-window-coords-to-parent window 10 20))))))

;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_effective_toplevel

;;; 2021-8-20
