(def-suite gdk-event-structures :in gdk-suite)
(in-suite gdk-event-structures)

;;;     GdkScrollDirection

(test gdk-scroll-direction
  ;; Check the type
  (is-true (g-type-is-enum "GdkScrollDirection"))
  ;; Check the type initializer
  (is (eq (gtype "GdkScrollDirection")
          (gtype (foreign-funcall "gdk_scroll_direction_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-scroll-direction
          (registered-enum-type "GdkScrollDirection")))
  ;; Check the names
  (is (equal '("GDK_SCROLL_UP" "GDK_SCROLL_DOWN" "GDK_SCROLL_LEFT"
               "GDK_SCROLL_RIGHT" "GDK_SCROLL_SMOOTH")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkScrollDirection"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkScrollDirection"))))
  ;; Check the nick names
  (is (equal '("up" "down" "left" "right" "smooth")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkScrollDirection"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkScrollDirection"
                             GDK-SCROLL-DIRECTION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_scroll_direction_get_type")
                             (:UP 0)
                             (:DOWN 1)
                             (:LEFT 2)
                             (:RIGHT 3)
                             (:SMOOTH 4))
             (get-g-type-definition "GdkScrollDirection"))))

;;;     GdkVisibilityState

(test gdk-visibility-state
  ;; Check the type
  (is-true (g-type-is-enum "GdkVisibilityState"))
  ;; Check the type initializer
  (is (eq (gtype "GdkVisibilityState")
          (gtype (foreign-funcall "gdk_visibility_state_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-visibility-state
          (registered-enum-type "GdkVisibilityState")))
  ;; Check the names
  (is (equal '("GDK_VISIBILITY_UNOBSCURED" "GDK_VISIBILITY_PARTIAL"
               "GDK_VISIBILITY_FULLY_OBSCURED")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkVisibilityState"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkVisibilityState"))))
  ;; Check the nick names
  (is (equal '("unobscured" "partial" "fully-obscured")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkVisibilityState"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkVisibilityState"
                             GDK-VISIBILITY-STATE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_visibility_state_get_type")
                             (:UNOBSCURED 0)
                             (:PARTIAL 1)
                             (:FULLY-OBSCURED 2))
             (get-g-type-definition "GdkVisibilityState"))))

;;;     GdkCrossingMode

(test gdk-crossing-mode
  ;; Check the type
  (is-true (g-type-is-enum "GdkCrossingMode"))
  ;; Check the type initializer
  (is (eq (gtype "GdkCrossingMode")
          (gtype (foreign-funcall "gdk_crossing_mode_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-crossing-mode
          (registered-enum-type "GdkCrossingMode")))
  ;; Check the names
  (is (equal '("GDK_CROSSING_NORMAL" "GDK_CROSSING_GRAB" "GDK_CROSSING_UNGRAB"
               "GDK_CROSSING_GTK_GRAB" "GDK_CROSSING_GTK_UNGRAB"
               "GDK_CROSSING_STATE_CHANGED" "GDK_CROSSING_TOUCH_BEGIN"
               "GDK_CROSSING_TOUCH_END" "GDK_CROSSING_DEVICE_SWITCH")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkCrossingMode"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkCrossingMode"))))
  ;; Check the nick names
  (is (equal '("normal" "grab" "ungrab" "gtk-grab" "gtk-ungrab" "state-changed"
               "touch-begin" "touch-end" "device-switch")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkCrossingMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkCrossingMode"
                             GDK-CROSSING-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_crossing_mode_get_type")
                             (:NORMAL 0)
                             (:GRAB 1)
                             (:UNGRAB 2)
                             (:GTK-GRAB 3)
                             (:GTK-UNGRAB 4)
                             (:STATE-CHANGED 5)
                             (:TOUCH-BEGIN 6)
                             (:TOUCH-END 7)
                             (:DEVICE-SWITCH 8))
             (get-g-type-definition "GdkCrossingMode"))))

;;;     GdkNotifyType

(test gdk-notify-type
  ;; Check the type
  (is-true (g-type-is-enum "GdkNotifyType"))
  ;; Check the type initializer
  (is (eq (gtype "GdkNotifyType")
          (gtype (foreign-funcall "gdk_notify_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-notify-type (registered-enum-type "GdkNotifyType")))
  ;; Check the names
  (is (equal '("GDK_NOTIFY_ANCESTOR" "GDK_NOTIFY_VIRTUAL" "GDK_NOTIFY_INFERIOR"
               "GDK_NOTIFY_NONLINEAR" "GDK_NOTIFY_NONLINEAR_VIRTUAL"
               "GDK_NOTIFY_UNKNOWN")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkNotifyType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkNotifyType"))))
  ;; Check the nick names
  (is (equal '("ancestor" "virtual" "inferior" "nonlinear" "nonlinear-virtual"
               "unknown")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkNotifyType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkNotifyType"
                             GDK-NOTIFY-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_notify_type_get_type")
                             (:ANCESTOR 0)
                             (:VIRTUAL 1)
                             (:INFERIOR 2)
                             (:NONLINEAR 3)
                             (:NONLINEAR-VIRTUAL 4)
                             (:UNKNOWN 5))
             (get-g-type-definition "GdkNotifyType"))))

;;;     GdkPropertyState

(test gdk-property-state
  ;; Check the type
  (is-true (g-type-is-enum "GdkPropertyState"))
  ;; Check the type initializer
  (is (eq (gtype "GdkPropertyState")
          (gtype (foreign-funcall "gdk_property_state_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-property-state (registered-enum-type "GdkPropertyState")))
  ;; Check the names
  (is (equal '("GDK_PROPERTY_NEW_VALUE" "GDK_PROPERTY_DELETE")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkPropertyState"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkPropertyState"))))
  ;; Check the nick names
  (is (equal '("new-value" "delete")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkPropertyState"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkPropertyState"
                             GDK-PROPERTY-STATE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_property_state_get_type")
                             (:NEW-VALUE 0)
                             (:DELETE 1))
             (get-g-type-definition "GdkPropertyState"))))

;;;     GdkWindowState

(test gdk-window-state
  ;; Check the type
  (is (g-type-is-flags "GdkWindowState"))
  ;; Check the registered name
  (is (eq 'gdk-window-state
          (registered-flags-type "GdkWindowState")))
  ;; Check the type initializer
  (is (eq (gtype "GdkWindowState")
          (gtype (foreign-funcall "gdk_window_state_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_WINDOW_STATE_WITHDRAWN" "GDK_WINDOW_STATE_ICONIFIED"
               "GDK_WINDOW_STATE_MAXIMIZED" "GDK_WINDOW_STATE_STICKY"
               "GDK_WINDOW_STATE_FULLSCREEN" "GDK_WINDOW_STATE_ABOVE"
               "GDK_WINDOW_STATE_BELOW" "GDK_WINDOW_STATE_FOCUSED"
               "GDK_WINDOW_STATE_TILED" "GDK_WINDOW_STATE_TOP_TILED"
               "GDK_WINDOW_STATE_TOP_RESIZABLE" "GDK_WINDOW_STATE_RIGHT_TILED"
               "GDK_WINDOW_STATE_RIGHT_RESIZABLE"
               "GDK_WINDOW_STATE_BOTTOM_TILED"
               "GDK_WINDOW_STATE_BOTTOM_RESIZABLE"
               "GDK_WINDOW_STATE_LEFT_TILED" "GDK_WINDOW_STATE_LEFT_RESIZABLE")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkWindowState"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768
               65536)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkWindowState"))))
  ;; Check the nick names
  (is (equal '("withdrawn" "iconified" "maximized" "sticky" "fullscreen" "above"
               "below" "focused" "tiled" "top-tiled" "top-resizable"
               "right-tiled" "right-resizable" "bottom-tiled" "bottom-resizable"
               "left-tiled" "left-resizable")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkWindowState"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWindowState"
                              GDK-WINDOW-STATE
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_window_state_get_type")
                              (:WITHDRAWN 1)
                              (:ICONIFIED 2)
                              (:MAXIMIZED 4)
                              (:STICKY 8)
                              (:FULLSCREEN 16)
                              (:ABOVE 32)
                              (:BELOW 64)
                              (:FOCUSED 128)
                              (:TILED 256)
                              (:TOP-TILED 512)
                              (:TOP-RESIZABLE 1024)
                              (:RIGHT-TILED 2048)
                              (:RIGHT-RESIZABLE 4096)
                              (:BOTTOM-TILED 8192)
                              (:BOTTOM-RESIZABLE 16384)
                              (:LEFT-TILED 32768)
                              (:LEFT-RESIZABLE 65536))
             (get-g-type-definition "GdkWindowState"))))

;;;     GdkSettingAction

(test gdk-setting-action
  ;; Check the type
  (is-true (g-type-is-enum "GdkSettingAction"))
  ;; Check the type initializer
  (is (eq (gtype "GdkSettingAction")
          (gtype (foreign-funcall "gdk_setting_action_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-setting-action (registered-enum-type "GdkSettingAction")))
  ;; Check the names
  (is (equal '("GDK_SETTING_ACTION_NEW" "GDK_SETTING_ACTION_CHANGED"
               "GDK_SETTING_ACTION_DELETED")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkSettingAction"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkSettingAction"))))
  ;; Check the nick names
  (is (equal '("new" "changed" "deleted")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkSettingAction"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkSettingAction"
                             GDK-SETTING-ACTION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_setting_action_get_type")
                             (:NEW 0)
                             (:CHANGED 1)
                             (:DELETED 2))
             (get-g-type-definition "GdkSettingAction"))))

;;;     GdkOwnerChange

(test gdk-owner-change
  ;; Check the type
  (is-true (g-type-is-enum "GdkOwnerChange"))
  ;; Check the type initializer
  (is (eq (gtype "GdkOwnerChange")
          (gtype (foreign-funcall "gdk_owner_change_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-owner-change (registered-enum-type "GdkOwnerChange")))
  ;; Check the names
  (is (equal '("GDK_OWNER_CHANGE_NEW_OWNER" "GDK_OWNER_CHANGE_DESTROY"
               "GDK_OWNER_CHANGE_CLOSE")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkOwnerChange"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkOwnerChange"))))
  ;; Check the nick names
  (is (equal '("new-owner" "destroy" "close")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkOwnerChange"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkOwnerChange"
                             GDK-OWNER-CHANGE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_owner_change_get_type")
                             (:NEW-OWNER 0)
                             (:DESTROY 1)
                             (:CLOSE 2))
             (get-g-type-definition "GdkOwnerChange"))))

;;;     GdkEventType        <-- gdk.events.lisp

(test gdk-event-type
  ;; Check the type
  (is-true (g-type-is-enum "GdkEventType"))
  ;; Check the type initializer
  (is (eq (gtype "GdkEventType")
          (gtype (foreign-funcall "gdk_event_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-event-type (registered-enum-type "GdkEventType")))
  ;; Check the names
  (is (equal '("GDK_NOTHING" "GDK_DELETE" "GDK_DESTROY" "GDK_EXPOSE"
               "GDK_MOTION_NOTIFY" "GDK_BUTTON_PRESS" "GDK_2BUTTON_PRESS"
               "GDK_DOUBLE_BUTTON_PRESS" "GDK_3BUTTON_PRESS"
               "GDK_TRIPLE_BUTTON_PRESS" "GDK_BUTTON_RELEASE" "GDK_KEY_PRESS"
               "GDK_KEY_RELEASE" "GDK_ENTER_NOTIFY" "GDK_LEAVE_NOTIFY"
               "GDK_FOCUS_CHANGE" "GDK_CONFIGURE" "GDK_MAP" "GDK_UNMAP"
               "GDK_PROPERTY_NOTIFY" "GDK_SELECTION_CLEAR"
               "GDK_SELECTION_REQUEST" "GDK_SELECTION_NOTIFY" "GDK_PROXIMITY_IN"
               "GDK_PROXIMITY_OUT" "GDK_DRAG_ENTER" "GDK_DRAG_LEAVE"
               "GDK_DRAG_MOTION" "GDK_DRAG_STATUS" "GDK_DROP_START"
               "GDK_DROP_FINISHED" "GDK_CLIENT_EVENT" "GDK_VISIBILITY_NOTIFY"
               "GDK_SCROLL" "GDK_WINDOW_STATE" "GDK_SETTING" "GDK_OWNER_CHANGE"
               "GDK_GRAB_BROKEN" "GDK_DAMAGE" "GDK_TOUCH_BEGIN"
               "GDK_TOUCH_UPDATE" "GDK_TOUCH_END" "GDK_TOUCH_CANCEL"
               "GDK_TOUCHPAD_SWIPE" "GDK_TOUCHPAD_PINCH" "GDK_PAD_BUTTON_PRESS"
               "GDK_PAD_BUTTON_RELEASE" "GDK_PAD_RING" "GDK_PAD_STRIP"
               "GDK_PAD_GROUP_MODE" "GDK_EVENT_LAST")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkEventType"))))
  ;; Check the values
  (is (equal '(-1 0 1 2 3 4 5 5 6 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
               22 23 24 25 26 27 28 29 31 32 33 34 35 36 37 38 39 40 41 42 43
               44 45 46 47 48)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkEventType"))))
  ;; Check the nick names
  (is (equal '("nothing" "delete" "destroy" "expose" "motion-notify"
               "button-press" "2button-press" "double-button-press"
               "3button-press" "triple-button-press" "button-release"
               "key-press" "key-release" "enter-notify" "leave-notify"
               "focus-change" "configure" "map" "unmap" "property-notify"
               "selection-clear" "selection-request" "selection-notify"
               "proximity-in" "proximity-out" "drag-enter" "drag-leave"
               "drag-motion" "drag-status" "drop-start" "drop-finished"
               "client-event" "visibility-notify" "scroll" "window-state"
               "setting" "owner-change" "grab-broken" "damage" "touch-begin"
               "touch-update" "touch-end" "touch-cancel" "touchpad-swipe"
               "touchpad-pinch" "pad-button-press" "pad-button-release"
               "pad-ring" "pad-strip" "pad-group-mode" "event-last")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkEventType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkEventType"
                             GDK-EVENT-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_event_type_get_type")
                             (:NOTHING -1)
                             (:DELETE 0)
                             (:DESTROY 1)
                             (:EXPOSE 2)
                             (:MOTION-NOTIFY 3)
                             (:BUTTON-PRESS 4)
                             (:2BUTTON-PRESS 5)
                             (:DOUBLE-BUTTON-PRESS 5)
                             (:3BUTTON-PRESS 6)
                             (:TRIPLE-BUTTON-PRESS 6)
                             (:BUTTON-RELEASE 7)
                             (:KEY-PRESS 8)
                             (:KEY-RELEASE 9)
                             (:ENTER-NOTIFY 10)
                             (:LEAVE-NOTIFY 11)
                             (:FOCUS-CHANGE 12)
                             (:CONFIGURE 13)
                             (:MAP 14)
                             (:UNMAP 15)
                             (:PROPERTY-NOTIFY 16)
                             (:SELECTION-CLEAR 17)
                             (:SELECTION-REQUEST 18)
                             (:SELECTION-NOTIFY 19)
                             (:PROXIMITY-IN 20)
                             (:PROXIMITY-OUT 21)
                             (:DRAG-ENTER 22)
                             (:DRAG-LEAVE 23)
                             (:DRAG-MOTION 24)
                             (:DRAG-STATUS 25)
                             (:DROP-START 26)
                             (:DROP-FINISHED 27)
                             (:CLIENT-EVENT 28)
                             (:VISIBILITY-NOTIFY 29)
                             (:SCROLL 31)
                             (:WINDOW-STATE 32)
                             (:SETTING 33)
                             (:OWNER-CHANGE 34)
                             (:GRAB-BROKEN 35)
                             (:DAMAGE 36)
                             (:TOUCH-BEGIN 37)
                             (:TOUCH-UPDATE 38)
                             (:TOUCH-END 39)
                             (:TOUCH-CANCEL 40)
                             (:TOUCHPAD-SWIPE 41)
                             (:TOUCHPAD-PINCH 42)
                             (:PAD-BUTTON-PRESS 43)
                             (:PAD-BUTTON-RELEASE 44)
                             (:PAD-RING 45)
                             (:PAD-STRIP 46)
                             (:PAD-GROUP-MODE 47)
                             (:EVENT-LAST 48))
             (get-g-type-definition "GdkEventType"))))

;;;     GdkModifierType     <-- gdk.window.lisp

(test gdk-modifier-type
  ;; Check the type
  (is (g-type-is-flags "GdkModifierType"))
  ;; Check the registered name
  (is (eq 'gdk-modifier-type
          (registered-flags-type "GdkModifierType")))
  ;; Check the type initializer
  (is (eq (gtype "GdkModifierType")
          (gtype (foreign-funcall "gdk_modifier_type_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_SHIFT_MASK" "GDK_LOCK_MASK" "GDK_CONTROL_MASK"
               "GDK_MOD1_MASK" "GDK_MOD2_MASK" "GDK_MOD3_MASK" "GDK_MOD4_MASK"
               "GDK_MOD5_MASK" "GDK_BUTTON1_MASK" "GDK_BUTTON2_MASK"
               "GDK_BUTTON3_MASK" "GDK_BUTTON4_MASK" "GDK_BUTTON5_MASK"
               "GDK_MODIFIER_RESERVED_13_MASK" "GDK_MODIFIER_RESERVED_14_MASK"
               "GDK_MODIFIER_RESERVED_15_MASK" "GDK_MODIFIER_RESERVED_16_MASK"
               "GDK_MODIFIER_RESERVED_17_MASK" "GDK_MODIFIER_RESERVED_18_MASK"
               "GDK_MODIFIER_RESERVED_19_MASK" "GDK_MODIFIER_RESERVED_20_MASK"
               "GDK_MODIFIER_RESERVED_21_MASK" "GDK_MODIFIER_RESERVED_22_MASK"
               "GDK_MODIFIER_RESERVED_23_MASK" "GDK_MODIFIER_RESERVED_24_MASK"
               "GDK_MODIFIER_RESERVED_25_MASK" "GDK_SUPER_MASK" "GDK_HYPER_MASK"
               "GDK_META_MASK" "GDK_MODIFIER_RESERVED_29_MASK"
               "GDK_RELEASE_MASK" "GDK_MODIFIER_MASK")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkModifierType"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768
               65536 131072 262144 524288 1048576 2097152 4194304 8388608
               16777216 33554432 67108864 134217728 268435456 536870912
               1073741824 1543512063)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkModifierType"))))
  ;; Check the nick names
  (is (equal '("shift-mask" "lock-mask" "control-mask" "mod1-mask" "mod2-mask"
               "mod3-mask" "mod4-mask" "mod5-mask" "button1-mask" "button2-mask"
               "button3-mask" "button4-mask" "button5-mask"
               "modifier-reserved-13-mask" "modifier-reserved-14-mask"
               "modifier-reserved-15-mask" "modifier-reserved-16-mask"
               "modifier-reserved-17-mask" "modifier-reserved-18-mask"
               "modifier-reserved-19-mask" "modifier-reserved-20-mask"
               "modifier-reserved-21-mask" "modifier-reserved-22-mask"
               "modifier-reserved-23-mask" "modifier-reserved-24-mask"
               "modifier-reserved-25-mask" "super-mask" "hyper-mask" "meta-mask"
               "modifier-reserved-29-mask" "release-mask" "modifier-mask")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkModifierType"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkModifierType"
                              GDK-MODIFIER-TYPE
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_modifier_type_get_type")
                              (:SHIFT-MASK 1)
                              (:LOCK-MASK 2)
                              (:CONTROL-MASK 4)
                              (:MOD1-MASK 8)
                              (:MOD2-MASK 16)
                              (:MOD3-MASK 32)
                              (:MOD4-MASK 64)
                              (:MOD5-MASK 128)
                              (:BUTTON1-MASK 256)
                              (:BUTTON2-MASK 512)
                              (:BUTTON3-MASK 1024)
                              (:BUTTON4-MASK 2048)
                              (:BUTTON5-MASK 4096)
                              (:MODIFIER-RESERVED-13-MASK 8192)
                              (:MODIFIER-RESERVED-14-MASK 16384)
                              (:MODIFIER-RESERVED-15-MASK 32768)
                              (:MODIFIER-RESERVED-16-MASK 65536)
                              (:MODIFIER-RESERVED-17-MASK 131072)
                              (:MODIFIER-RESERVED-18-MASK 262144)
                              (:MODIFIER-RESERVED-19-MASK 524288)
                              (:MODIFIER-RESERVED-20-MASK 1048576)
                              (:MODIFIER-RESERVED-21-MASK 2097152)
                              (:MODIFIER-RESERVED-22-MASK 4194304)
                              (:MODIFIER-RESERVED-23-MASK 8388608)
                              (:MODIFIER-RESERVED-24-MASK 16777216)
                              (:MODIFIER-RESERVED-25-MASK 33554432)
                              (:SUPER-MASK 67108864)
                              (:HYPER-MASK 134217728)
                              (:META-MASK 268435456)
                              (:MODIFIER-RESERVED-29-MASK 536870912)
                              (:RELEASE-MASK 1073741824)
                              (:MODIFIER-MASK 1543512063))
             (get-g-type-definition "GdkModifierType"))))

;;;     GdkEventMask        <-- gdk.events.lisp

(test gdk-event-mask
  ;; Check the type
  (is (g-type-is-flags "GdkEventMask"))
  ;; Check the registered name
  (is (eq 'gdk-event-mask
          (registered-flags-type "GdkEventMask")))
  ;; Check the type initializer
  (is (eq (gtype "GdkEventMask")
          (gtype (foreign-funcall "gdk_event_mask_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_EXPOSURE_MASK" "GDK_POINTER_MOTION_MASK"
               "GDK_POINTER_MOTION_HINT_MASK" "GDK_BUTTON_MOTION_MASK"
               "GDK_BUTTON1_MOTION_MASK" "GDK_BUTTON2_MOTION_MASK"
               "GDK_BUTTON3_MOTION_MASK" "GDK_BUTTON_PRESS_MASK"
               "GDK_BUTTON_RELEASE_MASK" "GDK_KEY_PRESS_MASK"
               "GDK_KEY_RELEASE_MASK" "GDK_ENTER_NOTIFY_MASK"
               "GDK_LEAVE_NOTIFY_MASK" "GDK_FOCUS_CHANGE_MASK"
               "GDK_STRUCTURE_MASK" "GDK_PROPERTY_CHANGE_MASK"
               "GDK_VISIBILITY_NOTIFY_MASK" "GDK_PROXIMITY_IN_MASK"
               "GDK_PROXIMITY_OUT_MASK" "GDK_SUBSTRUCTURE_MASK"
               "GDK_SCROLL_MASK" "GDK_TOUCH_MASK" "GDK_SMOOTH_SCROLL_MASK"
               "GDK_TOUCHPAD_GESTURE_MASK" "GDK_TABLET_PAD_MASK"
               "GDK_ALL_EVENTS_MASK")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkEventMask"))))
  ;; Check the values
  (is (equal '(2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536
               131072 262144 524288 1048576 2097152 4194304 8388608 16777216
               33554432 67108862)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkEventMask"))))
  ;; Check the nick names
  (is (equal '("exposure-mask" "pointer-motion-mask" "pointer-motion-hint-mask"
               "button-motion-mask" "button1-motion-mask" "button2-motion-mask"
               "button3-motion-mask" "button-press-mask" "button-release-mask"
               "key-press-mask" "key-release-mask" "enter-notify-mask"
               "leave-notify-mask" "focus-change-mask" "structure-mask"
               "property-change-mask" "visibility-notify-mask"
               "proximity-in-mask" "proximity-out-mask" "substructure-mask"
               "scroll-mask" "touch-mask" "smooth-scroll-mask"
               "touchpad-gesture-mask" "tablet-pad-mask" "all-events-mask")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkEventMask"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkEventMask"
                              GDK-EVENT-MASK
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_event_mask_get_type")
                              (:EXPOSURE-MASK 2)
                              (:POINTER-MOTION-MASK 4)
                              (:POINTER-MOTION-HINT-MASK 8)
                              (:BUTTON-MOTION-MASK 16)
                              (:BUTTON1-MOTION-MASK 32)
                              (:BUTTON2-MOTION-MASK 64)
                              (:BUTTON3-MOTION-MASK 128)
                              (:BUTTON-PRESS-MASK 256)
                              (:BUTTON-RELEASE-MASK 512)
                              (:KEY-PRESS-MASK 1024)
                              (:KEY-RELEASE-MASK 2048)
                              (:ENTER-NOTIFY-MASK 4096)
                              (:LEAVE-NOTIFY-MASK 8192)
                              (:FOCUS-CHANGE-MASK 16384)
                              (:STRUCTURE-MASK 32768)
                              (:PROPERTY-CHANGE-MASK 65536)
                              (:VISIBILITY-NOTIFY-MASK 131072)
                              (:PROXIMITY-IN-MASK 262144)
                              (:PROXIMITY-OUT-MASK 524288)
                              (:SUBSTRUCTURE-MASK 1048576)
                              (:SCROLL-MASK 2097152)
                              (:TOUCH-MASK 4194304)
                              (:SMOOTH-SCROLL-MASK 8388608)
                              (:TOUCHPAD-GESTURE-MASK 16777216)
                              (:TABLET-PAD-MASK 33554432)
                              (:ALL-EVENTS-MASK 67108862))
             (get-g-type-definition "GdkEventMask"))))

;;;     GdkEventSequence    <-- gdk-events.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (foreign-funcall "gdk_event_sequence_get_type" g-size))

(test gdk-event-sequence
  ;; Type check
  (is-true (g-type-is-a (gtype "GdkEventSequence") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GdkEventSequence")
          (gtype (foreign-funcall "gdk_event_sequence_get_type" g-size)))))

;;;     GdkEvent
;;;     GdkEventAny

;;  (type gdk-event-type)
;;  (window (g-object gdk-window))
;;  (send-event (:boolean :int8))

;;  (:nothing -1)
;;  (:delete 0)
;;  (:destroy 1)
;;  (:map 14)
;;  (:unmap 15)
;;  (:client-event 28)
;;  (:not-used 30)          ; not used
;;  (:damage 36)

(test gdk-event-any.1
  (let ((event (gdk-event-new :nothing)))
    (is (typep event 'gdk-event))
    ;; Common slots
    (is (eq :nothing (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))))

(test gdk-event-any.2
  (let ((event (make-gdk-event :type :nothing)))
    (is (typep event 'gdk-event))
    ;; Common slots
    (is (eq :nothing (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))))

;;    ;; GdkEventExpose
;;    ((:expose) gdk-event-expose
;;     (area gdk-rectangle :inline t :initform (make-gdk-rectangle))
;;     (region (:pointer (:struct cairo-region-t)) :initform (null-pointer))
;;     (count :int :initform 0))

(test gdk-event-expose.1
  (let ((event (gdk-event-new :expose)))
    (is (typep event 'gdk-event-expose))
    ;; Common slots
    (is (eq :expose (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-expose
    (is (typep (gdk-event-expose-area event) 'gdk-rectangle))
    (is-true (null-pointer-p (gdk-event-expose-region event)))
    (is (= 0 (gdk-event-expose-count event)))))

(test gdk-event-expose.2
  (let ((event (make-gdk-event-expose)))
    (is (typep event 'gdk-event-expose))
    ;; Common slots
    (is (eq :expose (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-expose
    (is (typep (gdk-event-expose-area event) 'gdk-rectangle))
    (is (null-pointer-p (gdk-event-expose-region event)))
    (is (= 0 (gdk-event-expose-count event)))))

;;    ;; GdkEventMotion
;;    ((:motion-notify) gdk-event-motion
;;     (time :uint32 :initform 0)
;;     (x :double :initform 0.0d0)
;;     (y :double :initform 0.0d0)
;;     (axes (fixed-array :double 2) :initform '(0.0d0 0.0d0))
;;     (state gdk-modifier-type :initform 0)
;;     (is-hint :int16 :initform 0)
;;     (device (g-object gdk-device) :initform (null-pointer))
;;     (x-root :double :initform 0.0d0)
;;     (y-root :double :initform 0.0d0))

(test gdk-event-motion.1
  (let ((event (gdk-event-new :motion-notify)))
    (is (typep event 'gdk-event-motion))
    ;; Common slots
    (is (eq :motion-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-motion
    (is (= 0 (gdk-event-motion-time event)))
    (is (= 0.0d0 (gdk-event-motion-x event)))
    (is (= 0.0d0 (gdk-event-motion-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk-event-motion-axes event)))
    (is (= 0 (gdk-event-motion-state event)))
    (is (= 0 (gdk-event-motion-is-hint event)))
    (is (null-pointer-p (gdk-event-motion-device event)))
    (is (= 0.0d0 (gdk-event-motion-x-root event)))
    (is (= 0.0d0 (gdk-event-motion-y-root event)))))

(test gdk-event-motion.2
  (let ((event (make-gdk-event-motion)))
    (is (typep event 'gdk-event-motion))
    ;; Common slots
    (is (eq :motion-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-motion
    (is (= 0 (gdk-event-motion-time event)))
    (is (= 0.0d0 (gdk-event-motion-x event)))
    (is (= 0.0d0 (gdk-event-motion-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk-event-motion-axes event)))
    (is (= 0 (gdk-event-motion-state event)))
    (is (= 0 (gdk-event-motion-is-hint event)))
    (is (null-pointer-p (gdk-event-motion-device event)))
    (is (= 0.0d0 (gdk-event-motion-x-root event)))
    (is (= 0.0d0 (gdk-event-motion-y-root event)))))

;;    ;; GdkEventButton
;;    ((:button-press
;;      :2button-press
;;      :double-button-press
;;      :3button-press
;;      :triple-button-press
;;      :button-release) gdk-event-button
;;     (time :uint32 :initform 0)
;;     (x :double :initform 0.0d0)
;;     (y :double :initform 0.0d0)
;;     (axes (fixed-array :double 2) :initform '(0.0d0 0.0d0))
;;     (state gdk-modifier-type :initform 0)
;;     (button :uint :initform 0)
;;     (device (g-object gdk-device) :initform (null-pointer))
;;     (x-root :double :initform 0.0d0)
;;     (y-root :double :initform 0.0d0))

(test gdk-event-button.1
  (let ((event (gdk-event-new :button-press)))
    (is (typep event 'gdk-event-button))
    ;; Common slots
    (is (eq :button-press (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-button
    (is (= 0 (gdk-event-button-time event)))
    (is (= 0.0d0 (gdk-event-button-x event)))
    (is (= 0.0d0 (gdk-event-button-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk-event-button-axes event)))
    (is (= 0 (gdk-event-button-state event)))
    (is (= 0 (gdk-event-button-button event)))
    (is (null-pointer-p (gdk-event-button-device event)))
    (is (= 0.0d0 (gdk-event-button-x-root event)))
    (is (= 0.0d0 (gdk-event-button-y-root event)))))

(test gdk-event-button.2
  (let ((event (make-gdk-event-button :type :button-press)))
    (is (typep event 'gdk-event-button))
    ;; Common slots
    (is (eq :button-press (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-button
    (is (= 0 (gdk-event-button-time event)))
    (is (= 0.0d0 (gdk-event-button-x event)))
    (is (= 0.0d0 (gdk-event-button-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk-event-button-axes event)))
    (is (= 0 (gdk-event-button-state event)))
    (is (= 0 (gdk-event-button-button event)))
    (is (null-pointer-p (gdk-event-button-device event)))
    (is (= 0.0d0 (gdk-event-button-x-root event)))
    (is (= 0.0d0 (gdk-event-button-y-root event)))))

;;    ;; GdkEventKey
;;    ((:key-press :key-release) gdk-event-key
;;     (time :uint32 :initform 0)
;;     (state gdk-modifier-type :initform 0)
;;     (keyval :uint :initform 0)
;;     (length :int :initform 0)
;;     (string (:string :free-from-foreign nil
;;                      :free-to-foreign nil)
;;             :initform "")
;;     (hardware-keycode :uint16 :initform 0)
;;     (group :uint8 :initform 0)
;;     (is-modifier :uint :initform 0))

(test gdk-event-key.1
  (let ((event (gdk-event-new :key-press)))
    (is (typep event 'gdk-event-key))
    ;; Common slots
    (is (eq :key-press (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-key
    (is (= 0 (gdk-event-key-time event)))
    (is (= 0 (gdk-event-key-state event)))
    (is (= 0 (gdk-event-key-keyval event)))
    (is (= 0 (gdk-event-key-length event)))
    (is (string= "" (gdk-event-key-string event)))
    (is (= 0 (gdk-event-key-hardware-keycode event)))
    (is (= 0 (gdk-event-key-group event)))
    (is (= 0 (gdk-event-key-is-modifier event)))))

(test gdk-event-key.2
  (let ((event (make-gdk-event-key :type :key-press)))
    (is (typep event 'gdk-event-key))
    ;; Common slots
    (is (eq :key-press (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-key
    (is (= 0 (gdk-event-key-time event)))
    (is (= 0 (gdk-event-key-state event)))
    (is (= 0 (gdk-event-key-keyval event)))
    (is (= 0 (gdk-event-key-length event)))
    (is (string= "" (gdk-event-key-string event)))
    (is (= 0 (gdk-event-key-hardware-keycode event)))
    (is (= 0 (gdk-event-key-group event)))
    (is (= 0 (gdk-event-key-is-modifier event)))))

;;            ;; GdkEventCrossing
;;            ((:enter-notify :leave-notify) gdk-event-crossing
;;             (subwindow (g-object gdk-window))
;;             (time :uint32)
;;             (x :double)
;;             (y :double)
;;             (x-root :double)
;;             (y-root :double)
;;             (mode gdk-crossing-mode)
;;             (detail gdk-notify-type)
;;             (focus :boolean)
;;             (state :uint))

(test gdk-event-crossing.1
  (let ((event (gdk-event-new :enter-notify)))
    (is (typep event 'gdk-event-crossing))
    ;; Common slots
    (is (eq :enter-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-crossing
    (is (null-pointer-p (gdk-event-crossing-subwindow event)))
    (is (= 0 (gdk-event-crossing-time event)))
    (is (= 0.0d0 (gdk-event-crossing-x event)))
    (is (= 0.0d0 (gdk-event-crossing-y event)))
    (is (= 0.0d0 (gdk-event-crossing-x-root event)))
    (is (= 0.0d0 (gdk-event-crossing-y-root event)))
    (is (eq :normal (gdk-event-crossing-mode event)))
    (is (eq :ancestor (gdk-event-crossing-detail event)))
    (is-false (gdk-event-crossing-focus event))
    (is (= 0 (gdk-event-crossing-state event)))))

(test gdk-event-crossing.2
  (let ((event (make-gdk-event-crossing :type :enter-notify)))
    (is (typep event 'gdk-event-crossing))
    ;; Common slots
    (is (eq :enter-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-crossing
    (is (null-pointer-p (gdk-event-crossing-subwindow event)))
    (is (= 0 (gdk-event-crossing-time event)))
    (is (= 0.0d0 (gdk-event-crossing-x event)))
    (is (= 0.0d0 (gdk-event-crossing-y event)))
    (is (= 0.0d0 (gdk-event-crossing-x-root event)))
    (is (= 0.0d0 (gdk-event-crossing-y-root event)))
    (is (eq :normal (gdk-event-crossing-mode event)))
    (is (eq :ancestor (gdk-event-crossing-detail event)))
    (is-false (gdk-event-crossing-focus event))
    (is (= 0 (gdk-event-crossing-state event)))))

;;    ;; GdkEventFocus
;;    ((:focus-change) gdk-event-focus
;;     (in :int16 :initform 0))

(test gdk-event-focus.1
  (let ((event (gdk-event-new :focus-change)))
    (is (typep event 'gdk-event-focus))
    ;; Common slots
    (is (eq :focus-change (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-focus
    (is (= 0 (gdk-event-focus-in event)))))

(test gdk-event-focus.2
  (let ((event (make-gdk-event-focus)))
    (is (typep event 'gdk-event-focus))
    ;; Common slots
    (is (eq :focus-change (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-focus
    (is (= 0 (gdk-event-focus-in event)))))

;;    ;; GdkEventConfigure
;;    ((:configure) gdk-event-configure
;;     (x :int :initform 0)
;;     (y :int :initform 0)
;;     (width :int :initform 0)
;;     (height :int :initform 0))

(test gdk-event-configure.1
  (let ((event (gdk-event-new :configure)))
    (is (typep event 'gdk-event-configure))
    ;; Common slots
    (is (eq :configure (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-configure
    (is (= 0 (gdk-event-configure-x event)))
    (is (= 0 (gdk-event-configure-y event)))
    (is (= 0 (gdk-event-configure-width event)))
    (is (= 0 (gdk-event-configure-height event)))))

(test gdk-event-configure.2
  (let ((event (make-gdk-event-configure)))
    (is (typep event 'gdk-event-configure))
    ;; Common slots
    (is (eq :configure (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-configure
    (is (= 0 (gdk-event-configure-x event)))
    (is (= 0 (gdk-event-configure-y event)))
    (is (= 0 (gdk-event-configure-width event)))
    (is (= 0 (gdk-event-configure-height event)))))

;;    ;; GdkEventProperty
;;    ((:property-notify) gdk-event-property
;;     (atom gdk-atom :init-form (null-pointer))
;;     (time :uint32 :initform 0)
;;     (state gdk-property-state :init-from :new-value))

(test gdk-event-property.1
  (let ((event (gdk-event-new :property-notify)))
    (is (typep event 'gdk-event-property))
    ;; Common slots
    (is (eq :property-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-property
    (is (null-pointer-p (gdk-event-property-atom event)))
    (is (= 0 (gdk-event-property-time event)))
    (is (eq :new-value (gdk-event-property-state event)))))

(test gdk-event-property.2
  (let ((event (make-gdk-event-property)))
    (is (typep event 'gdk-event-property))
    ;; Common slots
    (is (eq :property-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-property
    (is (null-pointer-p (gdk-event-property-atom event)))
    (is (= 0 (gdk-event-property-time event)))
    (is (eq :new-value (gdk-event-property-state event)))))

;;    ;; GdkEventSelection
;;    ((:selection-clear
;;      :selection-notify
;;      :selection-request) gdk-event-selection
;;     (selection gdk-atom :initform (null-pointer))
;;     (target gdk-atom :initform (null-poiner))
;;     (property gdk-atom :initform (null-pointer))
;;     (time :uint32 :initform 0)
;;     (requestor (g-object gdk-window)))

(test gdk-event-selection.1
  (let ((event (gdk-event-new :selection-clear)))
    (is (typep event 'gdk-event-selection))
    ;; Common slots
    (is (eq :selection-clear (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-selection
    (is (null-pointer-p (gdk-event-selection-selection event)))
    (is (null-pointer-p (gdk-event-selection-target event)))
    (is (null-pointer-p (gdk-event-selection-property event)))
    (is (= 0 (gdk-event-selection-time event)))
    (is-false (gdk-event-selection-requestor event))))

(test gdk-event-selection.2
  (let ((event (make-gdk-event-selection)))
    (is (typep event 'gdk-event-selection))
    ;; Common slots
    (is (eq :selection-clear (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-selection
    (is (null-pointer-p (gdk-event-selection-selection event)))
    (is (null-pointer-p (gdk-event-selection-target event)))
    (is (null-pointer-p (gdk-event-selection-property event)))
    (is (= 0 (gdk-event-selection-time event)))
    (is-false (gdk-event-selection-requestor event))))

;;    ;; GdkEventProximity
;;    ((:proximity-in
;;      :proximity-out) gdk-event-proximity
;;     (time :uint32 :initform 0)
;;     (device (g-object gdk-device)))

(test gdk-event-proximity.1
  (let ((event (gdk-event-new :proximity-in)))
    (is (typep event 'gdk-event-proximity))
    ;; Common slots
    (is (eq :proximity-in (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-proximity
    (is (= 0 (gdk-event-proximity-time event)))
    (is-false (gdk-event-proximity-device event))))

(test gdk-event-proximity.2
  (let ((event (make-gdk-event-proximity)))
    (is (typep event 'gdk-event-proximity))
    ;; Common slots
    (is (eq :proximity-in (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-proximity
    (is (= 0 (gdk-event-proximity-time event)))
    (is-false (gdk-event-proximity-device event))))

;;    ;; GdkEventDND
;;    ((:drag-enter
;;      :drag-leave
;;      :drag-motion
;;      :drag-status
;;      :drop-start
;;      :drop-finished) gdk-event-dnd
;;     (context (g-object gdk-drag-context))
;;     (time :uint32 :initform 0)
;;     (x-root :short :initform 0)
;;     (y-root :short :initform 0))

(test gdk-event-dnd.1
  (let ((event (gdk-event-new :drag-enter)))
    (is (typep event 'gdk-event-dnd))
    ;; Common slots
    (is (eq :drag-enter (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-dnd
    (is-false (gdk-event-dnd-context event))
    (is (= 0 (gdk-event-dnd-time event)))
    (is (= 0 (gdk-event-dnd-x-root event)))
    (is (= 0 (gdk-event-dnd-y-root event)))))

(test gdk-event-dnd.2
  (let ((event (make-gdk-event-dnd :type :drag-enter)))
    (is (typep event 'gdk-event-dnd))
    ;; Common slots
    (is (eq :drag-enter (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-dnd
    (is-false (gdk-event-dnd-context event))
    (is (= 0 (gdk-event-dnd-time event)))
    (is (= 0 (gdk-event-dnd-x-root event)))
    (is (= 0 (gdk-event-dnd-y-root event)))))

;;    ;; GdkEventVisibility
;;    ((:visibility-notify) gdk-event-visibility
;;     (state gdk-visibility-state :initform :unobscured))

(test gdk-event-visibility.1
  (let ((event (gdk-event-new :visibility-notify)))
    (is (typep event 'gdk-event-visibility))
    ;; Common slots
    (is (eq :visibility-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-visibility
    (is (eq :unobscured (gdk-event-visibility-state event)))))

(test gdk-event-visibility.2
  (let ((event (make-gdk-event-visibility :type :visibility-notify)))
    (is (typep event 'gdk-event-visibility))
    ;; Common slots
    (is (eq :visibility-notify (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-visibility
    (is (eq :unobscured (gdk-event-visibility-state event)))))

;;    ;; GdkEventScroll
;;    ((:scroll) gdk-event-scroll
;;     (time :uint32 :initform 0)
;;     (x :double :initform 0.0d0)
;;     (y :double :initform 0.0d0)
;;     (state gdk-modifier-type)
;;     (direction gdk-scroll-direction :initform :up)
;;     (device (g-object gdk-device))
;;     (x-root :double :initform 0.0d0)
;;     (y-root :double :initform 0.0d0)
;;     (delta-x :double :initform 0.0d0)
;;     (delta-y :double :initform 0.0d0))

(test gdk-event-scroll.1
  (let ((event (gdk-event-new :scroll)))
    (is (typep event 'gdk-event-scroll))
    ;; Common slots
    (is (eq :scroll (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-scroll
    (is (= 0 (gdk-event-scroll-time event)))
    (is (= 0 (gdk-event-scroll-x event)))
    (is (= 0 (gdk-event-scroll-y event)))
    (is-false (gdk-event-scroll-state event))
    (is (eq :up (gdk-event-scroll-direction event)))
    (is-false (gdk-event-scroll-device event))
    (is (= 0 (gdk-event-scroll-x-root event)))
    (is (= 0 (gdk-event-scroll-y-root event)))
    (is (= 0 (gdk-event-scroll-delta-x event)))
    (is (= 0 (gdk-event-scroll-delta-y event)))))

(test gdk-event-scroll.2
  (let ((event (make-gdk-event-scroll :type :scroll)))
    (is (typep event 'gdk-event-scroll))
    ;; Common slots
    (is (eq :scroll (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-scroll
    (is (= 0 (gdk-event-scroll-time event)))
    (is (= 0 (gdk-event-scroll-x event)))
    (is (= 0 (gdk-event-scroll-y event)))
    (is-false (gdk-event-scroll-state event))
    (is (eq :up (gdk-event-scroll-direction event)))
    (is-false (gdk-event-scroll-device event))
    (is (= 0 (gdk-event-scroll-x-root event)))
    (is (= 0 (gdk-event-scroll-y-root event)))
    (is (= 0 (gdk-event-scroll-delta-x event)))
    (is (= 0 (gdk-event-scroll-delta-y event)))))

;;    ;; GdkEventWindowState
;;    ((:window-state) gdk-event-window-state
;;     (changed-mask gdk-window-state)
;;     (new-window-state gdk-window-state))

(test gdk-event-window-state.1
  (let ((event (gdk-event-new :window-state)))
    (is (typep event 'gdk-event-window-state ))
    ;; Common slots
    (is (eq :window-state (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-window-state
    (is-false (gdk-event-window-state-changed-mask event))
    (is-false (gdk-event-window-state-new-window-state event))))

(test gdk-event-window-state.2
  (let ((event (make-gdk-event-window-state :type :window-state)))
    (is (typep event 'gdk-event-window-state))
    ;; Common slots
    (is (eq :window-state (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-window-state
    (is-false (gdk-event-window-state-changed-mask event))
    (is-false (gdk-event-window-state-new-window-state event))))

;;    ;; GdkEventSetting
;;    ((:setting) gdk-event-setting
;;     (action gdk-setting-action :initform :new)
;;     (name (:string :free-from-foreign nil :free-to-foreign nil)))

(test gdk-event-setting.1
  (let ((event (gdk-event-new :setting)))
    (is (typep event 'gdk-event-setting))
    ;; Common slots
    (is (eq :setting (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-setting
    (is (eq :new (gdk-event-setting-action event)))
    (is-false (gdk-event-setting-name event))))

(test gdk-event-setting.2
  (let ((event (make-gdk-event-setting :type :setting)))
    (is (typep event 'gdk-event-setting))
    ;; Common slots
    (is (eq :setting (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-setting
    (is (eq :new (gdk-event-setting-action event)))
    (is-false (gdk-event-setting-name event))))

;;    ;; GdkEventOwnerChange
;;    ((:owner-change) gdk-event-owner-change
;;     (owner (g-object gdk-window))
;;     (reason gdk-owner-change :initform :new-owner)
;;     (selection gdk-atom :initform (null-pointer))
;;     (time :uint32 :initform 0)
;;     (selection-time :uint32 :initform 0))

(test gdk-event-owner-change.1
  (let ((event (gdk-event-new :owner-change)))
    (is (typep event 'gdk-event-owner-change))
    ;; Common slots
    (is (eq :owner-change (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-owner-change
    (is-false (gdk-event-owner-change-owner event))
    (is (eq :new-owner (gdk-event-owner-change-reason event)))
    (is (null-pointer-p (gdk-event-owner-change-selection event)))
    (is (= 0 (gdk-event-owner-change-time event)))
    (is (= 0 (gdk-event-owner-change-selection-time event)))))

(test gdk-event-owner-change.2
  (let ((event (make-gdk-event-owner-change :type :owner-change)))
    (is (typep event 'gdk-event-owner-change))
    ;; Common slots
    (is (eq :owner-change (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-owner-change
    (is-false (gdk-event-owner-change-owner event))
    (is (eq :new-owner (gdk-event-owner-change-reason event)))
    (is (null-pointer-p (gdk-event-owner-change-selection event)))
    (is (= 0 (gdk-event-owner-change-time event)))
    (is (= 0 (gdk-event-owner-change-selection-time event)))))

;;    ;; GdkEventGrabBroken
;;    ((:grab-broken) gdk-event-grab-broken
;;     (keyboard :boolean)
;;     (implicit :boolean)
;;     (grab-window (g-object gdk-window)))

(test gdk-event-grab-broken.1
  (let ((event (gdk-event-new :grab-broken)))
    (is (typep event 'gdk-event-grab-broken))
    ;; Common slots
    (is (eq :grab-broken (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-grab-broken
    (is-false (gdk-event-grab-broken-keyboard event))
    (is-false (gdk-event-grab-broken-implicit event))
    (is-false (gdk-event-grab-broken-grab-window event))))

(test gdk-event-grab-broken.2
  (let ((event (make-gdk-event-grab-broken :type :grab-broken)))
    (is (typep event 'gdk-event-grab-broken))
    ;; Common slots
    (is (eq :grab-broken (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-grab-broken
    (is-false (gdk-event-grab-broken-keyboard event))
    (is-false (gdk-event-grab-broken-implicit event))
    (is-false (gdk-event-grab-broken-grab-window event))))

;;    ;; GdkEventTouch
;;    ((:touch-begin
;;      :touch-update
;;      :touch-end
;;      :touch-cancel) gdk-event-touch
;;     (time :uint32 :initform 0)
;;     (x :double :initform 0.0d0)
;;     (y :double :initform 0.0d0)
;;     (axes (fixed-array :double 2) :initform '(0.0d0 0.0d0))
;;     (state gdk-modifier-type)
;;     ;; FIXME: We can not initialize sequence from the Lisp side.
;;     (sequence (g-boxed-foreign gdk-event-sequence))
;;     (emulating-pointer :boolean)
;;     (device (g-object gdk-device))
;;     (x-root :double :initform 0.0d0)
;;     (y-root :double :initform 0.0d0))

(test gdk-event-touch.1
  (let ((event (gdk-event-new :touch-begin)))
    (is (typep event 'gdk-event-touch))
    ;; Common slots
    (is (eq :touch-begin (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-touch
    (is (= 0 (gdk-event-touch-time event)))
    (is (= 0 (gdk-event-touch-x event)))
    (is (= 0 (gdk-event-touch-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk-event-touch-axes event)))
    (is-false (gdk-event-touch-state event))
    ;; FIXME: We can not initialize sequence from Lisp side
    (is-false (gdk-event-touch-sequence event))
    (is-false (gdk-event-touch-emulating-pointer event))
    (is-false (gdk-event-touch-device event))
    (is (= 0 (gdk-event-touch-x-root event)))
    (is (= 0 (gdk-event-touch-y-root event)))))

(test gdk-event-touch.2
  (let ((event (make-gdk-event-touch :type :touch-begin)))
    (is (typep event 'gdk-event-touch))
    ;; Common slots
    (is (eq :touch-begin (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-touch
    (is (= 0 (gdk-event-touch-time event)))
    (is (= 0 (gdk-event-touch-x event)))
    (is (= 0 (gdk-event-touch-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk-event-touch-axes event)))
    (is-false (gdk-event-touch-state event))
    ;; FIXME: We can not initialize sequence from Lisp side
    (is-false (gdk-event-touch-sequence event))
    (is-false (gdk-event-touch-emulating-pointer event))
    (is-false (gdk-event-touch-device event))
    (is (= 0 (gdk-event-touch-x-root event)))
    (is (= 0 (gdk-event-touch-y-root event)))))

;;    ;; GdkEventTouchpadSwipe
;;    ((:touchpad-swipe) gdk-event-touchpad-swipe
;;     (phase :int8)
;;     (n-fingers :int8 :initform 0)
;;     (time :uint32 :initform 0)
;;     (x :double :initform 0.0d0)
;;     (y :double :initform 0.0d0)
;;     (dx :double :initform 0.0d0)
;;     (dy :double :initform 0.0d0)
;;     (x-root :double :initform 0.0d0)
;;     (y-root :double :initform 0.0d0)
;;     (state gdk-modifier-type))

(test gdk-event-touchpad-swipe.1
  (let ((event (gdk-event-new :touchpad-swipe)))
    (is (typep event 'gdk-event-touchpad-swipe))
    ;; Common slots
    (is (eq :touchpad-swipe (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-touchpad-swipe
    (is (= 0 (gdk-event-touchpad-swipe-phase event)))
    (is (= 0 (gdk-event-touchpad-swipe-n-fingers event)))
    (is (= 0 (gdk-event-touchpad-swipe-time event)))
    (is (= 0 (gdk-event-touchpad-swipe-x event)))
    (is (= 0 (gdk-event-touchpad-swipe-y event)))
    (is (= 0 (gdk-event-touchpad-swipe-dx event)))
    (is (= 0 (gdk-event-touchpad-swipe-dy event)))
    (is (= 0 (gdk-event-touchpad-swipe-x-root event)))
    (is (= 0 (gdk-event-touchpad-swipe-y-root event)))
    (is-false (gdk-event-touchpad-swipe-state event))))

(test gdk-event-touchpad-swipe.2
  (let ((event (make-gdk-event-touchpad-swipe :type :touchpad-swipe)))
    (is (typep event 'gdk-event-touchpad-swipe))
    ;; Common slots
    (is (eq :touchpad-swipe (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-touchpad-swipe
    (is (= 0 (gdk-event-touchpad-swipe-phase event)))
    (is (= 0 (gdk-event-touchpad-swipe-n-fingers event)))
    (is (= 0 (gdk-event-touchpad-swipe-time event)))
    (is (= 0 (gdk-event-touchpad-swipe-x event)))
    (is (= 0 (gdk-event-touchpad-swipe-y event)))
    (is (= 0 (gdk-event-touchpad-swipe-dx event)))
    (is (= 0 (gdk-event-touchpad-swipe-dy event)))
    (is (= 0 (gdk-event-touchpad-swipe-x-root event)))
    (is (= 0 (gdk-event-touchpad-swipe-y-root event)))
    (is-false (gdk-event-touchpad-swipe-state event))))

;;    ;; GdkEventTouchpadPinch
;;    ((:touchpad-pinch) gdk-event-touchpad-pinch
;;     (phase :int8 :initform 0)
;;     (n-fingers :int8 :initform 0)
;;     (time :uint32 :initform 0)
;;     (x :double :initform 0.0d0)
;;     (y :double :initform 0.0d0)
;;     (dx :double :initform 0.0d0)
;;     (dy :double :initform 0.0d0)
;;     (angle-delta :double :initform 0.0d0)
;;     (scale :double :initform 0.0d0)
;;     (x-root :double :initform 0.0d0)
;;     (y-root :double :initform 0.0d0)
;;     (state gdk-modifier-type))

(test gdk-event-touchpad-pinch.1
  (let ((event (gdk-event-new :touchpad-pinch)))
    (is (typep event 'gdk-event-touchpad-pinch))
    ;; Common slots
    (is (eq :touchpad-pinch (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-touchpad-pinch
    (is (= 0 (gdk-event-touchpad-pinch-phase event)))
    (is (= 0 (gdk-event-touchpad-pinch-n-fingers event)))
    (is (= 0 (gdk-event-touchpad-pinch-time event)))
    (is (= 0 (gdk-event-touchpad-pinch-x event)))
    (is (= 0 (gdk-event-touchpad-pinch-y event)))
    (is (= 0 (gdk-event-touchpad-pinch-dx event)))
    (is (= 0 (gdk-event-touchpad-pinch-dy event)))
    (is (= 0 (gdk-event-touchpad-pinch-angle-delta event)))
    (is (= 0 (gdk-event-touchpad-pinch-scale event)))
    (is (= 0 (gdk-event-touchpad-pinch-x-root event)))
    (is (= 0 (gdk-event-touchpad-pinch-y-root event)))
    (is-false (gdk-event-touchpad-pinch-state event))))

(test gdk-event-touchpad-pinch.2
  (let ((event (make-gdk-event-touchpad-pinch :type :touchpad-pinch)))
    (is (typep event 'gdk-event-touchpad-pinch))
    ;; Common slots
    (is (eq :touchpad-pinch (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-touchpad-pinch
    (is (= 0 (gdk-event-touchpad-pinch-phase event)))
    (is (= 0 (gdk-event-touchpad-pinch-n-fingers event)))
    (is (= 0 (gdk-event-touchpad-pinch-time event)))
    (is (= 0 (gdk-event-touchpad-pinch-x event)))
    (is (= 0 (gdk-event-touchpad-pinch-y event)))
    (is (= 0 (gdk-event-touchpad-pinch-dx event)))
    (is (= 0 (gdk-event-touchpad-pinch-dy event)))
    (is (= 0 (gdk-event-touchpad-pinch-angle-delta event)))
    (is (= 0 (gdk-event-touchpad-pinch-scale event)))
    (is (= 0 (gdk-event-touchpad-pinch-x-root event)))
    (is (= 0 (gdk-event-touchpad-pinch-y-root event)))
    (is-false (gdk-event-touchpad-pinch-state event))))

;;    ;; GdkEventPadButton
;;    ((:pad-button-press :pad-button-release) gdk-event-pad-button
;;     (time :uint32 :initform 0)
;;     (group :uint :initform 0)
;;     (button :uint :initform 0)
;;     (mode :uint :initform 0)) ; TODO: Check the type of mode

(test gdk-event-pad-button.1
  (let ((event (gdk-event-new :pad-button-press)))
    (is (typep event 'gdk-event-pad-button))
    ;; Common slots
    (is (eq :pad-button-press (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-pad-button
    (is (= 0 (gdk-event-pad-button-time event)))
    (is (= 0 (gdk-event-pad-button-group event)))
    (is (= 0 (gdk-event-pad-button-button event)))
    (is (= 0 (gdk-event-pad-button-mode event)))))

(test gdk-event-pad-button.2
  (let ((event (make-gdk-event-pad-button :type :pad-button-press)))
    (is (typep event 'gdk-event-pad-button))
    ;; Common slots
    (is (eq :pad-button-press (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-pad-button
    (is (= 0 (gdk-event-pad-button-time event)))
    (is (= 0 (gdk-event-pad-button-group event)))
    (is (= 0 (gdk-event-pad-button-button event)))
    (is (= 0 (gdk-event-pad-button-mode event)))))

;;    ;; GdkEventPadAxis
;;    ((:pad-ring :pad-strip) gdk-event-pad-axis
;;     (time :uint32 :initform 0)
;;     (group :uint :initform 0)
;;     (index :uint :initform 0)
;;     (mode :uint :initform 0)
;;     (value :double :initform 0.0d0))

(test gdk-event-pad-axis.1
  (let ((event (gdk-event-new :pad-ring)))
    (is (typep event 'gdk-event-pad-axis))
    ;; Common slots
    (is (eq :pad-ring (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-pad-axis
    (is (= 0 (gdk-event-pad-axis-time event)))
    (is (= 0 (gdk-event-pad-axis-group event)))
    (is (= 0 (gdk-event-pad-axis-index event)))
    (is (= 0 (gdk-event-pad-axis-mode event)))
    (is (= 0 (gdk-event-pad-axis-value event)))))

(test gdk-event-pad-axis.2
  (let ((event (make-gdk-event-pad-axis :type :pad-ring)))
    (is (typep event 'gdk-event-pad-axis))
    ;; Common slots
    (is (eq :pad-ring (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-pad-axis
    (is (= 0 (gdk-event-pad-axis-time event)))
    (is (= 0 (gdk-event-pad-axis-group event)))
    (is (= 0 (gdk-event-pad-axis-index event)))
    (is (= 0 (gdk-event-pad-axis-mode event)))
    (is (= 0 (gdk-event-pad-axis-value event)))))

;;    ;; GdkEventPadGroupMode
;;    ((:pad-group-mode) gdk-event-pad-group-mode
;;     (time :uint32 :initform 0)
;;     (group :uint :initform 0)
;;     (mode :uint :initform 0))

(test gdk-event-pad-group-mode.1
  (let ((event (gdk-event-new :pad-group-mode)))
    (is (typep event 'gdk-event-pad-group-mode))
    ;; Common slots
    (is (eq :pad-group-mode (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-pad-group-mode
    (is (= 0 (gdk-event-pad-group-mode-time event)))
    (is (= 0 (gdk-event-pad-group-mode-group event)))
    (is (= 0 (gdk-event-pad-group-mode-mode event)))))

(test gdk-event-pad-group-mode.2
  (let ((event (make-gdk-event-pad-group-mode :type :pad-group-mode)))
    (is (typep event 'gdk-event-pad-group-mode))
    ;; Common slots
    (is (eq :pad-group-mode (gdk-event-type event)))
    (is-false (gdk-event-window event))
    (is-false (gdk-event-send-event event))
    ;; Slots for gdk-event-pad-group-mode
    (is (= 0 (gdk-event-pad-group-mode-time event)))
    (is (= 0 (gdk-event-pad-group-mode-group event)))
    (is (= 0 (gdk-event-pad-group-mode-mode event)))))

;;; 2020-11-29
