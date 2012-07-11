;;; ----------------------------------------------------------------------------
;;; gdk.event-structures.lisp
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
;;; Event Structures
;;;
;;; Data structures specific to each type of event
;;;
;;; Synopsis
;;;
;;; typedef             GdkNativeWindow
;;;
;;; enum                GdkEventType
;;; enum                GdkScrollDirection
;;; enum                GdkVisibilityState
;;; enum                GdkCrossingMode
;;; enum                GdkNotifyType
;;; enum                GdkPropertyState
;;; enum                GdkWindowState
;;; enum                GdkSettingAction
;;; enum                GdkOwnerChange
;;;
;;; flags               GdkModifierType
;;;
;;; union               GdkEvent
;;;
;;; struct              GdkEventAny
;;; struct              GdkEventKey
;;; struct              GdkEventButton
;;; struct              GdkEventScroll
;;; struct              GdkEventMotion
;;; struct              GdkEventExpose
;;; struct              GdkEventVisibility
;;; struct              GdkEventCrossing
;;; struct              GdkEventFocus
;;; struct              GdkEventConfigure
;;; struct              GdkEventProperty
;;; struct              GdkEventSelection
;;; struct              GdkEventDND
;;; struct              GdkEventProximity
;;; struct              GdkEventClient
;;; struct              GdkEventNoExpose
;;; struct              GdkEventWindowState
;;; struct              GdkEventSetting
;;; struct              GdkEventOwnerChange
;;; struct              GdkEventGrabBroken
;;;
;;; Description
;;;
;;; The event structs contain data specific to each type of event in GDK.
;;;
;;; Note
;;;
;;; A common mistake is to forget to set the event mask of a widget so that
;;; the required events are received. See gtk_widget_set_events().
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;; "CFFI foreign type for an array of a fixed length. Slot element-type
;; specifies the type of elements and array-size specifies the size of array
;; (in elements).

(define-foreign-type fixed-array ()
  ((element-type :reader fixed-array-element-type
                 :initarg :element-type
                 :initform (error "Element type must be specified"))
   (array-size :reader fixed-array-array-size
               :initarg :array-size
               :initform (error "Array size must be specified")))
  (:actual-type :pointer)
  (:documentation ""))

(define-parse-method fixed-array (element-type array-size)
  (make-instance 'fixed-array
                 :element-type element-type
                 :array-size array-size))

(defmethod translate-from-foreign (ptr (type fixed-array))
  (when (not (null-pointer-p ptr))
    (let ((result (make-array (fixed-array-array-size type)))
          (el-type (fixed-array-element-type type)))
      (loop
         for i from 0 below (fixed-array-array-size type)
         do (setf (aref result i) (mem-aref ptr el-type i)))
      result)))

(defmethod translate-to-foreign (value (type fixed-array))
  (if (null value)
      (null-pointer)
      (foreign-alloc (fixed-array-element-type type)
                     :count (length value)
                     :initial-contents value)))

(defmethod free-translated-object (value (type fixed-array) param)
  (declare (ignore param))
  (unless (null-pointer-p value)
    (foreign-free value)))

;;; ----------------------------------------------------------------------------
;;; GdkNativeWindow
;;;
;;; typedef gpointer GdkNativeWindow;
;;;
;;; Used to represent native windows (Windows for the X11 backend, HWNDs for
;;; Win32).
;;; ----------------------------------------------------------------------------

;;;FIXME: Check correct type
#+ windows
(defctype gdk-native-window :pointer)
#- windows
(defctype gdk-native-window :uint32)

;;; ----------------------------------------------------------------------------
;;; enum GdkEventType
;;;
;;; typedef enum
;;; {
;;;   GDK_NOTHING               = -1,
;;;   GDK_DELETE                = 0,
;;;   GDK_DESTROY               = 1,
;;;   GDK_EXPOSE                = 2,
;;;   GDK_MOTION_NOTIFY         = 3,
;;;   GDK_BUTTON_PRESS          = 4,
;;;   GDK_2BUTTON_PRESS         = 5,
;;;   GDK_3BUTTON_PRESS         = 6,
;;;   GDK_BUTTON_RELEASE        = 7,
;;;   GDK_KEY_PRESS             = 8,
;;;   GDK_KEY_RELEASE           = 9,
;;;   GDK_ENTER_NOTIFY          = 10,
;;;   GDK_LEAVE_NOTIFY          = 11,
;;;   GDK_FOCUS_CHANGE          = 12,
;;;   GDK_CONFIGURE             = 13,
;;;   GDK_MAP                   = 14,
;;;   GDK_UNMAP                 = 15,
;;;   GDK_PROPERTY_NOTIFY       = 16,
;;;   GDK_SELECTION_CLEAR       = 17,
;;;   GDK_SELECTION_REQUEST     = 18,
;;;   GDK_SELECTION_NOTIFY      = 19,
;;;   GDK_PROXIMITY_IN          = 20,
;;;   GDK_PROXIMITY_OUT         = 21,
;;;   GDK_DRAG_ENTER            = 22,
;;;   GDK_DRAG_LEAVE            = 23,
;;;   GDK_DRAG_MOTION           = 24,
;;;   GDK_DRAG_STATUS           = 25,
;;;   GDK_DROP_START            = 26,
;;;   GDK_DROP_FINISHED         = 27,
;;;   GDK_CLIENT_EVENT          = 28,
;;;   GDK_VISIBILITY_NOTIFY     = 29,
;;;   GDK_NO_EXPOSE             = 30,
;;;   GDK_SCROLL                = 31,
;;;   GDK_WINDOW_STATE          = 32,
;;;   GDK_SETTING               = 33,
;;;   GDK_OWNER_CHANGE          = 34,
;;;   GDK_GRAB_BROKEN           = 35,
;;;   GDK_DAMAGE                = 36,
;;;   GDK_EVENT_LAST        /* helper variable for decls */
;;; } GdkEventType;
;;;
;;; Specifies the type of the event.
;;;
;;; Do not confuse these events with the signals that GTK+ widgets emit.
;;; Although many of these events result in corresponding signals being
;;; emitted, the events are often transformed or filtered along the way.
;;;
;;; GDK_NOTHING
;;;     A special code to indicate a null event.
;;;
;;; GDK_DELETE
;;;     The window manager has requested that the toplevel window be hidden or
;;;     destroyed, usually when the user clicks on a special icon in the title
;;;     bar.
;;;
;;; GDK_DESTROY
;;;     The window has been destroyed.
;;;
;;; GDK_EXPOSE
;;;     All or part of the window has become visible and needs to be redrawn.
;;;
;;; GDK_MOTION_NOTIFY
;;;     The pointer (usually a mouse) has moved.
;;;
;;; GDK_BUTTON_PRESS
;;;     A mouse button has been pressed.
;;;
;;; GDK_2BUTTON_PRESS
;;;     A mouse button has been double-clicked (clicked twice within a short
;;;     period of time). Note that each click also generates a GDK_BUTTON_PRESS
;;;     event.
;;;
;;; GDK_3BUTTON_PRESS
;;;     A mouse button has been clicked 3 times in a short period of time.
;;;     Note that each click also generates a GDK_BUTTON_PRESS event.
;;;
;;; GDK_BUTTON_RELEASE
;;;     A mouse button has been released.
;;;
;;; GDK_KEY_PRESS
;;;     A key has been pressed.
;;;
;;; GDK_KEY_RELEASE
;;;     A key has been released.
;;;
;;; GDK_ENTER_NOTIFY
;;;     The pointer has entered the window.
;;;
;;; GDK_LEAVE_NOTIFY
;;;     The pointer has left the window.
;;;
;;; GDK_FOCUS_CHANGE
;;;     The keyboard focus has entered or left the window.
;;;
;;; GDK_CONFIGURE
;;;     The size, position or stacking order of the window has changed. Note
;;;     that GTK+ discards these events for GDK_WINDOW_CHILD windows.
;;;
;;; GDK_MAP
;;;     The window has been mapped.
;;;
;;; GDK_UNMAP
;;;     The window has been unmapped.
;;;
;;; GDK_PROPERTY_NOTIFY
;;;     A property on the window has been changed or deleted.
;;;
;;; GDK_SELECTION_CLEAR
;;;     The application has lost ownership of a selection.
;;;
;;; GDK_SELECTION_REQUEST
;;;     Another application has requested a selection.
;;;
;;; GDK_SELECTION_NOTIFY
;;;     A selection has been received.
;;;
;;; GDK_PROXIMITY_IN
;;;     An input device has moved into contact with a sensing surface
;;;     (e.g. a touchscreen or graphics tablet).
;;;
;;; GDK_PROXIMITY_OUT
;;;     An input device has moved out of contact with a sensing surface.
;;;
;;; GDK_DRAG_ENTER
;;;     The mouse has entered the window while a drag is in progress.
;;;
;;; GDK_DRAG_LEAVE
;;;     The mouse has left the window while a drag is in progress.
;;;
;;; GDK_DRAG_MOTION
;;;     The mouse has moved in the window while a drag is in progress.
;;;
;;; GDK_DRAG_STATUS
;;;     The status of the drag operation initiated by the window has changed.
;;;
;;; GDK_DROP_START
;;;     A drop operation onto the window has started.
;;;
;;; GDK_DROP_FINISHED
;;;     The drop operation initiated by the window has completed.
;;;
;;; GDK_CLIENT_EVENT
;;;     A message has been received from another application.
;;;
;;; GDK_VISIBILITY_NOTIFY
;;;     The window visibility status has changed.
;;;
;;; GDK_NO_EXPOSE
;;;     Indicates that the source region was completely available when parts
;;;     of a drawable were copied. This is not very useful.
;;;
;;; GDK_SCROLL
;;;     The scroll wheel was turned.
;;;
;;; GDK_WINDOW_STATE
;;;     The state of a window has changed. See GdkWindowState for the possible
;;;     window states.
;;;
;;; GDK_SETTING
;;;     A setting has been modified.
;;;
;;; GDK_OWNER_CHANGE
;;;     The owner of a selection has changed. This event type was added in 2.6.
;;;
;;; GDK_GRAB_BROKEN
;;;     A pointer or keyboard grab was broken. This event type was added in 2.8.
;;;
;;; GDK_DAMAGE
;;;     The content of the window has been changed. This event type was added
;;;     in 2.14.
;;;
;;; GDK_EVENT_LAST
;;;     Marks the end of the GdkEventType enumeration. Added in 2.18.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkEventType" gdk-event-type
  ()
  (:nothing -1)
  (:delete 0)
  (:destroy 1)
  (:expose 2)
  (:motion-notify 3)
  (:button-press 4)
  (:2button-press 5)
  (:3button-press 6)
  (:button-release 7)
  (:key-press 8)
  (:key-release 9)
  (:enter-notify 10)
  (:leave-notify 11)
  (:focus-change 12)
  (:configure 13)
  (:map 14)
  (:unmap 15)
  (:property-notify 16)
  (:selection-clear 17)
  (:selection-request 18)
  (:selection-notify 19)
  (:proximity-in 20)
  (:proximity-out 21)
  (:drag-enter 22)
  (:drag-leave 23)
  (:drag-motion 24)
  (:drag-status 25)
  (:drop-start 26)
  (:drop-finished 27)
  (:client-event 28)
  (:visibility-notify 29)
  (:no-expose 30)
  (:scroll 31)
  (:window-state 32)
  (:setting 33)
  (:owner-change 34)
  (:grab-broken 35)
  (:damage 36))

;;; ----------------------------------------------------------------------------
;;; enum GdkScrollDirection
;;;
;;; typedef enum
;;; {
;;;   GDK_SCROLL_UP,
;;;   GDK_SCROLL_DOWN,
;;;   GDK_SCROLL_LEFT,
;;;   GDK_SCROLL_RIGHT
;;; } GdkScrollDirection;
;;;
;;; Specifies the direction for GdkEventScroll.
;;;
;;; GDK_SCROLL_UP
;;;     The window is scrolled up.
;;;
;;; GDK_SCROLL_DOWN
;;;     The window is scrolled down.
;;;
;;; GDK_SCROLL_LEFT
;;;     The window is scrolled to the left.
;;;
;;; GDK_SCROLL_RIGHT
;;;     The window is scrolled to the right.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkScrollDirection" gdk-scroll-direction
  ()
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3))

;;; ----------------------------------------------------------------------------
;;; enum GdkVisibilityState
;;;
;;; typedef enum
;;; {
;;;   GDK_VISIBILITY_UNOBSCURED,
;;;   GDK_VISIBILITY_PARTIAL,
;;;   GDK_VISIBILITY_FULLY_OBSCURED
;;; } GdkVisibilityState;
;;;
;;; Specifies the visiblity status of a window for a GdkEventVisibility.
;;;
;;; GDK_VISIBILITY_UNOBSCURED
;;;     The window is completely visible.
;;;
;;; GDK_VISIBILITY_PARTIAL
;;;     The window is partially visible.
;;;
;;; GDK_VISIBILITY_FULLY_OBSCURED
;;;     The window is not visible at all.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkVisibilityState" gdk-visibility-state
  ()
  (:unobscured 0)
  (:partial 1)
  (:fully-obscured 2))

;;; ----------------------------------------------------------------------------
;;; enum GdkCrossingMode
;;;
;;; typedef enum
;;; {
;;;   GDK_CROSSING_NORMAL,
;;;   GDK_CROSSING_GRAB,
;;;   GDK_CROSSING_UNGRAB,
;;;   GDK_CROSSING_GTK_GRAB,
;;;   GDK_CROSSING_GTK_UNGRAB,
;;;   GDK_CROSSING_STATE_CHANGED
;;; } GdkCrossingMode;
;;;
;;; Specifies the crossing mode for GdkEventCrossing.
;;;
;;; GDK_CROSSING_NORMAL
;;;     crossing because of pointer motion.
;;;
;;; GDK_CROSSING_GRAB
;;;     crossing because a grab is activated.
;;;
;;; GDK_CROSSING_UNGRAB
;;;     crossing because a grab is deactivated.
;;;
;;; GDK_CROSSING_GTK_GRAB
;;;     crossing because a GTK+ grab is activated.
;;;
;;; GDK_CROSSING_GTK_UNGRAB
;;;     crossing because a GTK+ grab is deactivated.
;;;
;;; GDK_CROSSING_STATE_CHANGED
;;;     crossing because a GTK+ widget changed state (e.g. sensitivity).
;;; ----------------------------------------------------------------------------

(defcenum gdk-crossing-mode
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed)

(export 'gdk-crossing-mode)

;;; ----------------------------------------------------------------------------
;;; enum GdkNotifyType
;;;
;;; typedef enum
;;; {
;;;   GDK_NOTIFY_ANCESTOR            = 0,
;;;   GDK_NOTIFY_VIRTUAL             = 1,
;;;   GDK_NOTIFY_INFERIOR            = 2,
;;;   GDK_NOTIFY_NONLINEAR           = 3,
;;;   GDK_NOTIFY_NONLINEAR_VIRTUAL   = 4,
;;;   GDK_NOTIFY_UNKNOWN             = 5
;;; } GdkNotifyType;
;;;
;;; Specifies the kind of crossing for GdkEventCrossing.
;;;
;;; See the X11 protocol specification of LeaveNotify for full details of
;;; crossing event generation.
;;;
;;; GDK_NOTIFY_ANCESTOR
;;;     the window is entered from an ancestor or left towards an ancestor.
;;;
;;; GDK_NOTIFY_VIRTUAL
;;;     the pointer moves between an ancestor and an inferior of the window.
;;;
;;; GDK_NOTIFY_INFERIOR
;;;     the window is entered from an inferior or left towards an inferior.
;;;
;;; GDK_NOTIFY_NONLINEAR
;;;     the window is entered from or left towards a window which is neither an
;;;     ancestor nor an inferior.
;;;
;;; GDK_NOTIFY_NONLINEAR_VIRTUAL
;;;     the pointer moves between two windows which are not ancestors of each
;;;     other and the window is part of the ancestor chain between one of these
;;;     windows and their least common ancestor.
;;;
;;; GDK_NOTIFY_UNKNOWN
;;;     an unknown type of enter/leave event occurred.
;;; ----------------------------------------------------------------------------

(defcenum gdk-notify-type
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)

(export 'gdk-notify-type)

;;; ----------------------------------------------------------------------------
;;; enum GdkPropertyState
;;;
;;; typedef enum
;;; {
;;;   GDK_PROPERTY_NEW_VALUE,
;;;   GDK_PROPERTY_DELETE
;;; } GdkPropertyState;
;;;
;;; Specifies the type of a property change for a GdkEventProperty.
;;;
;;; GDK_PROPERTY_NEW_VALUE
;;;     the property value was changed.
;;;
;;; GDK_PROPERTY_DELETE
;;;     the property was deleted.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkPropertyState" gdk-property-state
  ()
  :new-value
  :delete)

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierType
;;;
;;; typedef enum
;;; {
;;;   GDK_SHIFT_MASK    = 1 << 0,
;;;   GDK_LOCK_MASK     = 1 << 1,
;;;   GDK_CONTROL_MASK  = 1 << 2,
;;;   GDK_MOD1_MASK     = 1 << 3,
;;;   GDK_MOD2_MASK     = 1 << 4,
;;;   GDK_MOD3_MASK     = 1 << 5,
;;;   GDK_MOD4_MASK     = 1 << 6,
;;;   GDK_MOD5_MASK     = 1 << 7,
;;;   GDK_BUTTON1_MASK  = 1 << 8,
;;;   GDK_BUTTON2_MASK  = 1 << 9,
;;;   GDK_BUTTON3_MASK  = 1 << 10,
;;;   GDK_BUTTON4_MASK  = 1 << 11,
;;;   GDK_BUTTON5_MASK  = 1 << 12,
;;;
;;;   /* The next few modifiers are used by XKB, so we skip to the end.
;;;    * Bits 15 - 25 are currently unused. Bit 29 is used internally.
;;;    */
;;;
;;;   GDK_SUPER_MASK    = 1 << 26,
;;;   GDK_HYPER_MASK    = 1 << 27,
;;;   GDK_META_MASK     = 1 << 28,
;;;
;;;   GDK_RELEASE_MASK  = 1 << 30,
;;;
;;;   GDK_MODIFIER_MASK = 0x5c001fff
;;; } GdkModifierType;
;;;
;;; A set of bit-flags to indicate the state of modifier keys and mouse buttons
;;; in various event types. Typical modifier keys are Shift, Control, Meta,
;;; Super, Hyper, Alt, Compose, Apple, CapsLock or ShiftLock.
;;;
;;; Like the X Window System, GDK supports 8 modifier keys and 5 mouse buttons.
;;;
;;; Since 2.10, GDK recognizes which of the Meta, Super or Hyper keys are mapped
;;; to Mod2 - Mod5, and indicates this by setting GDK_SUPER_MASK,
;;; GDK_HYPER_MASK or GDK_META_MASK in the state field of key events.
;;;
;;; GDK_SHIFT_MASK
;;;     the Shift key.
;;;
;;; GDK_LOCK_MASK
;;;     a Lock key (depending on the modifier mapping of the X server this may
;;;     either be CapsLock or ShiftLock).
;;;
;;; GDK_CONTROL_MASK
;;;     the Control key.
;;;
;;; GDK_MOD1_MASK
;;;     the fourth modifier key (it depends on the modifier mapping of the
;;;     X server which key is interpreted as this modifier, but normally it is
;;;     the Alt key).
;;;
;;; GDK_MOD2_MASK
;;;     the fifth modifier key (it depends on the modifier mapping of the
;;;     X server which key is interpreted as this modifier).
;;;
;;; GDK_MOD3_MASK
;;;     the sixth modifier key (it depends on the modifier mapping of the
;;;     X server which key is interpreted as this modifier).
;;;
;;; GDK_MOD4_MASK
;;;     the seventh modifier key (it depends on the modifier mapping of the
;;;     X server which key is interpreted as this modifier).
;;;
;;; GDK_MOD5_MASK
;;;     the eighth modifier key (it depends on the modifier mapping of the
;;;     X server which key is interpreted as this modifier).
;;;
;;; GDK_BUTTON1_MASK
;;;     the first mouse button.
;;;
;;; GDK_BUTTON2_MASK
;;;     the second mouse button.
;;;
;;; GDK_BUTTON3_MASK
;;;     the third mouse button.
;;;
;;; GDK_BUTTON4_MASK
;;;     the fourth mouse button.
;;;
;;; GDK_BUTTON5_MASK
;;;     the fifth mouse button.
;;;
;;; GDK_SUPER_MASK
;;;     the Super modifier. Since 2.10
;;;
;;; GDK_HYPER_MASK
;;;     the Hyper modifier. Since 2.10
;;;
;;; GDK_META_MASK
;;;     the Meta modifier. Since 2.10
;;;
;;; GDK_RELEASE_MASK
;;;     not used in GDK itself. GTK+ uses it to differentiate between
;;;     (keyval, modifiers) pairs from key press and release events.
;;;
;;; GDK_MODIFIER_MASK
;;;     a mask covering all modifier types.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkModifierType" gdk-modifier-type
  (:export t
   :type-initializer "gdk_modifier_type_get_type")
  (:shift-mask 1)
  (:lock-mask 2)
  (:control-mask 4)
  (:mod1-mask 8)
  (:mod2-mask 16)
  (:mod3-mask 32)
  (:mod4-mask 64)
  (:mod5-mask 128)
  (:button1-mask 256)
  (:button2-mask 512)
  (:button3-mask 1024)
  (:button4-mask 2048)
  (:button5-mask 4096)
  (:super-mask 67108864)
  (:hyper-mask 134217728)
  (:meta-mask 268435456)
  (:release-mask 1073741824)
  (:modifier-mask 1543512063))

;;; ----------------------------------------------------------------------------
;;; enum GdkEventMask
;;;
;;; typedef enum
;;; {
;;;   GDK_EXPOSURE_MASK                 = 1 << 1,
;;;   GDK_POINTER_MOTION_MASK           = 1 << 2,
;;;   GDK_POINTER_MOTION_HINT_MASK      = 1 << 3,
;;;   GDK_BUTTON_MOTION_MASK            = 1 << 4,
;;;   GDK_BUTTON1_MOTION_MASK           = 1 << 5,
;;;   GDK_BUTTON2_MOTION_MASK           = 1 << 6,
;;;   GDK_BUTTON3_MOTION_MASK           = 1 << 7,
;;;   GDK_BUTTON_PRESS_MASK             = 1 << 8,
;;;   GDK_BUTTON_RELEASE_MASK           = 1 << 9,
;;;   GDK_KEY_PRESS_MASK                = 1 << 10,
;;;   GDK_KEY_RELEASE_MASK              = 1 << 11,
;;;   GDK_ENTER_NOTIFY_MASK             = 1 << 12,
;;;   GDK_LEAVE_NOTIFY_MASK             = 1 << 13,
;;;   GDK_FOCUS_CHANGE_MASK             = 1 << 14,
;;;   GDK_STRUCTURE_MASK                = 1 << 15,
;;;   GDK_PROPERTY_CHANGE_MASK          = 1 << 16,
;;;   GDK_VISIBILITY_NOTIFY_MASK        = 1 << 17,
;;;   GDK_PROXIMITY_IN_MASK             = 1 << 18,
;;;   GDK_PROXIMITY_OUT_MASK            = 1 << 19,
;;;   GDK_SUBSTRUCTURE_MASK             = 1 << 20,
;;;   GDK_SCROLL_MASK                   = 1 << 21,
;;;   GDK_ALL_EVENTS_MASK               = 0x3FFFFE
;;; } GdkEventMask;
;;;
;;; A set of bit-flags to indicate which events a window is to receive. Most
;;; of these masks map onto one or more of the GdkEventType event types above.
;;;
;;; GDK_POINTER_MOTION_HINT_MASK is a special mask which is used to reduce the
;;; number of GDK_MOTION_NOTIFY events received. Normally a GDK_MOTION_NOTIFY
;;; event is received each time the mouse moves. However, if the application
;;; spends a lot of time processing the event (updating the display, for
;;; example), it can lag behind the position of the mouse. When using
;;; GDK_POINTER_MOTION_HINT_MASK, fewer GDK_MOTION_NOTIFY events will be sent,
;;; some of which are marked as a hint (the is_hint member is TRUE). To receive
;;; more motion events after a motion hint event, the application needs to asks
;;; for more, by calling gdk_event_request_motions().
;;;
;;; GDK_EXPOSURE_MASK
;;;     receive expose events
;;;
;;; GDK_POINTER_MOTION_MASK
;;;     receive all pointer motion events
;;;
;;; GDK_POINTER_MOTION_HINT_MASK
;;;     see the explanation above
;;;
;;; GDK_BUTTON_MOTION_MASK
;;;     receive pointer motion events while any button is pressed
;;;
;;; GDK_BUTTON1_MOTION_MASK
;;;     receive pointer motion events while 1 button is pressed
;;;
;;; GDK_BUTTON2_MOTION_MASK
;;;     receive pointer motion events while 2 button is pressed
;;;
;;; GDK_BUTTON3_MOTION_MASK
;;;     receive pointer motion events while 3 button is pressed
;;;
;;; GDK_BUTTON_PRESS_MASK
;;;     receive button press events
;;;
;;; GDK_BUTTON_RELEASE_MASK
;;;     receive button release events
;;;
;;; GDK_KEY_PRESS_MASK
;;;     receive key press events
;;;
;;; GDK_KEY_RELEASE_MASK
;;;     receive key release events
;;;
;;; GDK_ENTER_NOTIFY_MASK
;;;     receive window enter events
;;;
;;; GDK_LEAVE_NOTIFY_MASK
;;;     receive window leave events
;;;
;;; GDK_FOCUS_CHANGE_MASK
;;;     receive focus change events
;;;
;;; GDK_STRUCTURE_MASK
;;;     receive events about window configuration change
;;;
;;; GDK_PROPERTY_CHANGE_MASK
;;;     receive property change events
;;;
;;; GDK_VISIBILITY_NOTIFY_MASK
;;;     receive visibility change events
;;;
;;; GDK_PROXIMITY_IN_MASK
;;;     receive proximity in events
;;;
;;; GDK_PROXIMITY_OUT_MASK
;;;     receive proximity out events
;;;
;;; GDK_SUBSTRUCTURE_MASK
;;;     receive events about window configuration changes of child windows
;;;
;;; GDK_SCROLL_MASK
;;;     receive scroll events
;;;
;;; GDK_ALL_EVENTS_MASK
;;;     the combination of all the above event masks.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkEventMask" gdk-event-mask
  ()
  (:exposure-mask 2)
  (:pointer-motion-mask 4)
  (:pointer-motion-hint-mask 8)
  (:button-motion-mask 16)
  (:button1-motion-mask 32)
  (:button2-motion-mask 64)
  (:button3-motion-mask 128)
  (:button-press-mask 256)
  (:button-release-mask 512)
  (:key-press-mask 1024)
  (:key-release-mask 2048)
  (:enter-notify-mask 4096)
  (:leave-notify-mask 8192)
  (:focus-change-mask 16384)
  (:structure-mask 32768)
  (:property-change-mask 65536)
  (:visibility-notify-mask 131072)
  (:proximity-in-mask 262144)
  (:proximity-out-mask 524288)
  (:substructure-mask 1048576)
  (:scroll-mask 2097152)
  (:all-events-mask 4194302))

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowState
;;;
;;; typedef enum
;;; {
;;;   GDK_WINDOW_STATE_WITHDRAWN  = 1 << 0,
;;;   GDK_WINDOW_STATE_ICONIFIED  = 1 << 1,
;;;   GDK_WINDOW_STATE_MAXIMIZED  = 1 << 2,
;;;   GDK_WINDOW_STATE_STICKY     = 1 << 3,
;;;   GDK_WINDOW_STATE_FULLSCREEN = 1 << 4,
;;;   GDK_WINDOW_STATE_ABOVE      = 1 << 5,
;;;   GDK_WINDOW_STATE_BELOW      = 1 << 6
;;; } GdkWindowState;
;;;
;;; Specifies the state of a toplevel window.
;;;
;;; GDK_WINDOW_STATE_WITHDRAWN
;;;     the window is not shown.
;;;
;;; GDK_WINDOW_STATE_ICONIFIED
;;;     the window is minimized.
;;;
;;; GDK_WINDOW_STATE_MAXIMIZED
;;;     the window is maximized.
;;;
;;; GDK_WINDOW_STATE_STICKY
;;;     the window is sticky.
;;;
;;; GDK_WINDOW_STATE_FULLSCREEN
;;;     the window is maximized without decorations.
;;;
;;; GDK_WINDOW_STATE_ABOVE
;;;     the window is kept above other windows.
;;;
;;; GDK_WINDOW_STATE_BELOW
;;;     the window is kept below other windows.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWindowState" gdk-window-state
  (:export t
   :type-initializer "gdk_window_state_get_type")
  (:withdrawn 1)
  (:iconified 2)
  (:maximized 4)
  (:sticky 8)
  (:fullscreen 16)
  (:above 32)
  (:below 64))

;;; ----------------------------------------------------------------------------
;;; enum GdkSettingAction
;;;
;;; typedef enum
;;; {
;;;   GDK_SETTING_ACTION_NEW,
;;;   GDK_SETTING_ACTION_CHANGED,
;;;   GDK_SETTING_ACTION_DELETED
;;; } GdkSettingAction;
;;;
;;; Specifies the kind of modification applied to a setting in a
;;; GdkEventSetting.
;;;
;;; GDK_SETTING_ACTION_NEW
;;;     a setting was added.
;;;
;;; GDK_SETTING_ACTION_CHANGED
;;;     a setting was changed.
;;;
;;; GDK_SETTING_ACTION_DELETED
;;;     a setting was deleted.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkSettingAction" gdk-setting-action
  ()
  (:new 0)
  (:changed 1)
  (:deleted 2))

;;; ----------------------------------------------------------------------------
;;; enum GdkOwnerChange
;;;
;;; typedef enum
;;; {
;;;   GDK_OWNER_CHANGE_NEW_OWNER,
;;;   GDK_OWNER_CHANGE_DESTROY,
;;;   GDK_OWNER_CHANGE_CLOSE
;;; } GdkOwnerChange;
;;;
;;; Specifies why a selection ownership was changed.
;;;
;;; GDK_OWNER_CHANGE_NEW_OWNER
;;;     some other app claimed the ownership
;;;
;;; GDK_OWNER_CHANGE_DESTROY
;;;     the window was destroyed
;;;
;;; GDK_OWNER_CHANGE_CLOSE
;;;     the client was closed
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkOwnerChange" gdk-owner-change
  ()
  (:new-owner 0)
  (:destroy 1)
  (:close 2))

;;; ----------------------------------------------------------------------------
;;; union GdkEvent
;;;
;;; union _GdkEvent
;;; {
;;;   GdkEventType          type;
;;;   GdkEventAny           any;
;;;   GdkEventExpose        expose;
;;;   GdkEventNoExpose      no_expose;
;;;   GdkEventVisibility    visibility;
;;;   GdkEventMotion        motion;
;;;   GdkEventButton        button;
;;;   GdkEventScroll        scroll;
;;;   GdkEventKey           key;
;;;   GdkEventCrossing      crossing;
;;;   GdkEventFocus         focus_change;
;;;   GdkEventConfigure     configure;
;;;   GdkEventProperty      property;
;;;   GdkEventSelection     selection;
;;;   GdkEventOwnerChange   owner_change;
;;;   GdkEventProximity     proximity;
;;;   GdkEventClient        client;
;;;   GdkEventDND           dnd;
;;;   GdkEventWindowState   window_state;
;;;   GdkEventSetting       setting;
;;;   GdkEventGrabBroken    grab_broken;
;;; };
;;;
;;; The GdkEvent struct contains a union of all of the event structs, and
;;; allows access to the data fields in a number of ways.
;;;
;;; The event type is always the first field in all of the event structs, and
;;; can always be accessed with the following code, no matter what type of
;;; event it is:
;;;
;;;  GdkEvent *event;
;;;  GdkEventType type;
;;;  type = event->type;
;;;
;;; To access other fields of the event structs, the pointer to the event can
;;; be cast to the appropriate event struct pointer, or the union member name
;;; can be used. For example if the event type is GDK_BUTTON_PRESS then the x
;;; coordinate of the button press can be accessed with:
;;;
;;;  GdkEvent *event;
;;;  gdouble x;
;;;  x = ((GdkEventButton*)event)->x;
;;;
;;; or:
;;;
;;;  GdkEvent *event;
;;;  gdouble x;
;;;  x = event->button.x;
;;; ----------------------------------------------------------------------------

(define-g-boxed-variant-cstruct gdk-event "GdkEvent"
  (type gdk-event-type)
  (window (g-object gdk-window))
  (send-event (:boolean :int8))
  (:variant type
            ((:key-press :key-release) event-key
             (time :uint32)
             (state gdk-modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))
            ((:button-press
              :2button-press
              :3button-press
              :button-release) event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (button :uint)
             (device (g-object device))
             (x-root :double)
             (y-root :double))
            ((:scroll) event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state gdk-modifier-type)
             (direction gdk-scroll-direction)
             (device (g-object device))
             (x-root :double)
             (y-root :double))
            ((:motion-notify) event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state gdk-modifier-type)
             (is-hint :int16)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
            ((:expose) event-expose
             (area gdk-rectangle :inline t)
             (region :pointer)
             (count :int))
            ((:visibility-notify) event-visibility
             (state gdk-visibility-state))
            ((:enter-notify :leave-notify) event-crossing
             (sub-window (g-object gdk-window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode gdk-crossing-mode)
             (detail gdk-notify-type)
             (focus :boolean)
             (state :uint))
            ((:focus-change) event-focus
             (in :int16))
            ((:configure) event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))
            ((:property-notify) event-property
             (atom gdk-atom)
             (time :uint32)
             (state gdk-property-state))
            ((:selection-clear
              :selection-notify
              :selection-request) event-selection
             (selection gdk-atom)
             (target gdk-atom)
             (property gdk-atom)
             (time :uint32)
             (requestor gdk-native-window))
            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) event-dnd
             (drag-context (g-object gdk-drag-context))
             (time :uint32)
             (x-root :short)
             (y-root :short))
            ((:proximity-in
              :proximity-out) event-proximity
             (time :uint32)
             (device (g-object gdk-device)))
            ((:client-event) event-client
             (message-time gdk-atom)
             (data-format :ushort)
             (:variant data-format
                       (8 event-client-8
                          (data :uchar :count 20))
                       (16 event-client-16
                           (data :ushort :count 10))
                       (32 event-client-32
                           (data :ulong :count 5))))
            ((:no-expose) event-no-expose)
            ((:window-state) event-window-state
             (changed-mask gdk-window-state)
             (new-window-state gdk-window-state))
            ((:setting) event-setting
             (action gdk-setting-action)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))
            ((:owner-change) event-owner-change
             (owner gdk-native-window)
             (reason gdk-owner-change)
             (selection gdk-atom)
             (time :uint32)
             (selection-time :uint32))
            ((:grab-broken) event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g-object gdk-window)))))

(export (boxed-related-symbols 'gdk-event))

;;; ----------------------------------------------------------------------------
;;; struct GdkEventAny
;;;
;;; struct GdkEventAny {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;; };
;;;
;;; Contains the fields which are common to all event structs. Any event pointer
;;; can safely be cast to a pointer to a GdkEventAny to access these fields.
;;;
;;; GdkEventType type;
;;;     the type of the event.
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventKey
;;;
;;; struct GdkEventKey {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   guint32 time;
;;;   guint state;
;;;   guint keyval;
;;;   gint length;
;;;   gchar *string;
;;;   guint16 hardware_keycode;
;;;   guint8 group;
;;;   guint is_modifier : 1;
;;; };
;;;
;;; Describes a key press or key release event.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_KEY_PRESS or GDK_KEY_RELEASE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; guint state;
;;;     a bit-mask representing the state of the modifier keys (e.g. Control,
;;;     Shift and Alt) and the pointer buttons. See GdkModifierType.
;;;
;;; guint keyval;
;;;     the key that was pressed or released. See the <gdk/gdkkeysyms.h> header
;;;     file for a complete list of GDK key codes.
;;;
;;; gint length;
;;;     the length of string.
;;;
;;; gchar *string;
;;;     a string containing the an approximation of the text that would result
;;;     from this keypress. The only correct way to handle text input of text
;;;     is using input methods (see GtkIMContext), so this field is deprecated
;;;     and should never be used. (gdk_unicode_to_keyval() provides a
;;;     non-deprecated way of getting an approximate translation for a key.)
;;;     The string is encoded in the encoding of the current locale (Note: this
;;;     for backwards compatibility: strings in GTK+ and GDK are typically in
;;;     UTF-8.) and NUL-terminated. In some cases, the translation of the key
;;;     code will be a single NUL byte, in which case looking at length is
;;;     necessary to distinguish it from the an empty translation.
;;;
;;; guint16 hardware_keycode;
;;;     the raw code of the key that was pressed or released.
;;;
;;; guint8 group;
;;;     the keyboard group.
;;;
;;; guint is_modifier : 1;
;;;     a flag that indicates if hardware_keycode is mapped to a modifier.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventButton
;;;
;;; struct GdkEventButton {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   guint32 time;
;;;   gdouble x;
;;;   gdouble y;
;;;   gdouble *axes;
;;;   guint state;
;;;   guint button;
;;;   GdkDevice *device;
;;;   gdouble x_root, y_root;
;;; };
;;;
;;; Used for button press and button release events. The type field will be one
;;; of GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS, and
;;; GDK_BUTTON_RELEASE.
;;;
;;; Double and triple-clicks result in a sequence of events being received.
;;; For double-clicks the order of events will be:
;;;
;;;    1. GDK_BUTTON_PRESS
;;;    2. GDK_BUTTON_RELEASE
;;;    3. GDK_BUTTON_PRESS
;;;    4. GDK_2BUTTON_PRESS
;;;    5. GDK_BUTTON_RELEASE
;;;
;;; Note that the first click is received just like a normal button press,
;;; while the second click results in a GDK_2BUTTON_PRESS being received just
;;; after the GDK_BUTTON_PRESS.
;;;
;;; Triple-clicks are very similar to double-clicks, except that
;;; GDK_3BUTTON_PRESS is inserted after the third click. The order of the
;;; events is:
;;;
;;;    1. GDK_BUTTON_PRESS
;;;    2. GDK_BUTTON_RELEASE
;;;    3. GDK_BUTTON_PRESS
;;;    4. GDK_2BUTTON_PRESS
;;;    5. GDK_BUTTON_RELEASE
;;;    6. GDK_BUTTON_PRESS
;;;    7. GDK_3BUTTON_PRESS
;;;    8. GDK_BUTTON_RELEASE
;;;
;;; For a double click to occur, the second button press must occur within 1/4
;;; of a second of the first. For a triple click to occur, the third button
;;; press must also occur within 1/2 second of the first button press.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS,
;;;     GDK_3BUTTON_PRESS or GDK_BUTTON_RELEASE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; gdouble x;
;;;     the x coordinate of the pointer relative to the window.
;;;
;;; gdouble y;
;;;     the y coordinate of the pointer relative to the window.
;;;
;;; gdouble *axes;
;;;     x, y translated to the axes of device, or NULL if device is the mouse.
;;;
;;; guint state;
;;;     a bit-mask representing the state of the modifier keys (e.g. Control,
;;;     Shift and Alt) and the pointer buttons. See GdkModifierType.
;;;
;;; guint button;
;;;     the button which was pressed or released, numbered from 1 to 5.
;;;     Normally button 1 is the left mouse button, 2 is the middle button, and
;;;     3 is the right button. On 2-button mice, the middle button can often be
;;;     simulated by pressing both mouse buttons together.
;;;
;;; GdkDevice *device;
;;;     the device where the event originated.
;;;
;;; gdouble x_root;
;;;     the x coordinate of the pointer relative to the root of the screen.
;;;
;;; gdouble y_root;
;;;     the y coordinate of the pointer relative to the root of the screen.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventScroll
;;;
;;; struct GdkEventScroll {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   guint32 time;
;;;   gdouble x;
;;;   gdouble y;
;;;   guint state;
;;;   GdkScrollDirection direction;
;;;   GdkDevice *device;
;;;   gdouble x_root, y_root;
;;; };
;;;
;;; Generated from button presses for the buttons 4 to 7. Wheel mice are
;;; usually configured to generate button press events for buttons 4 and 5 when
;;; the wheel is turned.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_SCROLL).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; gdouble x;
;;;     the x coordinate of the pointer relative to the window.
;;;
;;; gdouble y;
;;;     the y coordinate of the pointer relative to the window.
;;;
;;; guint state;
;;;     a bit-mask representing the state of the modifier keys (e.g. Control,
;;;     Shift and Alt) and the pointer buttons. See GdkModifierType.
;;;
;;; GdkScrollDirection direction;
;;;     the direction to scroll to (one of GDK_SCROLL_UP, GDK_SCROLL_DOWN,
;;;     GDK_SCROLL_LEFT and GDK_SCROLL_RIGHT).
;;;
;;; GdkDevice *device;
;;;     the device where the event originated.
;;;
;;; gdouble x_root;
;;;     the x coordinate of the pointer relative to the root of the screen.
;;;
;;; gdouble y_root;
;;;     the y coordinate of the pointer relative to the root of the screen.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventMotion
;;;
;;; struct GdkEventMotion {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   guint32 time;
;;;   gdouble x;
;;;   gdouble y;
;;;   gdouble *axes;
;;;   guint state;
;;;   gint16 is_hint;
;;;   GdkDevice *device;
;;;   gdouble x_root, y_root;
;;; };
;;;
;;; Generated when the pointer moves.
;;;
;;; GdkEventType type;
;;;     the type of the event.
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; gdouble x;
;;;     the x coordinate of the pointer relative to the window.
;;;
;;; gdouble y;
;;;     the y coordinate of the pointer relative to the window.
;;;
;;; gdouble *axes;
;;;     x, y translated to the axes of device, or NULL if device is the mouse.
;;;
;;; guint state;
;;;     a bit-mask representing the state of the modifier keys (e.g. Control,
;;;     Shift and Alt) and the pointer buttons. See GdkModifierType.
;;;
;;; gint16 is_hint;
;;;     set to 1 if this event is just a hint, see the
;;;     GDK_POINTER_MOTION_HINT_MASK value of GdkEventMask.
;;;
;;; GdkDevice *device;
;;;     the device where the event originated.
;;;
;;; gdouble x_root;
;;;     the x coordinate of the pointer relative to the root of the screen.
;;;
;;; gdouble y_root;
;;;     the y coordinate of the pointer relative to the root of the screen.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventExpose
;;;
;;; struct GdkEventExpose {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkRectangle area;
;;;   GdkRegion *region;
;;;   gint count; /* If non-zero, how many more events follow. */
;;; };
;;;
;;; Generated when all or part of a window becomes visible and needs to be
;;; redrawn.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_EXPOSE or GDK_DAMAGE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkRectangle area;
;;;     bounding box of region.
;;;
;;; GdkRegion *region;
;;;     the region that needs to be redrawn.
;;;
;;; gint count;
;;;     the number of contiguous GDK_EXPOSE events following this one. The only
;;;     use for this is "exposure compression", i.e. handling all contiguous
;;;     GDK_EXPOSE events in one go, though GDK performs some exposure
;;;     compression so this is not normally needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventVisibility
;;;
;;; struct GdkEventVisibility {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkVisibilityState state;
;;; };
;;;
;;; Generated when the window visibility status has changed.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_VISIBILITY_NOTIFY).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkVisibilityState state;
;;;     the new visibility state (GDK_VISIBILITY_FULLY_OBSCURED,
;;;     GDK_VISIBILITY_PARTIAL or GDK_VISIBILITY_UNOBSCURED).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventCrossing
;;;
;;; struct GdkEventCrossing {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkWindow *subwindow;
;;;   guint32 time;
;;;   gdouble x;
;;;   gdouble y;
;;;   gdouble x_root;
;;;   gdouble y_root;
;;;   GdkCrossingMode mode;
;;;   GdkNotifyType detail;
;;;   gboolean focus;
;;;   guint state;
;;; };
;;;
;;; Generated when the pointer enters or leaves a window.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_ENTER_NOTIFY or GDK_LEAVE_NOTIFY).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkWindow *subwindow;
;;;     the window that was entered or left.
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; gdouble x;
;;;     the x coordinate of the pointer relative to the window.
;;;
;;; gdouble y;
;;;     the y coordinate of the pointer relative to the window.
;;;
;;; gdouble x_root;
;;;     the x coordinate of the pointer relative to the root of the screen.
;;;
;;; gdouble y_root;
;;;     the y coordinate of the pointer relative to the root of the screen.
;;;
;;; GdkCrossingMode mode;
;;;     the crossing mode (GDK_CROSSING_NORMAL, GDK_CROSSING_GRAB,
;;;     GDK_CROSSING_UNGRAB, GDK_CROSSING_GTK_GRAB, GDK_CROSSING_GTK_UNGRAB or
;;;     GDK_CROSSING_STATE_CHANGED). GDK_CROSSING_GTK_GRAB,
;;;     GDK_CROSSING_GTK_UNGRAB, and GDK_CROSSING_STATE_CHANGED were added in
;;;     2.14 and are always synthesized, never native.
;;;
;;; GdkNotifyType detail;
;;;     the kind of crossing that happened (GDK_NOTIFY_INFERIOR,
;;;     GDK_NOTIFY_ANCESTOR, GDK_NOTIFY_VIRTUAL, GDK_NOTIFY_NONLINEAR or
;;;     GDK_NOTIFY_NONLINEAR_VIRTUAL).
;;;
;;; gboolean focus;
;;;     TRUE if window is the focus window or an inferior.
;;;
;;; guint state;
;;;     a bit-mask representing the state of the modifier keys (e.g. Control,
;;;     Shift and Alt) and the pointer buttons. See GdkModifierType.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventFocus
;;;
;;; struct GdkEventFocus {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   gint16 in;
;;; };
;;;
;;; Describes a change of keyboard focus.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_FOCUS_CHANGE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; gint16 in;
;;;     TRUE if the window has gained the keyboard focus, FALSE if it has lost
;;;     the focus.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventConfigure
;;;
;;; struct GdkEventConfigure {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   gint x, y;
;;;   gint width;
;;;   gint height;
;;; };
;;;
;;; Generated when a window size or position has changed.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_CONFIGURE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; gint x;
;;;     the new x coordinate of the window, relative to its parent.
;;;
;;; gint y;
;;;     the new y coordinate of the window, relative to its parent.
;;;
;;; gint width;
;;;     the new width of the window.
;;;
;;; gint height;
;;;     the new height of the window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventProperty
;;;
;;; struct GdkEventProperty {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkAtom atom;
;;;   guint32 time;
;;;   guint state;
;;; };
;;;
;;; Describes a property change on a window.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_PROPERTY_NOTIFY).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkAtom atom;
;;;     the property that was changed.
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; guint state;
;;;     whether the property was changed (GDK_PROPERTY_NEW_VALUE) or deleted
;;;    (GDK_PROPERTY_DELETE).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventSelection
;;;
;;; struct GdkEventSelection {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkAtom selection;
;;;   GdkAtom target;
;;;   GdkAtom property;
;;;   guint32 time;
;;;   GdkNativeWindow requestor;
;;; };
;;;
;;; Generated when a selection is requested or ownership of a selection is
;;; taken over by another client application.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_SELECTION_CLEAR, GDK_SELECTION_NOTIFY or
;;;     GDK_SELECTION_REQUEST).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkAtom selection;
;;;     the selection.
;;;
;;; GdkAtom target;
;;;     the target to which the selection should be converted.
;;;
;;; GdkAtom property;
;;;     the property in which to place the result of the conversion.
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; GdkNativeWindow requestor;
;;;     the native window on which to place property.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventDND
;;;
;;; struct GdkEventDND {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkDragContext *context;
;;;
;;;   guint32 time;
;;;   gshort x_root, y_root;
;;; };
;;;
;;; Generated during DND operations.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_DRAG_ENTER, GDK_DRAG_LEAVE, GDK_DRAG_MOTION,
;;;     GDK_DRAG_STATUS, GDK_DROP_START or GDK_DROP_FINISHED).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkDragContext *context;
;;;     the GdkDragContext for the current DND operation.
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; gshort x_root;
;;;     the x coordinate of the pointer relative to the root of the screen,
;;;     only set for GDK_DRAG_MOTION and GDK_DROP_START.
;;;
;;; gshort y_root;
;;;     the y coordinate of the pointer relative to the root of the screen,
;;;     only set for GDK_DRAG_MOTION and GDK_DROP_START.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventProximity
;;;
;;; struct GdkEventProximity {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   guint32 time;
;;;   GdkDevice *device;
;;; };
;;;
;;; Proximity events are generated when using GDK's wrapper for the XInput
;;; extension. The XInput extension is an add-on for standard X that allows you
;;; to use nonstandard devices such as graphics tablets. A proximity event
;;; indicates that the stylus has moved in or out of contact with the tablet,
;;; or perhaps that the user's finger has moved in or out of contact with a
;;; touch screen.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_PROXIMITY_IN or GDK_PROXIMITY_OUT).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; GdkDevice *device;
;;;     the device where the event originated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventClient
;;;
;;; struct GdkEventClient {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkAtom message_type;
;;;   gushort data_format;
;;;   union {
;;;     char b[20];
;;;     short s[10];
;;;     long l[5];
;;;   } data;
;;; };
;;;
;;; An event sent by another client application.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_CLIENT_EVENT).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkAtom message_type;
;;;     the type of the message, which can be defined by the application.
;;;
;;; gushort data_format;
;;;     the format of the data, given as the number of bits in each data
;;;     element, i.e. 8, 16, or 32. 8-bit data uses the b array of the data
;;;     union, 16-bit data uses the s array, and 32-bit data uses the l array.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventNoExpose
;;;
;;; struct GdkEventNoExpose {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;; };
;;;
;;; Generated when the area of a GdkDrawable being copied, with
;;; gdk_draw_drawable() or gdk_window_copy_area(), was completely available.
;;;
;;; FIXME: add more here.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_NO_EXPOSE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventWindowState
;;;
;;; struct GdkEventWindowState {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkWindowState changed_mask;
;;;   GdkWindowState new_window_state;
;;; };
;;;
;;; Generated when the state of a toplevel window changes.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_WINDOW_STATE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkWindowState changed_mask;
;;;     mask specifying what flags have changed.
;;;
;;; GdkWindowState new_window_state;
;;;     the new window state, a combination of GdkWindowState bits.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventSetting
;;;
;;; struct GdkEventSetting {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkSettingAction action;
;;;   char *name;
;;; };
;;;
;;; Generated when a setting is modified.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_SETTING).
;;;
;;; GdkWindow *window;
;;;     the window which received the event.
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkSettingAction action;
;;;     what happened to the setting (GDK_SETTING_ACTION_NEW,
;;;     GDK_SETTING_ACTION_CHANGED or GDK_SETTING_ACTION_DELETED).
;;;
;;; char *name;
;;;     the name of the setting.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventOwnerChange
;;;
;;; struct GdkEventOwnerChange {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   GdkNativeWindow owner;
;;;   GdkOwnerChange reason;
;;;   GdkAtom selection;
;;;   guint32 time;
;;;   guint32 selection_time;
;;; };
;;;
;;; Generated when the owner of a selection changes. On X11, this information
;;; is only available if the X server supports the XFIXES extension.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_OWNER_CHANGE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; GdkNativeWindow owner;
;;;     the new owner of the selection
;;;
;;; GdkOwnerChange reason;
;;;     the reason for the ownership change as a GdkOwnerChange value
;;;
;;; GdkAtom selection;
;;;     the atom identifying the selection
;;;
;;; guint32 time;
;;;     the timestamp of the event
;;;
;;; guint32 selection_time;
;;;     the time at which the selection ownership was taken over
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventGrabBroken
;;;
;;; struct GdkEventGrabBroken {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   gboolean keyboard;
;;;   gboolean implicit;
;;;   GdkWindow *grab_window;
;;; };
;;;
;;; Generated when a pointer or keyboard grab is broken. On X11, this happens
;;; when the grab window becomes unviewable (i.e. it or one of its ancestors
;;; is unmapped), or if the same application grabs the pointer or keyboard
;;; again. Note that implicit grabs (which are initiated by button presses) can
;;; also cause GdkEventGrabBroken events.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_GRAB_BROKEN)
;;;
;;; GdkWindow *window;
;;;     the window which received the event, i.e. the window that previously
;;;     owned the grab
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent).
;;;
;;; gboolean keyboard;
;;;     TRUE if a keyboard grab was broken, FALSE if a pointer grab was broken
;;;
;;; gboolean implicit;
;;;     TRUE if the broken grab was implicit
;;;
;;; GdkWindow *grab_window;
;;;     If this event is caused by another grab in the same application,
;;;     grab_window contains the new grab window. Otherwise grab_window is NULL.
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------


;;; --- End of file gdk.event-structures.lisp ----------------------------------
