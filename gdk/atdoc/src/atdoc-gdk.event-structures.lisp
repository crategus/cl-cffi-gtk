;;; ----------------------------------------------------------------------------
;;; atdoc-gdk.event-structures.lisp
;;;
;;; Documentation strings for the library GDK.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gdk)

;;; --- gdk-scroll-direction ---------------------------------------------------

(setf (gethash 'gdk-scroll-direction atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-scroll-direction atdoc:*external-symbols*)
 "@version{2013-1-12}
  @short{Specifies the direction for GdkEventScroll.}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GdkScrollDirection\" gdk-scroll-direction
  ()
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))
    @end{pre}
    @begin[code]{table}
      @entry[:up]{the window is scrolled up.}
      @entry[:down]{the window is scrolled down.}
      @entry[:left]{the window is scrolled to the left.}
      @entry[:right]{the window is scrolled to the right.}
      @entry[:smooth]{the scrolling is determined by the delta values in
        GdkEventScroll. See gdk_event_get_scroll_deltas().}
    @end{table}
  @end{dictionary}")

;;; --- gdk-visibility-state ---------------------------------------------------

(setf (gethash 'gdk-visibility-state atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-visibility-state atdoc:*external-symbols*)
 "@version{2013-1-12}
  @short{Specifies the visiblity status of a window for a GdkEventVisibility.}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GdkVisibilityState\" gdk-visibility-state
  ()
  (:unobscured 0)
  (:partial 1)
  (:fully-obscured 2))
    @end{pre}
    @begin[code]{table}
      @entry[:unobscured]{the window is completely visible.}
      @entry[:partial]{the window is partially visible.}
      @entry[:obscured]{the window is not visible at all.}
    @end{table}
  @end{dictionary}")

;;; --- gdk-crossing-mode ------------------------------------------------------

(setf (gethash 'gdk-crossing-mode atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-crossing-mode atdoc:*external-symbols*)
 "@version{2013-1-2}
  @short{Specifies the crossing mode for GdkEventCrossing.}
  @begin[Lisp Implementaton]{dictionary}
    @begin{pre}
(defcenum gdk-crossing-mode
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed
  :touch-begin
  :touch-end
  :device-switch)
    @end{pre}
    @begin[code]{table}
      @entry[:normal]{crossing because of pointer motion.}
      @entry[:grab]{crossing because a grab is activated.}
      @entry[:ungrab]{crossing because a grab is deactivated.}
      @entry[:gtk-grab]{crossing because a GTK+ grab is activated.}
      @entry[:gtk-ungrab]{crossing because a GTK+ grab is deactivated.}
      @entry[:state-changed]{crossing because a GTK+ widget changed state (e.g.
        sensitivity).}
      @entry[:touch-begin]{crossing because a touch sequence has begun, this
        event is synthetic as the pointer might have not left the window.}
      @entry[:touch-end]{crossing because a touch sequence has ended, this event
        is synthetic as the pointer might have not left the window.}
      @entry[:device-switch]{crossing because of a device switch (i.e. a mouse
        taking control of the pointer after a touch device), this event is
        synthetic as the pointer didn't leave the window.}
    @end{table}
  @end{dictionary}")

;;; --- gdk-notify-type --------------------------------------------------------

(setf (gethash 'gdk-notify-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-notify-type atdoc:*external-symbols*)
 "@version{2013-1-12}
  @short{Specifies the kind of crossing for GdkEventCrossing.}

  See the X11 protocol specification of LeaveNotify for full details of
  crossing event generation.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defcenum gdk-notify-type
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)
    @end{pre}
    @begin[code]{table}
      @entry[:ancestor]{the window is entered from an ancestor or left towards
        an ancestor.}
      @entry[:virtual]{the pointer moves between an ancestor and an inferior of
        the window.}
      @entry[:inferior]{the window is entered from an inferior or left towards
        an inferior.}
      @entry[:nonlinear]{the window is entered from or left towards a window
        which is neither an ancestor nor an inferior.}
      @entry[:nonlinear-virtual]{the pointer moves between two windows which are
        not ancestors of each other and the window is part of the ancestor chain
        between one of these windows and their least common ancestor.}
      @entry[:unknown]{an unknown type of enter/leave event occurred.}
    @end{table}
  @end{dictionary}")

;;; --- gdk-property-state -----------------------------------------------------

(setf (gethash 'gdk-property-state atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-property-state atdoc:*external-symbols*)
 "@version{2013-1-12}
  @short{Specifies the type of a property change for a GdkEventProperty.}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GdkPropertyState\" gdk-property-state
  ()
  :new-value
  :delete)
    @end{pre}
    @begin[code]{table}
      @entry[:new-value]{the property value was changed.}
      @entry[:delete]{the property was deleted.}
    @end{table}
  @end{dictionary}")

;;; --- gdk-window-state -------------------------------------------------------

(setf (gethash 'gdk-window-state atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-window-state atdoc:*external-symbols*)
 "@version{2013-1-12}
  @short{Specifies the state of a toplevel window.}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-flags \"GdkWindowState\" gdk-window-state
  (:export t
   :type-initializer \"gdk_window_state_get_type\")
  (:withdrawn 1)
  (:iconified 2)
  (:maximized 4)
  (:sticky 8)
  (:fullscreen 16)
  (:above 32)
  (:below 64)
  (:focused 128))
    @end{pre}
    @begin[code]{table}
      @entry[:withdrawn]{the window is not shown.}
      @entry[:iconified]{the window is minimized.}
      @entry[:maximized]{the window is maximized.}
      @entry[:sticky]{the window is sticky.}
      @entry[:fullscreen]{the window is maximized without decorations.}
      @entry[:above]{the window is kept above other windows.}
      @entry[:below]{the window is kept below other windows.}
      @entry[:focused]{the window is presented as focused (with active
        decorations).}
    @end{table}
  @end{dictionary}")

;;; --- gdk-setting-action -----------------------------------------------------

(setf (gethash 'gdk-setting-action atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-setting-action atdoc:*external-symbols*)
 "@version{2013-1-12}
  @begin{short}
    Specifies the kind of modification applied to a setting in a
    GdkEventSetting.
  @end{short}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GdkSettingAction\" gdk-setting-action
  ()
  (:new 0)
  (:changed 1)
  (:deleted 2))
    @end{pre}
    @begin[code]{table}
      @entry[:new]{a setting was added.}
      @entry[:changes]{a setting was changed.}
      @entry[:deleted]{a setting was deleted.}
    @end{table}
  @end{dictionary}")

;;; --- gdk-owner-change -------------------------------------------------------

(setf (gethash 'gdk-owner-change atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-owner-change atdoc:*external-symbols*)
 "@version{2013-1-12}
  @short{Specifies why a selection ownership was changed.}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GdkOwnerChange\" gdk-owner-change
  ()
  (:new-owner 0)
  (:destroy 1)
  (:close 2))
    @end{pre}
    @begin[code]{table}
      @entry[:owner]{some other app claimed the ownership}
      @entry[:destroy]{the window was destroyed}
      @entry[:close]{the client was closed}
    @end{table}
  @end{dictionary}")

;;; --- gdk-event-type ---------------------------------------------------------

(setf (gethash 'gdk-event-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gdk-event-type atdoc:*external-symbols*)
 "@version{2013-1-11}
  @short{Specifies the type of the event.}

  Do not confuse these events with the signals that GTK+ widgets emit.
  Although many of these events result in corresponding signals being emitted,
  the events are often transformed or filtered along the way.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GdkEventType\" gdk-event-type
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
    @end{pre}
    @begin[code]{table}
      @entry[:nothing]{a special code to indicate a null event.}
      @entry[:delete]{the window manager has requested that the toplevel window
        be hidden or destroyed, usually when the user clicks on a special icon
        in the title bar.}
      @entry[:destroy]{the window has been destroyed.}
      @entry[:expose]{all or part of the window has become visible and needs to
        be redrawn.}
      @entry[:motion-notify]{the pointer (usually a mouse) has moved.}
      @entry[:button-press]{a mouse button has been pressed.}
      @entry[:2button-press]{a mouse button has been double-clicked (clicked
        twice within a short period of time). Note that each click also
        generates a GDK_BUTTON_PRESS event.}
      @entry[3button-press]{a mouse button has been clicked 3 times in a short
        period of time. Note that each click also generates a GDK_BUTTON_PRESS
        event.}
      @entry[:button-release]{a mouse button has been released.}
      @entry[:key-press]{a key has been pressed.}
      @entry[:key-release]{a key has been released.}
      @entry[:enter-notifiy]{the pointer has entered the window.}
      @entry[:leave-notify]{the pointer has left the window.}
      @entry[:focus-change]{the keyboard focus has entered or left the window.}
      @entry[:configure]{the size, position or stacking order of the window has
        changed. Note that GTK+ discards these events for GDK_WINDOW_CHILD
        windows.}
      @entry[:map]{the window has been mapped.}
      @entry[:unmap]{the window has been unmapped.}
      @entry[:property-notify]{a property on the window has been changed or
        deleted.}
      @entry[:selection-clear]{the application has lost ownership of a
        selection.}
      @entry[:selection-request]{another application has requested a selection.}
      @entry[:selection-notify]{a selection has been received.}
      @entry[:proximity-in]{an input device has moved into contact with a
        sensing surface (e.g. a touchscreen or graphics tablet).}
      @entry[:proximity-out]{an input device has moved out of contact with a
        sensing surface.}
      @entry[:drag-enter]{the mouse has entered the window while a drag is in
        progress.}
      @entry[:drag-leave]{the mouse has left the window while a drag is in
        progress.}
      @entry[:drag-motion]{the mouse has moved in the window while a drag is in
        progress.}
      @entry[:drag-status]{the status of the drag operation initiated by the
        window has changed.}
      @entry[:drop-start]{a drop operation onto the window has started.}
      @entry[:drop-finished]{the drop operation initiated by the window has
        completed.}
      @entry[:client-event]{a message has been received from another
        application.}
      @entry[:visibility-notify]{the window visibility status has changed.}
      @entry[:scroll]{the scroll wheel was turned}
      @entry[:window-state]{the state of a window has changed. See
        GdkWindowState for the possible window states}
      @entry[:setting]{a setting has been modified.}
      @entry[:owner-change]{the owner of a selection has changed. This event
        type was added in 2.6}
      @entry[:grab-broken]{a pointer or keyboard grab was broken. This event
        type was added in 2.8.}
      @entry[:damage]{the content of the window has been changed. This event
        type was added in 2.14.}
      @entry[:touch-begin]{A new touch event sequence has just started. This
        event type was addedin 3.4.}
      @entry[:touch-update]{A touch event sequence has been updated. This event
        type was added in 3.4.}
      @entry[:toch-end]{A touch event sequence has finished. This event type was
        added in 3.4.}
      @entry[:touch-cancel]{A touch event sequence has been canceled. This event
        type was added in 3.4.}
      @entry[:event-last]{marks the end of the GdkEventType enumeration. Added
        in 2.18}
    @end{table}
  @end{dictionary}")

#|
;;; ----------------------------------------------------------------------------
;;; enum GdkModifierType
;;;
;;; typedef enum {
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
;;;   GDK_MODIFIER_RESERVED_13_MASK  = 1 << 13,
;;;   GDK_MODIFIER_RESERVED_14_MASK  = 1 << 14,
;;;   GDK_MODIFIER_RESERVED_15_MASK  = 1 << 15,
;;;   GDK_MODIFIER_RESERVED_16_MASK  = 1 << 16,
;;;   GDK_MODIFIER_RESERVED_17_MASK  = 1 << 17,
;;;   GDK_MODIFIER_RESERVED_18_MASK  = 1 << 18,
;;;   GDK_MODIFIER_RESERVED_19_MASK  = 1 << 19,
;;;   GDK_MODIFIER_RESERVED_20_MASK  = 1 << 20,
;;;   GDK_MODIFIER_RESERVED_21_MASK  = 1 << 21,
;;;   GDK_MODIFIER_RESERVED_22_MASK  = 1 << 22,
;;;   GDK_MODIFIER_RESERVED_23_MASK  = 1 << 23,
;;;   GDK_MODIFIER_RESERVED_24_MASK  = 1 << 24,
;;;   GDK_MODIFIER_RESERVED_25_MASK  = 1 << 25,
;;;
;;;   /* The next few modifiers are used by XKB, so we skip to the end.
;;;    * Bits 15 - 25 are currently unused. Bit 29 is used internally.
;;;    */
;;;
;;;   GDK_SUPER_MASK    = 1 << 26,
;;;   GDK_HYPER_MASK    = 1 << 27,
;;;   GDK_META_MASK     = 1 << 28,
;;;
;;;   GDK_MODIFIER_RESERVED_29_MASK  = 1 << 29,
;;;
;;;   GDK_RELEASE_MASK  = 1 << 30,
;;;
;;;   /* Combination of GDK_SHIFT_MASK..GDK_BUTTON5_MASK + GDK_SUPER_MASK
;;;      + GDK_HYPER_MASK + GDK_META_MASK + GDK_RELEASE_MASK */
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
;;; to Mod2 - Mod5, and indicates this by setting GDK_SUPER_MASK, GDK_HYPER_MASK
;;; or GDK_META_MASK in the state field of key events.
;;;
;;; Note that GDK may add internal values to events which include reserved
;;; values such as GDK_MODIFIER_RESERVED_13_MASK. Your code should preserve and
;;; ignore them. You can use GDK_MODIFIER_MASK to remove all reserved values.
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
;;;     the fourth modifier key (it depends on the modifier mapping of the X
;;;     server which key is interpreted as this modifier, but normally it is the
;;;     Alt key).
;;;
;;; GDK_MOD2_MASK
;;;     the fifth modifier key (it depends on the modifier mapping of the X
;;;     server which key is interpreted as this modifier).
;;;
;;; GDK_MOD3_MASK
;;;     the sixth modifier key (it depends on the modifier mapping of the X
;;;     server which key is interpreted as this modifier).
;;;
;;; GDK_MOD4_MASK
;;;     the seventh modifier key (it depends on the modifier mapping of the X
;;;     server which key is interpreted as this modifier).
;;;
;;; GDK_MOD5_MASK
;;;     the eighth modifier key (it depends on the modifier mapping of the X
;;;     server which key is interpreted as this modifier).
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
;;; GDK_MODIFIER_RESERVED_13_MASK
;;;     A reserved bit flag; do not use in your own code
;;;
;;; GDK_MODIFIER_RESERVED_14_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_15_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_16_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_17_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_18_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_19_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_20_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_21_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_22_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_23_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_24_MASK
;;;
;;;
;;; GDK_MODIFIER_RESERVED_25_MASK
;;;
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
;;; GDK_MODIFIER_RESERVED_29_MASK
;;;
;;;
;;; GDK_RELEASE_MASK
;;;     not used in GDK itself. GTK+ uses it to differentiate between (keyval,
;;;     modifiers) pairs from key press and release events.
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
|#

;;; --- gdk-event-mask ---------------------------------------------------------

(setf (gethash 'gdk-event-mask atdoc:*symbol-name-alias*) "Flags")
(setf (gethash 'gdk-event-mask atdoc:*external-symbols*)
 "@version{2013-1-6}
  @begin{short}
    A set of bit-flags to indicate which events a window is to receive. Most of
    these masks map onto one or more of the @symbol{gdk-event-type} event
    types.
  @end{short}

  @code{:pointer-motion-hint-mask} is a special mask which is used to reduce the
  number of @code{GDK_MOTION_NOTIFY} events received. Normally a
  @code{GDK_MOTION_NOTIFY} event is received each time the mouse moves. However,
  if the application spends a lot of time processing the event (updating the
  display, for example), it can lag behind the position of the mouse. When using
  @code{:pointer-motion-hint-mask}, fewer @code{GDK_MOTION_NOTIFY} events will
  be sent, some of which are marked as a hint (the @code{is_hint member} is
  @arg{true}). To receive more motion events after a motion hint event, the
  application needs to asks for more, by calling
  @fun{gdk-event-request-motions}.

  If @code{:touch-mask} is enabled, the window will receive touch events from
  touch-enabled devices. Those will come as sequences of @em{GdkEventTouch}
  with type @code{GDK_TOUCH_UPDATE}, enclosed by two events with type
  @code{GDK_TOUCH_BEGIN} and @code{GDK_TOUCH_END} (or @code{GDK_TOUCH_CANCEL}).
  @code{gdk_event_get_event_sequence()} returns the event sequence for these
  events, so different sequences may be distinguished.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-flags \"GdkEventMask\" gdk-event-mask
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
  (:touch-mask #.(ash 1 22))
  (:smooth-scroll-mask #.(ash 1 23))
  (:all-events-mask 4194302))
    @end{pre}
    @begin{table}
      @entry[:exposure-mask]{receive expose events}
      @entry[:pointer-motion-mask]{receive all pointer motion events}
      @entry[:pointer-motion-hint-mask]{see the explanation above}
      @entry[:button-motion-mask]{receive pointer motion events while any
        button is pressed}
      @entry[:button1-motion-mask]{receive pointer motion events while 1
        button is pressed}
      @entry[:button2-motion-mask]{receive pointer motion events while 2
        button is pressed}
      @entry[:button3-motion-mask]{receive pointer motion events while 3
        button is pressed}
      @entry[:button-press-mask]{receive button press events}
      @entry[:button-release-mask]{receive button release events}
      @entry[:key-press-mask]{receive key press events}
      @entry[:key-release-mask]{receive key release events}
      @entry[:enter-notify-mask]{receive window enter events}
      @entry[:leave-notify-mask]{receive window leave events}
      @entry[:focus-change-mask]{receive focus change events}
      @entry[:structure-mask]{receive events about window configuration
        change}
      @entry[:property-change-mask]{receive property change events}
      @entry[:visibility-notify-mask]{receive visibility change events}
      @entry[:proximity-in-mask]{receive proximity in events}
      @entry[:proximity-out-mask]{receive proximity out events}
      @entry[:substructure-mask]{receive events about window configuration
        changes of child windows}
      @entry[:scroll-mask]{receive scroll events}
      @entry[:touch-mask]{receive touch events}
      @entry[:smooth-scroll-mask]{receive smooth scrolling events}
      @entry[:all-events-mask]{the combination of all the above event masks.}
    @end{table}
  @end{dictionary}")

;;; --- gdk-event --------------------------------------------------------------

(setf (documentation 'gdk-event 'type)
 "@version{2013-1-11}
  @begin{short}
    The @sym{gdk-event} struct contains a union of all of the event structs,
    and allows access to the data fields in a number of ways.
  @end{short}

  The event type is always the first field in all of the event structs, and
  can always be accessed with the following code, no matter what type of event
  it is:
  @begin{pre}
 GdkEvent *event;
 GdkEventType type;
 
 type = event->type;
  @end{pre}
  To access other fields of the event structs, the pointer to the event can be
  cast to the appropriate event struct pointer, or the union member name can
  be used. For example if the event type is GDK_BUTTON_PRESS then the x
  coordinate of the button press can be accessed with:
  @begin{pre}
 GdkEvent *event;
 gdouble x;

 x = ((GdkEventButton*)event)->x;
  @end{pre}
  or:
  @begin{pre}
 GdkEvent *event;
 gdouble x;

 x = event->button.x;
  @end{pre}
  @begin{pre}
  union GdkEvent
 
  union _GdkEvent
  {
    GdkEventType              type;
    GdkEventAny               any;
    GdkEventExpose            expose;
    GdkEventVisibility        visibility;
    GdkEventMotion            motion;
    GdkEventButton            button;
    GdkEventTouch             touch;
    GdkEventScroll            scroll;
    GdkEventKey               key;
    GdkEventCrossing          crossing;
    GdkEventFocus             focus_change;
    GdkEventConfigure         configure;
    GdkEventProperty          property;
    GdkEventSelection         selection;
    GdkEventOwnerChange       owner_change;
    GdkEventProximity         proximity;
    GdkEventDND               dnd;
    GdkEventWindowState       window_state;
    GdkEventSetting           setting;
    GdkEventGrabBroken        grab_broken;
  @};
  @end{pre}
  @see-slot{gdk-event-type}
  @see-slot{gdk-event-window}
  @see-slot{gdk-event-send-event}
")

;;; ----------------------------------------------------------------------------

#|
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
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
             ;; TODO: Is it correct to replace GdkNativeWindow with g-object
             (requestor g-object))
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
             ;; TODO: Is it correct to replace GdkNativeWindow with g-object
             (owner g-object)
             (reason gdk-owner-change)
             (selection gdk-atom)
             (time :uint32)
             (selection-time :uint32))
            ((:grab-broken) event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g-object gdk-window)))))
|#

;;; --- gdk-event-type ---------------------------------------------------------

(setf (gethash 'gdk-event-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-type 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event} struct.
  @end{short}")

;;; --- gdk-event-window -------------------------------------------------------

(setf (gethash 'gdk-event-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-window 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event} struct.
  @end{short}")

;;; --- gdk-event-send-event ---------------------------------------------------

(setf (gethash 'gdk-event-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-send-event 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event} struct.
  @end{short}")


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


;;; --- gdk-event-key ----------------------------------------------------------

(setf (documentation 'gdk-event-key 'type)
 "@version{2013-1-11}
  @short{Describes a key press or key release event.}
  @begin{pre}
 struct GdkEventKey {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   guint32 time;
   guint state;
   guint keyval;
   gint length;
   gchar *string;
   guint16 hardware_keycode;
   guint8 group;
   guint is_modifier : 1;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_KEY_PRESS or 
      GDK_KEY_RELEASE).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[guint state]{a bit-mask representing the state of the modifier keys
      (e.g. Control, Shift and Alt) and the pointer buttons.
      See GdkModifierType.}
    @entry[guint keyval]{the key that was pressed or released. See the
      <gdk/gdkkeysyms.h> header file for a complete list of GDK key codes.}
    @entry[gint length]{the length of string.}
    @entry[gchar *string]{a string containing the an approximation of the text
      that would result from this keypress. The only correct way to handle text
      input of text is using input methods (see GtkIMContext), so this field is
      deprecated and should never be used. (gdk_unicode_to_keyval() provides a
      non-deprecated way of getting an approximate translation for a key.) The
      string is encoded in the encoding of the current locale (Note: this for
      backwards compatibility: strings in GTK+ and GDK are typically in UTF-8.)
      and NUL-terminated. In some cases, the translation of the key code will be
      a single NUL byte, in which case looking at length is necessary to
      distinguish it from the an empty translation.}
    @entry[guint16 hardware_keycode]{the raw code of the key that was pressed or
      released.}
    @entry[guint8 group]{the keyboard group.}
    @entry[guint is_modifier : 1]{a flag that indicates if hardware_keycode is
      mapped to a modifier. Since 2.10}
  @end{table}
  @see-slot{gdk-event-key-type}
  @see-slot{gdk-event-key-window}
  @see-slot{gdk-event-key-send-event}
  @see-slot{gdk-event-key-time}
  @see-slot{gdk-event-key-state}
  @see-slot{gdk-event-key-keyval}
  @see-slot{gdk-event-key-length}
  @see-slot{gdk-event-key-string}
  @see-slot{gdk-event-key-hardware-keycode}
  @see-slot{gdk-event-key-group}
  @see-slot{gdk-event-key-is-modifier}")

;;; ----------------------------------------------------------------------------

;;; --- gdk-event-key-type -----------------------------------------------------

(setf (gethash 'gdk-event-key-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-type 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-window ---------------------------------------------------

(setf (gethash 'gdk-event-key-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-window 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-send-event -----------------------------------------------

(setf (gethash 'gdk-event-key-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-send-event 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-time -----------------------------------------------------

(setf (gethash 'gdk-event-key-time atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-time 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"time\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-state ----------------------------------------------------

(setf (gethash 'gdk-event-key-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-state 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"state\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-keyval ---------------------------------------------------

(setf (gethash 'gdk-event-key-keyval atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-keyval 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"keyval\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-length ---------------------------------------------------

(setf (gethash 'gdk-event-key-length atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-length 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"length\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-string ---------------------------------------------------

(setf (gethash 'gdk-event-key-string atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-string 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"string\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-keycode --------------------------------------------------

(setf (gethash 'gdk-event-key-keycode atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-keycode 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"keycode\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-group ----------------------------------------------------

(setf (gethash 'gdk-event-key-group atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-group 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"group\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-key-is-modifier ----------------------------------------------

(setf (gethash 'gdk-event-key-is-modifier atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-key-is-modifier 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"is-modifier\" of the @class{gdk-event-key} struct.
  @end{short}")

;;; --- gdk-event-button -------------------------------------------------------

(setf (documentation 'gdk-event-button 'type)
 "@version{2013-1-11}
  @short{Used for button press and button release events.}
  The type field will be one of GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS,
  GDK_3BUTTON_PRESS or GDK_BUTTON_RELEASE.
  @begin{pre}
    struct GdkEventButton {
      GdkEventType type;
      GdkWindow *window;
      gint8 send_event;
      guint32 time;
      gdouble x;
      gdouble y;
      gdouble *axes;
      guint state;
      guint button;
      GdkDevice *device;
      gdouble x_root, y_root;
    @};
  @end{pre}
  Double and triple-clicks result in a sequence of events being received. For
  double-clicks the order of events will be:
  @begin{pre}
      GDK_BUTTON_PRESS

      GDK_BUTTON_RELEASE
 
      GDK_BUTTON_PRESS

      GDK_2BUTTON_PRESS

      GDK_BUTTON_RELEASE
  @end{pre}
  Note that the first click is received just like a normal button press, while
  the second click results in a GDK_2BUTTON_PRESS being received just after
  the GDK_BUTTON_PRESS.

  Triple-clicks are very similar to double-clicks, except that
  GDK_3BUTTON_PRESS is inserted after the third click. The order of the events
  is:
  @begin{pre}
      GDK_BUTTON_PRESS

      GDK_BUTTON_RELEASE

      GDK_BUTTON_PRESS

      GDK_2BUTTON_PRESS

      GDK_BUTTON_RELEASE

      GDK_BUTTON_PRESS

      GDK_3BUTTON_PRESS

      GDK_BUTTON_RELEASE
  @end{pre}
  For a double click to occur, the second button press must occur within 1/4
  of a second of the first. For a triple click to occur, the third button
  press must also occur within 1/2 second of the first button press.
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_BUTTON_PRESS,
      GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS or GDK_BUTTON_RELEASE).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[gdouble x]{the x coordinate of the pointer relative to the window.}
    @entry[gdouble y]{the y coordinate of the pointer relative to the window.}
    @entry[gdouble *axes]{x, y translated to the axes of device, or NULL if
      device is the mouse.}
    @entry[guint state]{a bit-mask representing the state of the modifier keys
     (e.g. Control, Shift and Alt) and the pointer buttons. See
     GdkModifierType.}
    @entry[guint button]{the button which was pressed or released, numbered from
      1 to 5. Normally button 1 is the left mouse button, 2 is the middle
      button, and 3 is the right button. On 2-button mice, the middle button can
      often be simulated by pressing both mouse buttons together.}
    @entry[GdkDevice *device]{the device where the event originated.}
    @entry[gdouble x_root]{the x coordinate of the pointer relative to the root
      of the screen.}
    @entry[gdouble y_root]{the y coordinate of the pointer relative to the root
      of the screen.}
  @end{table}
  @see-slot{gdk-event-button-type}
  @see-slot{gdk-event-button-window}
  @see-slot{gdk-event-button-send-event}
  @see-slot{gdk-event-button-time}
  @see-slot{gdk-event-button-x}
  @see-slot{gdk-event-button-y}
  @see-slot{gdk-event-button-axes}
  @see-slot{gdk-event-button-state}
  @see-slot{gdk-event-button-button}
  @see-slot{gdk-event-button-device}
  @see-slot{gdk-event-button-x-root}
  @see-slot{gdk-event-button-y-root}
")

;;; --- gdk-event-button-type --------------------------------------------------

(setf (gethash 'gdk-event-button-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-type 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-window ------------------------------------------------

(setf (gethash 'gdk-event-button-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-window 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-send-event --------------------------------------------

(setf (gethash 'gdk-event-button-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-send-event 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-time --------------------------------------------------

(setf (gethash 'gdk-event-button-time atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-time 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"time\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-x -----------------------------------------------------

(setf (gethash 'gdk-event-button-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-x 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"x\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-y -----------------------------------------------------

(setf (gethash 'gdk-event-button-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-y 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"y\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-axes --------------------------------------------------

(setf (gethash 'gdk-event-button-axes atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-axes 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"axes\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-state -------------------------------------------------

(setf (gethash 'gdk-event-button-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-state 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"state\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-button ------------------------------------------------

(setf (gethash 'gdk-event-button-button atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-button 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"button\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-device ------------------------------------------------

(setf (gethash 'gdk-event-button-device atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-device 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"device\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-x-root ------------------------------------------------

(setf (gethash 'gdk-event-button-x-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-x-root 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"x-root\" of the @class{gdk-event-button} struct.
  @end{short}")

;;; --- gdk-event-button-y-root ------------------------------------------------

(setf (gethash 'gdk-event-button-y-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-button-y-root 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"y-root\" of the @class{gdk-event-button} struct.
  @end{short}")

#|
;;; ----------------------------------------------------------------------------
;;; struct GdkEventTouch
;;;
;;; struct GdkEventTouch {
;;;   GdkEventType type;
;;;   GdkWindow *window;
;;;   gint8 send_event;
;;;   guint32 time;
;;;   gdouble x;
;;;   gdouble y;
;;;   gdouble *axes;
;;;   guint state;
;;;   GdkEventSequence *sequence;
;;;   gboolean emulating_pointer;
;;;   GdkDevice *device;
;;;   gdouble x_root, y_root;
;;; };
;;;
;;; Used for touch events. type field will be one of GDK_TOUCH_BEGIN,
;;; GDK_TOUCH_UPDATE, GDK_TOUCH_END or GDK_TOUCH_CANCEL.
;;;
;;; Touch events are grouped into sequences by means of the sequence field,
;;; which can also be obtained with gdk_event_get_event_sequence(). Each
;;; sequence begins with a GDK_TOUCH_BEGIN event, followed by any number of
;;; GDK_TOUCH_UPDATE events, and ends with a GDK_TOUCH_END (or GDK_TOUCH_CANCEL)
;;; event. With multitouch devices, there may be several active sequences at the
;;; same time.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_TOUCH_BEGIN, GDK_TOUCH_UPDATE, GDK_TOUCH_END,
;;;     GDK_TOUCH_CANCEL)
;;;
;;; GdkWindow *window;
;;;     the window which received the event
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent)
;;;
;;; guint32 time;
;;;     the time of the event in milliseconds.
;;;
;;; gdouble x;
;;;     the x coordinate of the pointer relative to the window
;;;
;;; gdouble y;
;;;     the y coordinate of the pointer relative to the window
;;;
;;; gdouble *axes;
;;;     x, y translated to the axes of device, or NULL if device is the mouse
;;;
;;; guint state;
;;;     a bit-mask representing the state of the modifier keys (e.g. Control,
;;;     Shift and Alt) and the pointer buttons. See GdkModifierType.
;;;
;;; GdkEventSequence *sequence;
;;;     the event sequence that the event belongs to
;;;
;;; gboolean emulating_pointer;
;;;     whether the event should be used for emulating pointer event
;;;
;;; GdkDevice *device;
;;;     the device where the event originated
;;;
;;; gdouble x_root;
;;;     the x coordinate of the pointer relative to the root of the screen
;;;
;;; gdouble y_root;
;;;     the y coordinate of the pointer relative to the root of the screen
;;; ----------------------------------------------------------------------------
|#

;;; --- gdk-event-scroll -------------------------------------------------------

(setf (documentation 'gdk-event-scroll 'type)
 "@version{2013-1-11}
  @begin{short}
    Generated from button presses for the buttons 4 to 7. Wheel mice are usually
    configured to generate button press events for buttons 4 and 5 when the
    wheel is turned.
  @end{short}

  Some GDK backends can also generate 'smooth' scroll events, which can be
  recognized by the GDK_SCROLL_SMOOTH scroll direction. For these, the scroll
  deltas can be obtained with gdk_event_get_scroll_deltas().
  @begin{pre}
  struct GdkEventScroll {
    GdkEventType type;
    GdkWindow *window;
    gint8 send_event;
    guint32 time;
    gdouble x;
    gdouble y;
    guint state;
    GdkScrollDirection direction;
    GdkDevice *device;
    gdouble x_root, y_root;
    gdouble delta_x;
    gdouble delta_y;
  @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_SCROLL).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[gdouble x]{the x coordinate of the pointer relative to the window.}
    @entry[gdouble y]{the y coordinate of the pointer relative to the window.}
    @entry[guint state]{a bit-mask representing the state of the modifier keys
     (e.g. Control,Shift and Alt) and the pointer buttons. See GdkModifierType.}
    @entry[GdkScrollDirection direction]{the direction to scroll to (one of
      GDK_SCROLL_UP, GDK_SCROLL_DOWN, GDK_SCROLL_LEFT, GDK_SCROLL_RIGHT or
      GDK_SCROLL_SMOOTH).}
    @entry[GdkDevice *device]{the device where the event originated.}
    @entry[gdouble x_root]{the x coordinate of the pointer relative to the root
      of the screen.}
    @entry[gdouble y_root]{the y coordinate of the pointer relative to the root
      of the screen.}
    @entry[gdouble delta_x]{}
    @entry[gdouble delta_y]{}
  @end{table}
  @see-slot{gdk-event-scroll-type}
  @see-slot{gdk-event-scroll-window}
  @see-slot{gdk-event-scroll-send-event}
  @see-slot{gdk-event-scroll-time}
  @see-slot{gdk-event-scroll-x}
  @see-slot{gdk-event-scroll-y}
  @see-slot{gdk-event-scroll-state}
  @see-slot{gdk-event-scroll-direction}
  @see-slot{gdk-event-scroll-device}
  @see-slot{gdk-event-scroll-x-root}
  @see-slot{gdk-event-scroll-y-root}
  @see-slot{gdk-event-scroll-delta-x}
  @see-slot{gdk-event-scroll-delta-y}
")

;;; --- gdk-event-scroll-type --------------------------------------------------

(setf (gethash 'gdk-event-scroll-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-type 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-window ------------------------------------------------

(setf (gethash 'gdk-event-scroll-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-window 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-send-event --------------------------------------------

(setf (gethash 'gdk-event-scroll-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-send-event 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-time --------------------------------------------------

(setf (gethash 'gdk-event-scroll-time atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-time 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"time\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-x -----------------------------------------------------

(setf (gethash 'gdk-event-scroll-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-x 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"x\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-y -----------------------------------------------------

(setf (gethash 'gdk-event-scroll-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-y 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"y\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-state -------------------------------------------------

(setf (gethash 'gdk-event-scroll-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-state 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"state\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-direction ---------------------------------------------

(setf (gethash 'gdk-event-scroll-direction atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-direction 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"direction\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-device ------------------------------------------------

(setf (gethash 'gdk-event-scroll-device atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-device 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"device\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-x-root ------------------------------------------------

(setf (gethash 'gdk-event-scroll-x-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-x-root 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"x-root\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-y-root ------------------------------------------------

(setf (gethash 'gdk-event-scroll-y-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-y-root 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"y-root\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-delta-x -----------------------------------------------

(setf (gethash 'gdk-event-scroll-delta-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-delta-x 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"delta-x\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-scroll-state -------------------------------------------------

(setf (gethash 'gdk-event-scroll-delta-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-scroll-delta-y 'function)
 "@version{2013-1-11}
  @begin{short}
    Accessor of the slot \"delta-y\" of the @class{gdk-event-scroll} struct.
  @end{short}")

;;; --- gdk-event-motion -------------------------------------------------------

(setf (documentation 'gdk-event-motion 'type)
 "@version{2013-1-17}
  @begin{short}
    Generated when the pointer moves.
  @end{short}
  @begin{pre}
 struct GdkEventMotion {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   guint32 time;
   gdouble x;
   gdouble y;
   gdouble *axes;
   guint state;
   gint16 is_hint;
   GdkDevice *device;
   gdouble x_root, y_root;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event.}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[gdouble x]{the x coordinate of the pointer relative to the window.}
    @entry[gdouble y]{the y coordinate of the pointer relative to the window.}
    @entry[gdouble *axes]{x, y translated to the axes of device, or NULL if
      device is the mouse.}
    @entry[guint state]{a bit-mask representing the state of the modifier keys
      (e.g. Control, Shift and Alt) and the pointer buttons. See
      GdkModifierType.}
    @entry[gint16 is_hint]{set to 1 if this event is just a hint, see the
      GDK_POINTER_MOTION_HINT_MASK value of GdkEventMask.}
    @entry[GdkDevice *device]{the device where the event originated.}
    @entry[gdouble x_root]{the x coordinate of the pointer relative to the root
      of the screen.}
    @entry[gdouble y_root]{the y coordinate of the pointer relative to the root
      of the screen.}
  @end{table}
  @see-slot{gdk-event-motion-type}
  @see-slot{gdk-event-motion-window}
  @see-slot{gdk-event-motion-send-event}
  @see-slot{gdk-event-motion-time}
  @see-slot{gdk-event-motion-x}
  @see-slot{gdk-event-motion-y}
  @see-slot{gdk-event-motion-axes}
  @see-slot{gdk-event-motion-state}
  @see-slot{gdk-event-motion-is-hint}
  @see-slot{gdk-event-motion-device}
  @see-slot{gdk-event-motion-x-root}
  @see-slot{gdk-event-motion-y-root}")

;;; --- gdk-event-motion-type --------------------------------------------------

(setf (gethash 'gdk-event-motion-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-type 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"motion\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-window ------------------------------------------------

(setf (gethash 'gdk-event-motion-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-window 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"window\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-send-event --------------------------------------------

(setf (gethash 'gdk-event-motion-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-send-event 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"send-event\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-time --------------------------------------------------

(setf (gethash 'gdk-event-motion-time atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-time 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"time\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-x -----------------------------------------------------

(setf (gethash 'gdk-event-motion-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-x 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"x\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-y -----------------------------------------------------

(setf (gethash 'gdk-event-motion-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-y 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"y\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-axes --------------------------------------------------

(setf (gethash 'gdk-event-motion-axes atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-axes 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"axes\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-state -------------------------------------------------

(setf (gethash 'gdk-event-motion-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-state 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"state\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-is-hint -----------------------------------------------

(setf (gethash 'gdk-event-motion-is-hint atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-is-hint 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"is-hint\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-device ------------------------------------------------

(setf (gethash 'gdk-event-motion-device atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-device 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"device\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-x-root ------------------------------------------------

(setf (gethash 'gdk-event-motion-x-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-x-root 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"x-root\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-motion-y-root ------------------------------------------------

(setf (gethash 'gdk-event-motion-y-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-motion-y-root 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"y-root\"} of the @class{gdk-event-motion}
    struct.
  @end{short}")

;;; --- gdk-event-expose -------------------------------------------------------

(setf (documentation 'gdk-event-expose 'type)
 "@version{2013-1-18}
  @begin{short}
    Generated when all or part of a window becomes visible and needs to be
    redrawn.
  @end{short}
  @begin{pre}
 struct GdkEventExpose {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkRectangle area;
   cairo_region_t *region;
   gint count; /* If non-zero, how many more events follow. */
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_EXPOSE or GDK_DAMAGE).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[GdkRectangle area]{bounding box of region.}
    @entry[cairo_region_t *region]{the region that needs to be redrawn.}
    @entry[gint count]{the number of contiguous GDK_EXPOSE events following this
      one. The only use for this is \"exposure compression\", i.e. handling all
      contiguous GDK_EXPOSE events in one go, though GDK performs some exposure
      compression so this is not normally needed.}
  @end{table}
  @see-slot{gdk-event-expose-type}
  @see-slot{gdk-event-expose-window}
  @see-slot{gdk-event-expose-send-event}
  @see-slot{gdk-event-expose-area}
  @see-slot{gdk-event-expose-region}
  @see-slot{gdk-event-expose-count}")

;;; --- gdk-event-expose-type --------------------------------------------------

(setf (gethash 'gdk-event-expose-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-expose-type 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"type\"} of the @class{gdk-event-expose}
    struct.
  @end{short}")

;;; --- gdk-event-expose-window ------------------------------------------------

(setf (gethash 'gdk-event-expose-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-expose-window 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"window\"} of the @class{gdk-event-expose}
    struct.
  @end{short}")

;;; --- gdk-event-expose-send-event --------------------------------------------

(setf (gethash 'gdk-event-expose-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-expose-send-event 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"send-event\"} of the @class{gdk-event-expose}
    struct.
  @end{short}")

;;; --- gdk-event-expose-area --------------------------------------------------

(setf (gethash 'gdk-event-expose-area atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-expose-area 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"area\"} of the @class{gdk-event-expose}
    struct.
  @end{short}")

;;; --- gdk-event-expose-region ------------------------------------------------

(setf (gethash 'gdk-event-expose-region atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-expose-region 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"region\"} of the @class{gdk-event-expose}
    struct.
  @end{short}")

;;; --- gdk-event-expose-count -------------------------------------------------

(setf (gethash 'gdk-event-expose-count atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-expose-count 'function)
 "@version{2013-1-17}
  @begin{short}
    Accessor of the slot @arg{\"count\"} of the @class{gdk-event-expose}
    struct.
  @end{short}")

;;; --- gdk-event-visibility ---------------------------------------------------

(setf (documentation 'gdk-event-visibility 'type)
 "@version{2013-1-16}
  @short{Generated when the window visibility status has changed.}
  @begin{pre}
 struct GdkEventVisibility {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkVisibilityState state;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (@code{:notify}).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[GdkVisibilityState state]{the new visibility state
     (@code{:fully-obscured}, @code{:partial} or @code{:unobscured}).}
  @end{table}
  @see-slot{gdk-event-visibility-type}
  @see-slot{gdk-event-visibility-window}
  @see-slot{gdk-event-visibility-send-event}
  @see-slot{gdk-event-visibility-state}")

;;; --- gdk-event-visibility-type ----------------------------------------------

(setf (gethash 'gdk-event-visibility-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-visibility-type 'function)
 "@version{2013-1-16}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event-visibility} struct.
  @end{short}")

;;; --- gdk-event-visibility-window --------------------------------------------

(setf (gethash 'gdk-event-visibility-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-visibility-window 'function)
 "@version{2013-1-16}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event-visibility} struct.
  @end{short}")

;;; --- gdk-event-visibility-send-event ----------------------------------------

(setf (gethash 'gdk-event-visibility-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-visibility-send-event 'function)
 "@version{2013-1-16}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event-visibility}
    struct.
  @end{short}")

;;; --- gdk-event-visibility-state ---------------------------------------------

(setf (gethash 'gdk-event-visibility-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-visibility-state 'function)
 "@version{2013-1-16}
  @begin{short}
    Accessor of the slot \"state\" of the @class{gdk-event-visibility} struct.
  @end{short}")

;;; --- gdk-event-crossing -----------------------------------------------------

(setf (documentation 'gdk-event-crossing 'type)
 "@version{2013-1-15}
  @short{Generated when the pointer enters or leaves a window.}
  @begin{pre}
 struct GdkEventCrossing {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkWindow *subwindow;
   guint32 time;
   gdouble x;
   gdouble y;
   gdouble x_root;
   gdouble y_root;
   GdkCrossingMode mode;
   GdkNotifyType detail;
   gboolean focus;
   guint state;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_ENTER_NOTIFY or
      GDK_LEAVE_NOTIFY).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[GdkWindow *subwindow]{the window that was entered or left.}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[gdouble x]{the x coordinate of the pointer relative to the window.}
    @entry[gdouble y]{the y coordinate of the pointer relative to the window.}
    @entry[gdouble x_root]{the x coordinate of the pointer relative to the root
      of the screen.}
    @entry[gdouble y_root]{the y coordinate of the pointer relative to the root
      of the screen.}
    @entry[GdkCrossingMode mode]{the crossing mode (GDK_CROSSING_NORMAL,
      GDK_CROSSING_GRAB, GDK_CROSSING_UNGRAB, GDK_CROSSING_GTK_GRAB,
      GDK_CROSSING_GTK_UNGRAB or GDK_CROSSING_STATE_CHANGED).
      GDK_CROSSING_GTK_GRAB, GDK_CROSSING_GTK_UNGRAB, and
      GDK_CROSSING_STATE_CHANGED were added in 2.14 and are always synthesized,
      never native.}
    @entry[GdkNotifyType detail]{the kind of crossing that happened
     (GDK_NOTIFY_INFERIOR, GDK_NOTIFY_ANCESTOR, GDK_NOTIFY_VIRTUAL,
     GDK_NOTIFY_NONLINEAR or GDK_NOTIFY_NONLINEAR_VIRTUAL).}
    @entry[gboolean focus]{TRUE if window is the focus window or an inferior.}
    @entry[guint state]{a bit-mask representing the state of the modifier keys
    (e.g. Control, Shift and Alt) and the pointer buttons. See GdkModifierType.}
  @end{table}
  @see-slot{gdk-event-crossing-type}
  @see-slot{gdk-event-crossing-window}
  @see-slot{gdk-event-crossing-send-event}
  @see-slot{gdk-event-crossing-time}
  @see-slot{gdk-event-crossing-x}
  @see-slot{gdk-event-crossing-y}
  @see-slot{gdk-event-crossing-axes}
  @see-slot{gdk-event-crossing-state}
  @see-slot{gdk-event-crossing-is-hint}
  @see-slot{gdk-event-crossing-device}
  @see-slot{gdk-event-crossing-x-root}
  @see-slot{gdk-event-crossing-y-root}
  @see-slot{gdk-event-crossing-mode}
  @see-slot{gdk-event-crossing-detail}
  @see-slot{gdk-event-crossing-focus}
  @see-slot{gdk-event-crossing-state}")

;;; --- gdk-event-crossing-type ------------------------------------------------

(setf (gethash 'gdk-event-crossing-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-type 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-window ----------------------------------------------

(setf (gethash 'gdk-event-crossing-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-window 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-send-event ------------------------------------------

(setf (gethash 'gdk-event-crossing-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-send-event 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event-crossing}
    struct.
  @end{short}")

;;; --- gdk-event-crossing-time ------------------------------------------------

(setf (gethash 'gdk-event-crossing-time atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-time 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"time\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-x ---------------------------------------------------

(setf (gethash 'gdk-event-crossing-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-x 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"x\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-y ---------------------------------------------------

(setf (gethash 'gdk-event-crossing-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-y 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"y\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-axes ------------------------------------------------

(setf (gethash 'gdk-event-crossing-axes atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-axes 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"axes\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-state -----------------------------------------------

(setf (gethash 'gdk-event-crossing-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-state 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"state\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-is-hint ---------------------------------------------

(setf (gethash 'gdk-event-crossing-is-hint atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-is-hint 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"is-hint\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-device ----------------------------------------------

(setf (gethash 'gdk-event-crossing-device atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-device 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"device\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-x-root ----------------------------------------------

(setf (gethash 'gdk-event-crossing-x-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-x-root 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"x-root\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-y-root ----------------------------------------------

(setf (gethash 'gdk-event-crossing-y-root atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-y-root 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"y-root\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-mode ------------------------------------------------

(setf (gethash 'gdk-event-crossing-mode atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-mode 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"mode\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-detail ----------------------------------------------

(setf (gethash 'gdk-event-crossing-detail atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-detail 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"detail\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-focus -----------------------------------------------

(setf (gethash 'gdk-event-crossing-focus atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-focus 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"focus\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-crossing-state -----------------------------------------------

(setf (gethash 'gdk-event-crossing-state atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-crossing-state 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"state\" of the @class{gdk-event-crossing} struct.
  @end{short}")

;;; --- gdk-event-focus --------------------------------------------------------

(setf (documentation 'gdk-event-focus 'type)
 "@version{2013-1-18}
  @short{Describes a change of keyboard focus.}
  @begin{pre}
 struct GdkEventFocus {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   gint16 in;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_FOCUS_CHANGE).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[gint16 in]{TRUE if the window has gained the keyboard focus, FALSE if
      it has lost the focus.}
  @end{table}
  @see-slot{gdk-event-focus-type}
  @see-slot{gdk-event-focus-window}
  @see-slot{gdk-event-focus-send-event}
  @see-slot{gdk-event-focus-in}")

;;; --- gdk-event-focus-type ---------------------------------------------------

(setf (gethash 'gdk-event-focus-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-focus-type 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"type\" of the @class{gdk-event-focus} struct.
  @end{short}")

;;; --- gdk-event-focus-window -------------------------------------------------

(setf (gethash 'gdk-event-focus-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-focus-window 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gdk-event-focus} struct.
  @end{short}")

;;; --- gdk-event-focus-send-event ---------------------------------------------

(setf (gethash 'gdk-event-focus-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-focus-send-event 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot \"send-event\" of the @class{gdk-event-focus} struct.
  @end{short}")

;;; --- gdk-event-focus-in -----------------------------------------------------

(setf (gethash 'gdk-event-focus-in atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-focus-in 'function)
 "@version{2013-1-15}
  @begin{short}
    Accessor of the slot \"in\" of the @class{gdk-event-focus} struct.
  @end{short}")

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
;;;     (GDK_PROPERTY_DELETE).
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
;;;   GdkWindow *requestor;
;;; };
;;;
;;; Generated when a selection is requested or ownership of a selection is taken
;;; over by another client application.
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
;;; GdkWindow *requestor;
;;;     the window on which to place property or NULL if none.
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
;;;     the x coordinate of the pointer relative to the root of the screen, only
;;;     set for GDK_DRAG_MOTION and GDK_DROP_START.
;;;
;;; gshort y_root;
;;;     the y coordinate of the pointer relative to the root of the screen, only
;;;     set for GDK_DRAG_MOTION and GDK_DROP_START.
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
;;; indicates that the stylus has moved in or out of contact with the tablet, or
;;; perhaps that the user's finger has moved in or out of contact with a touch
;;; screen.
;;;
;;; This event type will be used pretty rarely. It only is important for XInput
;;; aware programs that are drawing their own cursor.
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
;;;   GdkWindow *owner;
;;;   GdkOwnerChange reason;
;;;   GdkAtom selection;
;;;   guint32 time;
;;;   guint32 selection_time;
;;; };
;;;
;;; Generated when the owner of a selection changes. On X11, this information is
;;; only available if the X server supports the XFIXES extension.
;;;
;;; GdkEventType type;
;;;     the type of the event (GDK_OWNER_CHANGE).
;;;
;;; GdkWindow *window;
;;;     the window which received the event
;;;
;;; gint8 send_event;
;;;     TRUE if the event was sent explicitly (e.g. using XSendEvent)
;;;
;;; GdkWindow *owner;
;;;     the new owner of the selection, or NULL if there is none
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


;;; --- gdk-event-grab-broken --------------------------------------------------

(setf (documentation 'gdk-event-grab-broken 'type)
 "@version{2013-1-19}
  @begin{short}
    Generated when a pointer or keyboard grab is broken.
  @end{short}
  On X11, this happens when the grab window becomes unviewable (i.e. it or one
  of its ancestors is unmapped), or if the same application grabs the pointer or
  keyboard again. Note that implicit grabs (which are initiated by button
  presses) can also cause @sym{gdk-event-grab-broken} events.
  @begin{pre}
 struct GdkEventGrabBroken {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   gboolean keyboard;
   gboolean implicit;
   GdkWindow *grab_window;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_GRAB_BROKEN)}
    @entry[GdkWindow *window]{the window which received the event, i.e. the
      window that previously owned the grab}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[gboolean keyboard]{TRUE if a keyboard grab was broken, FALSE if a
      pointer grab was broken}
    @entry[gboolean implicit]{TRUE if the broken grab was implicit}
    @entry[GdkWindow *grab_window]{If this event is caused by another grab in
      the same application, grab_window contains the new grab window.
      Otherwise grab_window is NULL.}
  @end{table}
  Since 2.8
  @see-slot{gdk-event-grab-broken-type}
  @see-slot{gdk-event-grab-broken-window}
  @see-slot{gdk-event-grab-broken-send-event}
  @see-slot{gdk-event-grab-broken-keyboard}
  @see-slot{gdk-event-grab-broken-implicit}
  @see-slot{gdk-event-grab-broken-grab-window}")

;;; --- gdk-event-grab-broken-type ---------------------------------------------

(setf (gethash 'gdk-event-grab-broken-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-grab-broken-type 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @arg{\"type\"} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-window -------------------------------------------

(setf (gethash 'gdk-event-grab-broken-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-grab-broken-window 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @arg{\"window\"} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-send-event ---------------------------------------

(setf (gethash 'gdk-event-grab-broken-send-event atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-grab-broken-send-event 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @arg{\"send-event\"} of the
    @class{gdk-event-grab-broken} struct.
  @end{short}")

;;; --- gdk-event-grab-broken-keyboard -----------------------------------------

(setf (gethash 'gdk-event-grab-broken-keyboard atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-grab-broken-keyboard 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @arg{\"keyboard\"} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-implicit -----------------------------------------

(setf (gethash 'gdk-event-grab-broken-implicit atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-grab-broken-implicit 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @arg{\"implicit\"} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-grab-window --------------------------------------

(setf (gethash 'gdk-event-grab-broken-grab-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gdk-event-grab-broken-grab-window 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @arg{\"grab-window\"} of the
    @class{gdk-event-grab-broken} struct.
  @end{short}")

;;; --- End of file atdoc-gdk.event-structures.lisp ----------------------------

