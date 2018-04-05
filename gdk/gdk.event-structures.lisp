;;; ----------------------------------------------------------------------------
;;; gdk.event-structures.lisp
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
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;;     GdkScrollDirection
;;;     GdkVisibilityState
;;;     GdkCrossingMode
;;;     GdkNotifyType
;;;     GdkPropertyState
;;;     GdkWindowState
;;;     GdkSettingAction
;;;     GdkOwnerChange
;;;
;;;     GdkEventType     <-- gdk.events.lisp
;;;     GdkModifierType  <-- gdk.window.lisp
;;;     GdkEventMask     <-- gdk.events.lisp
;;;     GdkEventSequence <-- gdk-events.lisp
;;;
;;;     GdkEvent
;;;
;;;     GdkEventAny
;;;     GdkEventKey
;;;     GdkEventButton
;;;     GdkEventTouch
;;;     GdkEventScroll
;;;     GdkEventMotion
;;;     GdkEventExpose
;;;     GdkEventVisibility
;;;     GdkEventCrossing
;;;     GdkEventFocus
;;;     GdkEventConfigure
;;;     GdkEventProperty
;;;     GdkEventSelection
;;;     GdkEventDND
;;;     GdkEventProximity
;;;     GdkEventWindowState
;;;     GdkEventSetting
;;;     GdkEventOwnerChange
;;;     GdkEventGrabBroken
;;;
;;; Description
;;;
;;; The event structs contain data specific to each type of event in GDK.
;;;
;;; Note
;;;
;;; A common mistake is to forget to set the event mask of a widget so that the
;;; required events are received. See gtk_widget_set_events().
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
;;; enum GdkScrollDirection
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkScrollDirection" gdk-scroll-direction
  (:export t
   :type-initializer "gdk_scroll_direction_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-scroll-direction atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-scroll-direction atdoc:*external-symbols*)
 "@version{2013-9-20}
  @short{Specifies the direction for @class{gdk-event-scroll}.}
  @begin{pre}
(define-g-enum \"GdkScrollDirection\" gdk-scroll-direction
  (:export t
   :type-initializer \"gdk_scroll_direction_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{The window is scrolled up.}
    @entry[:down]{The window is scrolled down.}
    @entry[:left]{The window is scrolled to the left.}
    @entry[:right]{The window is scrolled to the right.}
    @entry[:smooth]{The scrolling is determined by the delta values in
      @class{gdk-event-scroll}. See the function
      @fun{gdk-event-get-scroll-deltas}.}
  @end{table}
  @see-class{gdk-event-scroll}
  @see-function{gdk-event-get-scroll-delta}")

;;; ----------------------------------------------------------------------------
;;; enum GdkVisibilityState
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkVisibilityState" gdk-visibility-state
  (:export t
   :type-initializer "gdk_visibility_state_get_type")
  (:unobscured 0)
  (:partial 1)
  (:fully-obscured 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-visibility-state atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-visibility-state atdoc:*external-symbols*)
 "@version{2013-9-20}
  @begin{short}
    Specifies the visiblity status of a window for a
    @class{gdk-event-visibility}.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkVisibilityState\" gdk-visibility-state
  (:export t
   :type-initializer \"gdk_visibility_state_get_type\")
  (:unobscured 0)
  (:partial 1)
  (:fully-obscured 2))
  @end{pre}
  @begin[code]{table}
    @entry[:unobscured]{The window is completely visible.}
    @entry[:partial]{The window is partially visible.}
    @entry[:obscured]{The window is not visible at all.}
  @end{table}
  @see-class{gdk-event-visibility}")

;;; ----------------------------------------------------------------------------
;;; enum GdkCrossingMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkCrosssingMode" gdk-crossing-mode
  (:export t
   :type-initializer "gdk_crossing_mode_get_type")
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed
  :touch-begin
  :touch-end
  :device-switch)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-crossing-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-crossing-mode atdoc:*external-symbols*)
 "@version{2013-9-20}
  @short{Specifies the crossing mode for @class{gdk-event-crossing}.}
  @begin{pre}
(define-g-enum \"GdkCrosssingMode\" gdk-crossing-mode
  (:export t
   :type-initializer \"gdk_crossing_mode_get_type\")
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
    @entry[:normal]{Crossing because of pointer motion.}
    @entry[:grab]{Crossing because a grab is activated.}
    @entry[:ungrab]{Crossing because a grab is deactivated.}
    @entry[:gtk-grab]{Crossing because a GTK+ grab is activated.}
    @entry[:gtk-ungrab]{Crossing because a GTK+ grab is deactivated.}
    @entry[:state-changed]{Crossing because a GTK+ widget changed state (e. g.
      sensitivity).}
    @entry[:touch-begin]{Crossing because a touch sequence has begun, this
      event is synthetic as the pointer might have not left the window.}
    @entry[:touch-end]{Crossing because a touch sequence has ended, this event
      is synthetic as the pointer might have not left the window.}
    @entry[:device-switch]{crossing because of a device switch (i. e. a mouse
      taking control of the pointer after a touch device), this event is
      synthetic as the pointer didn't leave the window.}
  @end{table}
  @see-class{gdk-event-crossing}")

;;; ----------------------------------------------------------------------------
;;; enum GdkNotifyType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkNotifyType" gdk-notify-type
  (:export t
   :type-initializer "gdk_notify_type_get_type")
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-notify-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-notify-type atdoc:*external-symbols*)
 "@version{2013-9-21}
  @short{Specifies the kind of crossing for @class{gdk-event-crossing}.}

  See the X11 protocol specification of @code{LeaveNotify} for full details of
  crossing event generation.
  @begin{pre}
(define-g-enum gdk-notify-type
  (:export t
   :type-initializer \"gdk_notify_type_get_type\")
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)
  @end{pre}
  @begin[code]{table}
    @entry[:ancestor]{The window is entered from an ancestor or left towards
      an ancestor.}
    @entry[:virtual]{The pointer moves between an ancestor and an inferior of
      the window.}
    @entry[:inferior]{The window is entered from an inferior or left towards
      an inferior.}
    @entry[:nonlinear]{The window is entered from or left towards a window
      which is neither an ancestor nor an inferior.}
    @entry[:nonlinear-virtual]{The pointer moves between two windows which are
      not ancestors of each other and the window is part of the ancestor chain
      between one of these windows and their least common ancestor.}
    @entry[:unknown]{An unknown type of enter/leave event occurred.}
  @end{table}
  @see-class{gdk-event-crossing}")

;;; ----------------------------------------------------------------------------
;;; enum GdkPropertyState
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkPropertyState" gdk-property-state
  (:export t
   :type-initializer "gdk_property_state_get_type")
  :new-value
  :delete)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-property-state atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-property-state atdoc:*external-symbols*)
 "@version{2013-9-21}
  @begin{short}
    Specifies the type of a property change for a @class{gdk-event-property}.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkPropertyState\" gdk-property-state
  (:export t
   :type-initializer \"gdk_property_state_get_type\")
  :new-value
  :delete)
  @end{pre}
  @begin[code]{table}
    @entry[:new-value]{The property value was changed.}
    @entry[:delete]{The property was deleted.}
  @end{table}
  @see-class{gdk-event-property}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowState
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
  (:below 64)
  (:focused 128)
  (:tiled 256))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-state atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-window-state atdoc:*external-symbols*)
 "@version{2013-9-21}
  @short{Specifies the state of a toplevel window.}
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
    @entry[:withdrawn]{The window is not shown.}
    @entry[:iconified]{The window is minimized.}
    @entry[:maximized]{The window is maximized.}
    @entry[:sticky]{The window is sticky.}
    @entry[:fullscreen]{The window is maximized without decorations.}
    @entry[:above]{The window is kept above other windows.}
    @entry[:below]{The window is kept below other windows.}
    @entry[:focused]{The window is presented as focused (with active
      decorations).}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkSettingAction
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkSettingAction" gdk-setting-action
  (:export t
   :type-initializer "gdk_setting_action_get_type")
  (:new 0)
  (:changed 1)
  (:deleted 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-setting-action atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-setting-action atdoc:*external-symbols*)
 "@version{2013-9-21}
  @begin{short}
    Specifies the kind of modification applied to a setting in a
    @class{gdk-event-setting}.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkSettingAction\" gdk-setting-action
  (:export t
   :type-initializer \"gdk_setting_action_get_type\")
  (:new 0)
  (:changed 1)
  (:deleted 2))
  @end{pre}
  @begin[code]{table}
    @entry[:new]{A setting was added.}
    @entry[:changes]{A setting was changed.}
    @entry[:deleted]{A setting was deleted.}
  @end{table}
  @see-class{gdk-event-setting}")

;;; ----------------------------------------------------------------------------
;;; enum GdkOwnerChange
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkOwnerChange" gdk-owner-change
  (:export t
   :type-initializer "gdk_owner_change_get_type")
  (:new-owner 0)
  (:destroy 1)
  (:close 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-owner-change atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-owner-change atdoc:*external-symbols*)
 "@version{2013-9-21}
  @short{Specifies why a selection ownership was changed.}
  @begin{pre}
(define-g-enum \"GdkOwnerChange\" gdk-owner-change
  (:export t
   :type-initializer \"gdk_owner_change_get_type\")
  (:new-owner 0)
  (:destroy 1)
  (:close 2))
  @end{pre}
  @begin[code]{table}
    @entry[:owner]{Some other app claimed the ownership.}
    @entry[:destroy]{The window was destroyed.}
    @entry[:close]{The client was closed.}
  @end{table}
  @see-class{gdk-event-owner-change}")

;;; ----------------------------------------------------------------------------
;;; enum GdkEventType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkEventType" gdk-event-type
  (:export t
   :type-initializer "gdk_event_type_get_type")
  (:nothing -1)
  (:delete 0)
  (:destroy 1)
  (:expose 2)
  (:motion-notify 3)
  (:button-press 4)
  (:2button-press 5)
  (:double-button-press 5) ; Alias for :2button-press
  (:3button-press 6)
  (:triple-button-press 6) ; Alias for :3button-press
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
  (:no-expose 30)          ; not documented
  (:scroll 31)
  (:window-state 32)
  (:setting 33)
  (:owner-change 34)
  (:grab-broken 35)
  (:damage 36)
  (:touch-begin 37)
  (:touch-update 38)
  (:touch-end 39)
  (:touch-cancel 40)
  (:touchpad-swipe 41)
  (:touchpad-pinch 42)
  (:pad-button-press 43)
  (:pad-button-release 44)
  (:pad-ring 45)
  (:pad-strip 46)
  (:pad-group-mode 47))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-event-type atdoc:*external-symbols*)
 "@version{2013-9-20}
  @short{Specifies the type of the event.}

  Do not confuse these events with the signals that GTK+ widgets emit.
  Although many of these events result in corresponding signals being emitted,
  the events are often transformed or filtered along the way.
  @begin{pre}
(define-g-enum \"GdkEventType\" gdk-event-type
  (:export t
   :type-initializer \"gdk_event_type_get_type\")
  (:nothing -1)
  (:delete 0)
  (:destroy 1)
  (:expose 2)
  (:motion-notify 3)
  (:button-press 4)
  (:2button-press 5)
  (:double-button-press 5)
  (:3button-press 6)
  (:triple-button-press 6)
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
  (:no-expose 30)   ;; not documented
  (:scroll 31)
  (:window-state 32)
  (:setting 33)
  (:owner-change 34)
  (:grab-broken 35)
  (:damage 36)
  (:touch-begin 37)
  (:touch-update 38)
  (:touch-end 39)
  (:touch-cancel 40)
  (:touchpad-swipe 41)
  (:touchpad-pinch 42)
  (:pad-button-press 43)
  (:pad-button-release 44)
  (:pad-ring 45)
  (:pad-strip 46)
  (:pad-group-mode 47))
  @end{pre}
  @begin[code]{table}
    @entry[:nothing]{A special code to indicate a null event.}
    @entry[:delete]{The window manager has requested that the toplevel window
      be hidden or destroyed, usually when the user clicks on a special icon
      in the title bar.}
    @entry[:destroy]{The window has been destroyed.}
    @entry[:expose]{All or part of the window has become visible and needs to
      be redrawn.}
    @entry[:motion-notify]{The pointer, usually a mouse, has moved.}
    @entry[:button-press]{A mouse button has been pressed.}
    @entry[:2button-press]{A mouse button has been double-clicked. Note that
      each click also generates a @code{:button-press} event.}
    @entry[:double-button-press]{Alias for @code{:2button-press}, added in 3.6.}
    @entry[3button-press]{A mouse button has been clicked 3 times in a short
      period of time. Note that each click also generates a @code{:button-press}
      event.}
    @entry[:triple-button-press]{Alias for @code{:3button-press}, added in 3.6.}
    @entry[:button-release]{A mouse button has been released.}
    @entry[:key-press]{A key has been pressed.}
    @entry[:key-release]{A key has been released.}
    @entry[:enter-notifiy]{The pointer has entered the window.}
    @entry[:leave-notify]{The pointer has left the window.}
    @entry[:focus-change]{The keyboard focus has entered or left the window.}
    @entry[:configure]{The size, position or stacking order of the window has
      changed. Note that GTK+ discards these events for @code{:child} windows
      of type @symbol{gdk-window-type}.}
    @entry[:map]{The window has been mapped.}
    @entry[:unmap]{The window has been unmapped.}
    @entry[:property-notify]{A property on the window has been changed or
      deleted.}
    @entry[:selection-clear]{The application has lost ownership of a
      selection.}
    @entry[:selection-request]{Another application has requested a selection.}
    @entry[:selection-notify]{A selection has been received.}
    @entry[:proximity-in]{An input device has moved into contact with a
      sensing surface, e. g. a touchscreen or graphics tablet.}
    @entry[:proximity-out]{An input device has moved out of contact with a
      sensing surface.}
    @entry[:drag-enter]{The mouse has entered the window while a drag is in
      progress.}
    @entry[:drag-leave]{The mouse has left the window while a drag is in
      progress.}
    @entry[:drag-motion]{The mouse has moved in the window while a drag is in
      progress.}
    @entry[:drag-status]{The status of the drag operation initiated by the
      window has changed.}
    @entry[:drop-start]{A drop operation onto the window has started.}
    @entry[:drop-finished]{The drop operation initiated by the window has
      completed.}
    @entry[:client-event]{A message has been received from another
      application.}
    @entry[:visibility-notify]{The window visibility status has changed.}
    @entry[:scroll]{The scroll wheel was turned.}
    @entry[:window-state]{The state of a window has changed. See
      @symbol{gdk-window-state} for the possible window states.}
    @entry[:setting]{A setting has been modified.}
    @entry[:owner-change]{The owner of a selection has changed. This event
      type was added in 2.6.}
    @entry[:grab-broken]{A pointer or keyboard grab was broken. This event
      type was added in 2.8.}
    @entry[:damage]{The content of the window has been changed. This event
      type was added in 2.14.}
    @entry[:touch-begin]{A new touch event sequence has just started. This
      event type was addedin 3.4.}
    @entry[:touch-update]{A touch event sequence has been updated. This event
      type was added in 3.4.}
    @entry[:touch-end]{A touch event sequence has finished. This event type was
      added in 3.4.}
    @entry[:touch-cancel]{A touch event sequence has been canceled. This event
      type was added in 3.4.}
    @entry[:touchpad-swipe]{A touchpad swipe gesture event, the current state
      is determined by its phase field. This event type was added in 3.18.}
    @entry[:touchpad-pinch]{A touchpad pinch gesture event, the current state
      is determined by its phase field. This event type was added in 3.18.}
    @entry[:pad-button-press]{A tablet pad button press event. This event type
      was added in 3.22.}
    @entry[:pad-button-release]{A tablet pad button release event. This event
      type was added in 3.22.}
    @entry[:pad-ring]{A tablet pad axis event from a "ring". This event type
      was added in 3.22.}
    @entry[:pad-strip]{A tablet pad axis event from a "strip". This event type
      was added in 3.22.}
    @entry[:pad-group-mode]{A tablet pad group mode change. This event type was
      added in 3.22.}
  @end{table}
  @see-class{gdk-event}
  @see-symbol{gdk-window-type}
  @see-symbol{gdk-window-state}")

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierType
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkModifierType" gdk-modifier-type
  (:export t
   :type-initializer "gdk_modifier_type_get_type")
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:mod1-mask    #.(ash 1 3))
  (:mod2-mask    #.(ash 1 4))
  (:mod3-mask    #.(ash 1 5))
  (:mod4-mask    #.(ash 1 6))
  (:mod5-mask    #.(ash 1 7))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28))
  (:release-mask #.(ash 1 30))
  (:modifier-mask #x5c001fff))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-modifier-type atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-modifier-type atdoc:*external-symbols*)
 "@version{2013-9-21}
  @begin{short}
    A set of bit-flags to indicate the state of modifier keys and mouse buttons
    in various event types.
  @end{short}
  Typical modifier keys are Shift, Control, Meta, Super, Hyper, Alt, Compose,
  Apple, CapsLock or ShiftLock.

  Like the X Window System, GDK supports 8 modifier keys and 5 mouse buttons.

  Since 2.10, GDK recognizes which of the Meta, Super or Hyper keys are mapped
  to Mod2 - Mod5, and indicates this by setting @code{:super-mask},
  @code{:hyper-mask} or @code{:meta-mask} in the state field of key events.

  Note that GDK may add internal values to events which include reserved
  values such as @code{GDK_MODIFIER_RESERVED_13_MASK}. Your code should preserve
  and ignore them. You can use @code{:modifier-mask} to remove all reserved
  values.

  Also note that the GDK X backend interprets button press events for button 4-7
  as scroll events, so @code{:button4-mask} and @code{:button5-mask} will never
  be set.
  @begin{pre}
(define-g-flags \"GdkModifierType\" gdk-modifier-type
  (:export t
   :type-initializer \"gdk_modifier_type_get_type\")
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:mod1-mask    #.(ash 1 3))
  (:mod2-mask    #.(ash 1 4))
  (:mod3-mask    #.(ash 1 5))
  (:mod4-mask    #.(ash 1 6))
  (:mod5-mask    #.(ash 1 7))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28))
  (:release-mask #.(ash 1 30))
  (:modifier-mask #x5c001fff))
  @end{pre}
  @begin[code]{table}
    @entry[:shift-mask]{The Shift key.}
    @entry[:lock-mask]{A Lock key, depending on the modifier mapping of the X
      server this may either be CapsLock or ShiftLock.}
    @entry[:control-mask]{The Control key.}
    @entry[:mod1-mask]{The fourth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier, but
      normally it is the Alt key.}
    @entry[:mod2-mask]{The fifth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:mod3-mask]{The sixth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:mod4-mask]{The seventh modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:mod5-mask]{The eighth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:button1-mask]{The first mouse button.}
    @entry[:button2-mask]{The second mouse button.}
    @entry[:button3-mask]{The third mouse button.}
    @entry[:button4-mask]{The fourth mouse button.}
    @entry[:button5-mask]{The fifth mouse button.}
    @entry[:super-mask]{The Super modifier. Since 2.10}
    @entry[:hyper-mask]{The Hyper modifier. Since 2.10}
    @entry[:meta-mask]{The Meta modifier. Since 2.10}
    @entry[:release-mask]{Not used in GDK itself. GTK+ uses it to differentiate
      between (keyval, modifiers) pairs from key press and release events.}
    @entry[:modifier-mask]{A mask covering all modifier types.}
  @end{table}
  @see-class{gdk-event}")

;;; ----------------------------------------------------------------------------
;;; enum GdkEventMask
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkEventMask" gdk-event-mask
  (:export t
   :type-initializer "gdk_event_mask_get_type")
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
  (:touchpad-gesture-mask #.(ash 1 24))
  (:tablet-pad-mask #.(ash 1 25))
  (:all-events-mask 4194302))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-mask atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-event-mask atdoc:*external-symbols*)
 "@version{2013-7-17}
  @begin{short}
    A set of bit-flags to indicate which events a window is to receive. Most of
    these masks map onto one or more of the @symbol{gdk-event-type} event
    types.
  @end{short}

  @code{:pointer-motion-hint-mask} is a special mask which is used to reduce
  the number of @code{:motion-notifiy} events received. Normally a
  @code{:motion-notify} event is received each time the mouse moves. However,
  if the application spends a lot of time processing the event, updating the
  display, for example, it can lag behind the position of the mouse. When using
  @code{:pointer-motion-hint-mask}, fewer @code{:motion-notify} events will
  be sent, some of which are marked as a hint (the @code{is_hint} member is
  @em{true}). To receive more motion events after a motion hint event, the
  application needs to asks for more, by calling the function
  @fun{gdk-event-request-motions}.

  If @code{:touch-mask} is enabled, the window will receive touch events from
  touch-enabled devices. Those will come as sequences of @class{gdk-event-touch}
  with type @code{:touch-update}, enclosed by two events with type
  @code{:touch-begin} and @code{:touch-end}, or @code{:touch-cancel}.
  The function @fun{gdk-event-get-event-sequence} returns the event sequence
  for these events, so different sequences may be distinguished.
  @begin{pre}
(define-g-flags \"GdkEventMask\" gdk-event-mask
  (:export t
   :type-initializer \"gdk_event_mask_get_type\")
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
  (:touchpad-gesture-mask #.(ash 1 24))
  (:tablet-pad-mask #.(ash 1 25))
  (:all-events-mask 4194302))
  @end{pre}
  @begin[code]{table}
    @entry[:exposure-mask]{Receive expose events.}
    @entry[:pointer-motion-mask]{Receive all pointer motion events.}
    @entry[:pointer-motion-hint-mask]{See the explanation above.}
    @entry[:button-motion-mask]{Receive pointer motion events while any
      button is pressed.}
    @entry[:button1-motion-mask]{Receive pointer motion events while 1
      button is pressed.}
    @entry[:button2-motion-mask]{Receive pointer motion events while 2
      button is pressed.}
    @entry[:button3-motion-mask]{Receive pointer motion events while 3
      button is pressed.}
    @entry[:button-press-mask]{Receive button press events.}
    @entry[:button-release-mask]{Receive button release events.}
    @entry[:key-press-mask]{Receive key press events.}
    @entry[:key-release-mask]{Receive key release events.}
    @entry[:enter-notify-mask]{Receive window enter events.}
    @entry[:leave-notify-mask]{Receive window leave events.}
    @entry[:focus-change-mask]{Receive focus change events.}
    @entry[:structure-mask]{Receive events about window configuration
      change.}
    @entry[:property-change-mask]{Receive property change events.}
    @entry[:visibility-notify-mask]{Receive visibility change events.}
    @entry[:proximity-in-mask]{Receive proximity in events.}
    @entry[:proximity-out-mask]{Receive proximity out events.}
    @entry[:substructure-mask]{Receive events about window configuration
      changes of child windows.}
    @entry[:scroll-mask]{Receive scroll events.}
    @entry[:touch-mask]{Receive touch events.}
    @entry[:smooth-scroll-mask]{Receive smooth scrolling events.}
    @entry[:touchpad-gesture-mask]{Receive touchpad gesture events.
      Since 3.18.}
    @entry[:tablet-pad-mask]{Receive tablet pad events. Since 3.22.}
    @entry[:all-events-mask]{The combination of all the above event masks.}
  @end{table}
  @see-symbol{gdk-event-type}
  @see-class{gdk-event-touch}
  @see-function{gdk-event-request-motions}
  @see-function{gdk-event-get-event-sequence}")

;;; ----------------------------------------------------------------------------
;;; GdkEventSequence
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gdk-event-sequence "GdkEventSequence"
  :alloc (error "GdkEventSequence can not be created from Lisp side."))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-sequence 'type)
 "@version{2013-6-17}
  See the function @fun{gdk-event-get-event-sequence}.
  @see-function{gdk-event-get-event-sequence}")

(export (boxed-related-symbols 'gdk-event-sequence))

;;; ----------------------------------------------------------------------------
;;; union GdkEvent
;;; ----------------------------------------------------------------------------

(define-g-boxed-variant-cstruct gdk-event "GdkEvent"
  (type gdk-event-type)
  (window (g-object gdk-window))
  (send-event (:boolean :int8))
  (:variant type
            ((:key-press :key-release) gdk-event-key
             (time :uint32)
             (state gdk-modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))

            ((:expose) gdk-event-expose
             (area gdk-rectangle :inline t)
             (region (:pointer (:struct cairo-region-t)))
             (count :int))

            ((:visibility-notify) gdk-event-visibility
             (state gdk-visibility-state))

            ((:motion-notify) gdk-event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state gdk-modifier-type)
             (is-hint :int16)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))

            ((:button-press
              :2button-press
              :double-button-press
              :3button-press
              :triple-button-press
              :button-release) gdk-event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (button :uint)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))

            ((:touch-begin
              :touch-update
              :touch-end
              :touch-cancel) gdk-event-touch
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (sequence (g-boxed-foreign gdk-event-sequence))
             (emulating-pointer :boolean)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))

            ((:scroll) gdk-event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state gdk-modifier-type)
             (direction gdk-scroll-direction)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double)
             (delta-x :double)
             (delta-y :double)
             #+gdk-3-20
             (is-stop :uint))           ; bitfield

            ((:enter-notify :leave-notify) gdk-event-crossing
             (subwindow (g-object gdk-window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode gdk-crossing-mode)
             (detail gdk-notify-type)
             (focus :boolean)
             (state :uint))

            ((:focus-change) gdk-event-focus
             (in :int16))

            ((:configure) gdk-event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))

            ((:property-notify) gdk-event-property
             (atom gdk-atom)
             (time :uint32)
             (state gdk-property-state))

            ((:selection-clear
              :selection-notify
              :selection-request) gdk-event-selection
             (selection gdk-atom)
             (target gdk-atom)
             (property gdk-atom)
             (time :uint32)
             (requestor (g-object gdk-window)))

            ((:owner-change) gdk-event-owner-change
             (owner (g-object gdk-window))
             (reason gdk-owner-change)
             (selection gdk-atom)
             (time :uint32)
             (selection-time :uint32))

            ((:proximity-in
              :proximity-out) gdk-event-proximity
             (time :uint32)
             (device (g-object gdk-device)))

            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) gdk-event-dnd
             (context (g-object gdk-drag-context))
             (time :uint32)
             (x-root :short)
             (y-root :short))

            ((:window-state) gdk-event-window-state
             (changed-mask gdk-window-state)
             (new-window-state gdk-window-state))

            ((:setting) gdk-event-setting
             (action gdk-setting-action)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))

            ((:grab-broken) gdk-event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g-object gdk-window)))))

(export (boxed-related-symbols 'gdk-event))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event 'type)
 "@version{2014-1-22}
  @begin{short}
    The @sym{gdk-event} structure contains a union of all of the event
    structures, and allows access to the data fields in a number of ways.
  @end{short}

  The event type is always the first field in all of the event structures, and
  can always be accessed with the following code, no matter what type of event
  it is:
  @begin{pre}
* (let ((event (make-gdk-event-button :type :button-press)))
    (gdk-event-type event))
=> :BUTTON-PRESS
  @end{pre}
  To access other fields of the event structures, the appropriate event
  structure accesor can be used. For example if the event type is
  @code{:button-press} then the x coordinate of the button press can be
  accessed with:
  @begin{pre}
* (let ((event (make-gdk-event-button :type :button-press :x 10.0)))
    (gdk-event-button-x event))
=> 10.0
  @end{pre}
  The complete variant structure which contains all event structure is as
  follows:
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  (type gdk-event-type)
  (window (g-object gdk-window))
  (send-event (:boolean :int8))
  (:variant type
            ((:key-press :key-release) gdk-event-key
             (time :uint32)
             (state gdk-modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))
            ((:expose) gdk-event-expose
             (area gdk-rectangle :inline t)
             (region (:pointer (:struct cairo-region-t)))
             (count :int))
            ((:visibility-notify) gdk-event-visibility
             (state gdk-visibility-state))
            ((:motion-notify) gdk-event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state gdk-modifier-type)
             (is-hint :int16)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
            ((:button-press
              :2button-press
              :double-button-press
              :3button-press
              :triple-button-press
              :button-release) gdk-event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (button :uint)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
            ((:touch-begin
              :touch-update
              :touch-end
              :touch-cancel) gdk-event-touch
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (sequence (g-boxed-foreign gdk-event-sequence))
             (emulating-pointer :boolean)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
            ((:scroll) gdk-event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state gdk-modifier-type)
             (direction gdk-scroll-direction)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double)
             (delta-x :double)
             (delta-y :double))
            ((:enter-notify :leave-notify) gdk-event-crossing
             (subwindow (g-object gdk-window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode gdk-crossing-mode)
             (detail gdk-notify-type)
             (focus :boolean)
             (state :uint))
            ((:focus-change) gdk-event-focus
             (in :int16))
            ((:configure) gdk-event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))
            ((:property-notify) gdk-event-property
             (atom gdk-atom)
             (time :uint32)
             (state gdk-property-state))
            ((:selection-clear
              :selection-notify
              :selection-request) gdk-event-selection
             (selection gdk-atom)
             (target gdk-atom)
             (property gdk-atom)
             (time :uint32)
             (requestor (g-object gdk-window)))
            ((:owner-change) gdk-event-owner-change
             (owner (g-object gdk-window))
             (reason gdk-owner-change)
             (selection gdk-atom)
             (time :uint32)
             (selection-time :uint32))
            ((:proximity-in
              :proximity-out) gdk-event-proximity
             (time :uint32)
             (device (g-object gdk-device)))
            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) gdk-event-dnd
             (context (g-object gdk-drag-context))
             (time :uint32)
             (x-root :short)
             (y-root :short))
            ((:window-state) gdk-event-window-state
             (changed-mask gdk-window-state)
             (new-window-state gdk-window-state))
            ((:setting) gdk-event-setting
             (action gdk-setting-action)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))
            ((:grab-broken) gdk-event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g-object gdk-window)))))
  @end{pre}
  The following fields are common to all event structures.
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  (type gdk-event-type)
  (window (g-object gdk-window))
  (send-event (:boolean :int8))
  ... )
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window of type @class{gdk-window} which received the
      event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
  @end{table}
  @see-constructor{make-gdk-event}
  @see-constructor{copy-gdk-event}
  @see-slot{gdk-event-type}
  @see-slot{gdk-event-window}
  @see-slot{gdk-event-send-event}
  @see-class{gdk-window}
  @see-symbol{gdk-event-type}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEvent structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event 'function)
 "@version{2014-1-22}
  @argument[instance]{a @class{gdk-event} structure}
  Copy constructor of a @class{gdk-event} structure.
  @see-class{gdk-event}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event 'function)
 "@version{2014-1-22}
  Creates a @class{gdk-event} structure.
  @see-class{gdk-event}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEvent structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-type 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event} structure.
  @see-class{gdk-event}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-window 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event} structure.
  @see-class{gdk-event}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-send-event atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-send-event 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event} structure.
  @see-class{gdk-event}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventAny
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkEventKey
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-key 'type)
 "@version{2014-1-22}
  @short{Describes a key press or key release event.}
  Possible event types are @code{:key-press} or @code{:key-release}.
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ((:key-press :key-release) gdk-event-key
             (time :uint32)
             (state gdk-modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[time]{The time of the event in milliseconds.}
    @entry[state]{The state of type @symbol{gdk-modifier-type} of the modifier
      keys, e. g. Control, Shift and Alt, and the pointer buttons.}
    @entry[keyval]{The key that was pressed or released. See the
      <gdk/gdkkeysyms.h> header file for a complete list of GDK key codes.}
    @entry[length]{The length of @code{string}.}
    @entry[string]{A string containing an approximation of the text that would
      result from this keypress. The only correct way to handle text input of
      text is using input methods, see @class{gtk-im-context}, so this field is
      deprecated and should never be used. The function
      @fun{gdk-unicode-to-keyval} provides a non-deprecated way of getting an
      approximate translation for a key. The @code{string} is encoded in the
      encoding of the current locale. Note this for backwards compatibility:
      strings in GTK+ and GDK are typically in UTF-8 and @code{NUL}-terminated.
      In some cases, the translation of the key code will be a single @code{NUL}
      byte, in which case looking at length is necessary to distinguish it from
      an empty translation.}
    @entry[hardware-keycode]{The raw code of the key that was pressed or
      released.}
    @entry[group]{The keyboard group.}
    @entry[is-modifier]{A flag that indicates if @code{hardware-keycode} is
      mapped to a modifier. Since 2.10}
  @end{table}
  @see-constructor{copy-gdk-event-key}
  @see-constructor{make-gdk-event-key}
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
;;;
;;; Constructors for the GdkEventKey structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-key 'function)
 "@version{2014-1-22}
  @argument[instance]{a @class{gdk-event-key} structure}
  Copy constructor of a @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-key 'function)
 "@version{2014-1-22}
  Creates a @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventKey structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-type 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-window 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-send-event atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-send-event 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-key}
  structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-time 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"time\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-state atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-state 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-keyval atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-keyval 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"keyval\"} of the @class{gdk-event-key} structure.
  @see-slot{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-length atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-length 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"length\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-string atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-string 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"string\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-hardware-keycode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-key-hardware-keycode 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"keycode\"} of the @class{gdk-event-key}
  structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-group atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-key-group 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"group\"} of the @class{gdk-event-key} structure.
  @see-class{gdk-event-key}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-key-is-modifier atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-key-is-modifier 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"is-modifier\"} of the @class{gdk-event-key}
  structure.
  @see-class{gdk-event-key}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventButton
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-button 'type)
 "@version{2014-1-22}
  @short{Used for button press and button release events.}
  The type field will be one of @code{:button-press}, @code{:2button-press},
  @code{3button-press}, @code{:triple-button-press}, or @code{:button-release}.

  Double and triple-clicks result in a sequence of events being received. For
  double-clicks the order of events will be:
  @begin{pre}
 :button-press
 :button-release
 :button-press
 :2button-press
 :button-release
  @end{pre}
  Note that the first click is received just like a normal button press, while
  the second click results in a @code{:2button-press} being received just after
  the @code{:button-press}.

  Triple-clicks are very similar to double-clicks, except that
  @code{:3button-press} is inserted after the third click. The order of the
  events is:
  @begin{pre}
 :button-press
 :button-release
 :button-press
 :2button-press
 :button-release
 :button-press
 :3button-press
 :button-release
  @end{pre}
  For a double click to occur, the second button press must occur within 1/4
  of a second of the first. For a triple click to occur, the third button
  press must also occur within 1/2 second of the first button press.
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:button-press
              :2button-press
              :double-button-press
              :3button-press
              :triple-button-press
              :button-release) gdk-event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (button :uint)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[time]{The time of the event in milliseconds.}
    @entry[x]{The x coordinate of the pointer relative to the window.}
    @entry[y]{The y coordinate of the pointer relative to the window.}
    @entry[axes]{@code{x}, @code{y} translated to the axes of device, or
      @code{nil} if device is the mouse.}
    @entry[state]{The state of type @symbol{gdk-modifier-type} of the modifier
      keys, e. g. Control, Shift and Alt, and the pointer buttons.}
    @entry[button]{The button which was pressed or released, numbered from 1 to
      5. Normally button 1 is the left mouse button, 2 is the middle button,
      and 3 is the right button. On 2-button mice, the middle button can often
      be simulated by pressing both mouse buttons together.}
    @entry[device]{The device of type @class{gdk-device} where the event
      originated.}
    @entry[x-root]{The x coordinate of the pointer relative to the root of the
      screen.}
    @entry[y-root]{The y coordinate of the pointer relative to the root of the
      screen.}
  @end{table}
  @see-constructor{copy-gdk-event-button}
  @see-constructor{make-gdk-event-button}
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
  @see-slot{gdk-event-button-y-root}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventButton structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-button 'function)
 "@version{2014-1-22}
  @argument[instance]{a @class{gdk-event-button} structure}
  Copy constructor of a @class{gdk-event-button} structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-button 'function)
 "@version{2014-1-22}
  Creates a @class{gdk-event-button} structure.
  @see-class{gdk-event-button}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventButton structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-type 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-window 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-button-send-event 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-time 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"time\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-x 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"x\"} of the @class{gdk-event-button} structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-y 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"y\"} of the @class{gdk-event-button} structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-axes atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-axes 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"axes\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-state atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-state 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-button atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-button 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"button\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-device atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-device 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"device\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-x-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-x-root 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"x-root\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-button-y-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-button-y-root 'function)
 "@version{2014-1-22}
  Accessor of the slot @code{\"y-root\"} of the @class{gdk-event-button}
  structure.
  @see-class{gdk-event-button}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventTouch
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-touch 'type)
 "@version{2014-1-31}
  @begin{short}
    Used for touch events. The type field will be one of @code{:touch-begin},
    @code{:touch-update}, @code{:touch-end} or @code{:touch-cancel}.
  @end{short}

  Touch events are grouped into sequences by means of the sequence field,
  which can also be obtained with the function
  @fun{gdk-event-get-event-sequence}. Each sequence begins with a
  @code{:touch-begin}, followed by any number of @code{:touch-update}, and ends
  with a @code{:touch-end} or @code{:touch-cancel} event. With multitouch
  devices, there may be several active sequences at the same time.
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:touch-begin
              :touch-update
              :touch-end
              :touch-cancel) gdk-event-touch
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (sequence (g-boxed-foreign gdk-event-sequence))
             (emulating-pointer :boolean)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event,
      one of the values @code{:touch-begin}, @code{:touch-update},
      @code{:touch-end}, @code{:touch-cancel}.}
    @entry[window]{The window which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[x]{The x coordinate of the pointer relative to the window.}
    @entry[y]{The y coordinate of the pointer relative to the window.}
    @entry[axes]{@code{x}, @code{y} translated to the axes of device,
      or @code{nil} if device is the mouse.}
    @entry[state]{A bit-mask representing the state of the modifier keys,
      e. g. Control, Shift and Alt and the pointer buttons. See
      @symbol{gdk-modifier-type}.}
    @entry[sequence]{The event sequence of type @symbol{gdk-event-sequence}
      that the event belongs to.}
    @entry[emulating-pointer]{Whether the event should be used for emulating
      pointer event.}
    @entry[device]{The device of type @class{gdk-device} where the event
      originated.}
    @entry[x-root]{The x coordinate of the pointer relative to the root of the
      screen.}
    @entry[y-root]{The y coordinate of the pointer relative to the root of the
      screen.}
  @end{table}
  @see-slot{gdk-event-touch-type}
  @see-slot{gdk-event-touch-window}
  @see-slot{gdk-event-touch-send-event}
  @see-slot{gdk-event-touch-time}
  @see-slot{gdk-event-touch-x}
  @see-slot{gdk-event-touch-y}
  @see-slot{gdk-event-touch-axes}
  @see-slot{gdk-event-touch-state}
  @see-slot{gdk-event-touch-sequence}
  @see-slot{gdk-event-touch-emulating-pointer}
  @see-slot{gdk-event-touch-device}
  @see-slot{gdk-event-touch-x-root}
  @see-slot{gdk-event-touch-y-root}
  @see-function{gdk-event-get-event-sequence}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventTouch structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-touch 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-touch} structure}
  Copy constructor of a @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-touch 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventTouch structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-touch-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-time 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"time\"} of the @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-x 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x\"} of the @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-y 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y\"} of the @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-axes atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-axes 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"axes\"} of the @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-state atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-state 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-sequence atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-sequence 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"sequence\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-emulating-pointer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-touch-emulating-pointer 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"emulating-pointer\"} of the
  @class{gdk-event-touch} structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-device atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-device 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"device\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-x-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-x-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x-root\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-touch-y-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-touch-y-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y-root\"} of the @class{gdk-event-touch}
  structure.
  @see-class{gdk-event-touch}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventScroll
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-scroll 'type)
 "@version{2014-1-31}
  @begin{short}
    Generated from button presses for the buttons 4 to 7. Wheel mice are usually
    configured to generate button press events for buttons 4 and 5 when the
    wheel is turned.
  @end{short}

  Some GDK backends can also generate 'smooth' scroll events, which can be
  recognized by the @code{:smooth} direction. For these, the scroll deltas can
  be obtained with the function @fun{gdk-event-get-scroll-deltas}.
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:scroll) gdk-event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state gdk-modifier-type)
             (direction gdk-scroll-direction)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double)
             (delta-x :double)
             (delta-y :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the scroll event.}
    @entry[window]{The window which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[x]{The x coordinate of the pointer relative to the window.}
    @entry[y]{The y coordinate of the pointer relative to the window.}
    @entry[state]{A bit-mask representing the state of the modifier keys,
      e. g. Control, Shift and Alt and the pointer buttons. See
      @symbol{gdk-modifier-type}.}
    @entry[direction]{The direction of type @symbol{gdk-scroll-direction} to
      scroll to, one of @code{:up}, @code{:down}, @code{:left}, @code{:right}
      or @code{:smooth}.}
    @entry[device]{The device of type @class{gdk-device} where the event
      originated.}
    @entry[x-root]{The x coordinate of the pointer relative to the root
      of the screen.}
    @entry[y-root]{The y coordinate of the pointer relative to the root
      of the screen.}
    @entry[delta-x]{}
    @entry[delta-y]{}
  @end{table}
  @see-constructor{copy-gdk-event-scroll}
  @see-constructor{make-gdk-event-scroll}
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
  @see-function{gdk-event-get-scroll-deltas}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventScroll structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-scroll 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-scroll} structure}
  Copy constructor of a @class{gdk-event-scroll} structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-scroll 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-scroll} structure.
  @see-class{gdk-event-scroll}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventScroll structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-scroll-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-time 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"time\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-x 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x\"} of the @class{gdk-event-scroll} structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-y 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y\"} of the @class{gdk-event-scroll} structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-state atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-state 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-direction atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-direction 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"direction\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-device atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-device 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"device\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-x-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-x-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x-root\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-y-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-y-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y-root\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-delta-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-delta-x 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"delta-x\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-scroll-delta-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-scroll-delta-y 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"delta-y\"} of the @class{gdk-event-scroll}
  structure.
  @see-class{gdk-event-scroll}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventMotion
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-motion 'type)
 "@version{2014-1-31}
  @begin{short}
    Generated when the pointer moves.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:motion-notify) gdk-event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state gdk-modifier-type)
             (is-hint :int16)
             (device (g-object gdk-device))
             (x-root :double)
             (y-root :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[x]{The x coordinate of the pointer relative to the window.}
    @entry[y]{The y coordinate of the pointer relative to the window.}
    @entry[axes]{@code{x}, @code{y} translated to the axes of device, or
      @code{nil} if device is the mouse.}
    @entry[state]{A bit-mask representing the state of the modifier keys,
      e. g. Control, Shift and Alt and the pointer buttons. See
      @symbol{gdk-modifier-type}.}
    @entry[is-hint]{Set to 1 if this event is just a hint, see the
      @code{:pointer-motion-hint-mask} value of @symbol{gdk-event-mask}.}
    @entry[device]{The device where the event originated.}
    @entry[x-root]{The x coordinate of the pointer relative to the root
      of the screen.}
    @entry[y-root]{The y coordinate of the pointer relative to the root
      of the screen.}
  @end{table}
  @see-constructor{copy-gdk-event-motion}
  @see-constructor{make-gdk-event-motion}
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

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventMotion structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-motion 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-motion} structure}
  Copy constructor of a @class{gdk-event-motion} structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-motion 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-motion} structure.
  @see-class{gdk-event-motion}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventMotion structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"motion\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-motion-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-time 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"time\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-x 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x\"} of the @class{gdk-event-motion} structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-y 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y\"} of the @class{gdk-event-motion} structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-axes atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-axes 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"axes\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-state atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-state 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-is-hint atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-is-hint 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"is-hint\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-device atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-device 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"device\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-x-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-x-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x-root\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-motion-y-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-motion-y-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y-root\"} of the @class{gdk-event-motion}
  structure.
  @see-class{gdk-event-motion}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventExpose
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-expose 'type)
 "@version{2014-1-31}
  @begin{short}
    Generated when all or part of a window becomes visible and needs to be
    redrawn.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:expose) gdk-event-expose
             (area gdk-rectangle :inline t)
             (region (:pointer (:struct cairo-region-t)))
             (count :int))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[area]{Bounding box of type @symbol{gdk-rectangle} of region.}
    @entry[region]{The region of type @symbol{cairo-region-t} that needs to be
      redrawn.}
    @entry[count]{The number of contiguous expoxse events following this one.
      The only use for this is \"exposure compression\", i. e. handling all
      contiguous expose events in one go, though GDK performs some exposure
      compression so this is not normally needed.}
  @end{table}
  @see-constructor{copy-gdk-event-expose}
  @see-constructor{make-gdk-event-expose}
  @see-slot{gdk-event-expose-type}
  @see-slot{gdk-event-expose-window}
  @see-slot{gdk-event-expose-send-event}
  @see-slot{gdk-event-expose-area}
  @see-slot{gdk-event-expose-region}
  @see-slot{gdk-event-expose-count}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventExpose structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-expose 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-expose} structure}
  Copy constructor of a @class{gdk-event-expose} structure.
  @see-class{gdk-event-expose}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-expose 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-expose} structure.
  @see-class{gdk-event-expose}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventExpose structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-expose-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-expose-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-expose}
  structure.
  @see-class{gdk-event-expose}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-expose-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-expose-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-expose}
  structure.
  @see-class{gdk-event-expose}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-expose-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-expose-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-expose}
  structure.
  @see-class{gdk-event-expose}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-expose-area atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-expose-area 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"area\"} of the @class{gdk-event-expose}
  structure.
  @see-class{gdk-event-expose}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-expose-region atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-expose-region 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"region\"} of the @class{gdk-event-expose}
  structure.
  @see-class{gdk-event-expose}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-expose-count atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-expose-count 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"count\"} of the @class{gdk-event-expose}
  structure.
  @see-class{gdk-event-expose}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventVisibility
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-visibility 'type)
 "@version{2013-1-16}
  @short{Generated when the window visibility status has changed.}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:visibility-notify) gdk-event-visibility
             (state gdk-visibility-state))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[state]{The new visibility state of type
     @symbol{gdk-visibility-state}, posible values are @code{:fully-obscured},
     @code{:partial} or @code{:unobscured}.}
  @end{table}
  @see-constructor{copy-gdk-event-visibility}
  @see-constructor{make-gdk-event-visibility}
  @see-slot{gdk-event-visibility-type}
  @see-slot{gdk-event-visibility-window}
  @see-slot{gdk-event-visibility-send-event}
  @see-slot{gdk-event-visibility-state}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventVisibility structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-visibility 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-visibility} structure}
  Copy constructor of a @class{gdk-event-visibility} structure.
  @see-class{gdk-event-visibility}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-visibility 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-visibility} structure.
  @see-class{gdk-event-visibility}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventVisibility structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-visibility-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-visibility-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-visibility}
  structure.
  @see-class{gdk-event-visibility}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-visibility-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-visibility-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-visibility}
  structure.
  @see-class{gdk-event-visibility}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-visibility-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-visibility-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-visibility}
  structure.
  @see-class{gdk-event-visibility}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-visibility-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-visibility-state 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-visibility}
  structure.
  @see-class{gdk-event-visibility}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventCrossing
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-crossing 'type)
 "@version{2014-1-31}
  @short{Generated when the pointer enters or leaves a window.}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:enter-notify :leave-notify) gdk-event-crossing
             (subwindow (g-object gdk-window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode gdk-crossing-mode)
             (detail gdk-notify-type)
             (focus :boolean)
             (state :uint))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of the event of type @symbol{gdk-event-type}).}
    @entry[window]{The window of type @class{gdk-window} which received the
      event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[subwindow]{The window of type @class{gdk-window} that was entered or
      left.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[x]{The x coordinate of the pointer relative to the window.}
    @entry[y]{The y coordinate of the pointer relative to the window.}
    @entry[x-root]{The x coordinate of the pointer relative to the root
      of the screen.}
    @entry[y-root]{The y coordinate of the pointer relative to the root
      of the screen.}
    @entry[mode]{The crossing mode of type @symbol{gdk-crossing-mode},
     values are @code{:normal}, @code{:grab}, @code{:ungrab}, @code{:gtk-grab},
     @code{:gtk-ungrab}, or @code{:state-changed}.
     @code{:gtk-grab}, @code{:gtk-ungrab}, and @code{:state-changed} were added
     in 2.14 and are always synthesized, never native.}
    @entry[detail]{The kind of crossing of type @symbol{gdk-notify-type} that
      happened, values are @code{:inferior}, @code{ancestor}, @code{:virtual},
      @code{:non-linear}, or @code{:nonlinear-virtual}.}
    @entry[focus]{@em{True} if window is the focus window or an inferior.}
    @entry[state]{A bit-mask representing the state of the modifier keys,
      e. g. Control, Shift and Alt) and the pointer buttons.
      See @symbol{gdk-modifier-type}.}
  @end{table}
  @see-constructor{copy-gdk-event-crossing}
  @see-constructor{make-gdk-event-crossing}
  @see-slot{gdk-event-crossing-type}
  @see-slot{gdk-event-crossing-window}
  @see-slot{gdk-event-crossing-send-event}
  @see-slot{gdk-event-crossing-subwindow}
  @see-slot{gdk-event-crossing-time}
  @see-slot{gdk-event-crossing-x}
  @see-slot{gdk-event-crossing-y}
  @see-slot{gdk-event-crossing-x-root}
  @see-slot{gdk-event-crossing-y-root}
  @see-slot{gdk-event-crossing-mode}
  @see-slot{gdk-event-crossing-detail}
  @see-slot{gdk-event-crossing-focus}
  @see-slot{gdk-event-crossing-state}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventCrossing structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-crossing 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-crossing} structure}
  Copy constructor of a @class{gdk-event-crossing} structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-crossing 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-crossing} structure.
  @see-class{gdk-event-crossing}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventCrossing structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-crossing-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-crossing-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-subwindow atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-crossing-subwindow 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{subwindow} of the @class{gdk-event-crossing}
  structure
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-time 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"time\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-x 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x\"} of the @class{gdk-event-crossing} structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-y 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y\"} of the @class{gdk-event-crossing} structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-x-root atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-crossing-x-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"x-root\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-y-root atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-crossing-y-root 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"y-root\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-mode atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-mode 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"mode\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-detail atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-crossing-detail 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"detail\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-focus atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-focus 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"focus\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-crossing-state atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-crossing-state 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"state\"} of the @class{gdk-event-crossing}
  structure.
  @see-class{gdk-event-crossing}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventFocus
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-focus 'type)
 "@version{2014-1-31}
  @short{Describes a change of keyboard focus.}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:focus-change) gdk-event-focus
             (in :int16))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window of type @class{gdk-window} which received the
      event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[in]{@em{True} if the window has gained the keyboard focus, @code{nil}
      if it has lost the focus.}
  @end{table}
  @see-constructor{copy-gdk-event-focus}
  @see-constructor{make-gdk-event-focus}
  @see-slot{gdk-event-focus-type}
  @see-slot{gdk-event-focus-window}
  @see-slot{gdk-event-focus-send-event}
  @see-slot{gdk-event-focus-in}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventFocus structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-focus 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-focus} structure}
  Copy constructor of a @class{gdk-event-focus} structure.
  @see-class{gdk-event-focus}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-focus 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-focus} structure.
  @see-class{gdk-event-focus}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventFocus structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-focus-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-focus-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"type\"} of the @class{gdk-event-focus} structure.
  @see-class{gdk-event-focus}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-focus-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-focus-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"window\"} of the @class{gdk-event-focus}
  structure.
  @see-class{gdk-event-focus}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-focus-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-focus-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"send-event\"} of the @class{gdk-event-focus}
  structure.
  @see-class{gdk-event-focus}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-focus-in atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-focus-in 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{\"in\"} of the @class{gdk-event-focus} structure.
  @see-class{gdk-event-focus}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventConfigure
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-configure 'type)
 "@version{2014-1-31}
  @begin{short}
    Generated when a window size or position has changed.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:configure) gdk-event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[x]{The new x coordinate of the window, relative to its parent.}
    @entry[y]{The new y coordinate of the window, relative to its parent.}
    @entry[width]{The new width of the window.}
    @entry[height]{The new height of the window.}
  @end{table}
  @see-constructor{copy-gdk-event-configure}
  @see-constructor{make-gdk-event-configure}
  @see-slot{gdk-event-configure-type}
  @see-slot{gdk-event-configure-window}
  @see-slot{gdk-event-configure-send-event}
  @see-slot{gdk-event-configure-x}
  @see-slot{gdk-event-configure-y}
  @see-slot{gdk-event-configure-width}
  @see-slot{gdk-event-configure-height}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventConfigure structure
;;;
;;; ----------------------------------------------------------------------------


#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-configure 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-configure} structure}
  Copy constructor of a @class{gdk-event-configure} structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-configure 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-configure} structure.
  @see-class{gdk-event-configure}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventConfigure structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-configure-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{type} of the @class{gdk-event-configure} structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-configure-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{window} of the @class{gdk-event-configure}
  structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-configure-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{send-event} of the @class{gdk-event-configure}
  structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-configure-x 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{x} of the @class{gdk-event-configure} structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-configure-y 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{y} of the @class{gdk-event-configure} structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-configure-width 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{width} of the @class{gdk-event-configure}
  structure.
  @see-class{gdk-event-configure}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-configure-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-configure-height 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{height} of the @class{gdk-event-configure}
  structure.
  @see-class{gdk-event-configure}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventProperty
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-property 'type)
 "@version{2014-1-31}
  @begin{short}
    Describes a property change on a window.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct gdk-event \"GdkEvent\"
  ...
  (:variant type
            ...
            ((:property-notify) gdk-event-property
             (atom gdk-atom)
             (time :uint32)
             (state gdk-property-state))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The type of type @symbol{gdk-event-type} of the event.}
    @entry[window]{The window of type @class{gdk-window} which received the
      event.}
    @entry[send-event]{@em{True} if the event was sent explicitly, e. g. using
      @code{XSendEvent}.}
    @entry[atom]{The property that was changed.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[state]{Whether the property was changed @code{:new-value} or
      deleted @code{:delete}.}
  @end{table}
  @see-constructor{copy-gdk-event-property}
  @see-constructor{make-gdk-event-property}
  @see-slot{gdk-event-property-type}
  @see-slot{gdk-event-property-window}
  @see-slot{gdk-event-property-send-event}
  @see-slot{gdk-event-property-atom}
  @see-slot{gdk-event-property-time}
  @see-slot{gdk-event-property-state}")

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkEventProperty structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-property 'function)
 "@version{2014-1-31}
  @argument[instance]{a @class{gdk-event-property} structure}
  Copy constructor of a @class{gdk-event-property} structure.
  @see-class{gdk-event-property}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-property 'function)
 "@version{2014-1-31}
  Creates a @class{gdk-event-property} structure.
  @see-class{gdk-event-property}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkEventProperty structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-property-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-property-type 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{type} of the @class{gdk-event-property} structure.
  @see-class{gdk-event-property}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-property-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-property-window 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{window} of the @class{gdk-event-property}
  structure.
  @see-class{gdk-event-property}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-property-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-property-send-event 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{send-event} of the @class{gdk-event-property}
  structure.
  @see-class{gdk-event-property}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-property-atom atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-property-atom 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{atom} of the @class{gdk-event-property} structure.
  @see-class{gdk-event-property}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-property-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-property-time 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{time} of the @class{gdk-event-property} structure.
  @see-class{gdk-event-property}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-property-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-property-state 'function)
 "@version{2014-1-31}
  Accessor of the slot @code{state} of the @class{gdk-event-property} structure.
  @see-class{gdk-event-property}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventSelection
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-selection 'type)
 "@version{2013-2-2}
  @begin{short}
    Generated when a selection is requested or ownership of a selection is taken
    over by another client application.
  @end{short}
  @begin{pre}
 struct GdkEventSelection {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkAtom selection;
   GdkAtom target;
   GdkAtom property;
   guint32 time;
   GdkWindow *requestor;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_SELECTION_CLEAR,
      GDK_SELECTION_NOTIFY or GDK_SELECTION_REQUEST).}
    @entry[GdkWindow *window]{the window which received the event.}
    @enty[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[GdkAtom selection]{the selection.}
    @entry[GdkAtom target]{the target to which the selection should be
      converted.}
    @entry[GdkAtom property]{the property in which to place the result of the
      conversion.}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[GdkWindow *requestor]{the window on which to place property or NULL
      if none.}
  @end{table}
  @see-constructor{copy-gdk-event-selection}
  @see-constructor{make-gdk-event-selection}
  @see-slot{gdk-event-selection-type}
  @see-slot{gdk-event-selection-window}
  @see-slot{gdk-event-selection-send-event}
  @see-slot{gdk-event-selection-selection}
  @see-slot{gdk-event-selection-target}
  @see-slot{gdk-event-selection-property}
  @see-slot{gdk-event-selection-time}
  @see-slot{gdk-event-selection-requestor}")

;;; --- copy-gdk-event-selection -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-selection 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-selection} structure}
  Copy constructor of a @class{gdk-event-selection} structure.")

;;; --- make-gdk-event-selection -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-selection 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-selection} structure.")

;;; --- gdk-event-selection-type -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-type 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-selection} struct.
  @end{short}")

;;; --- gdk-event-selection-window ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-window 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-selection}
    struct.
  @end{short}")

;;; --- gdk-event-selection-send-event -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-send-event 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-selection}
    struct.
  @end{short}")

;;; --- gdk-event-selection-selection ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-selection 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{selection} of the @class{gdk-event-selection}
    struct.
  @end{short}")

;;; --- gdk-event-selection-target ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-target atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-target 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{target} of the @class{gdk-event-selection}
    struct.
  @end{short}")

;;; --- gdk-event-selection-property -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-property atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-property 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{property} of the @class{gdk-event-selection}
    struct.
  @end{short}")

;;; --- gdk-event-selection-time -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-time 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{time} of the @class{gdk-event-selection} struct.
  @end{short}")

;;; --- gdk-event-selection-requestor ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-selection-requestor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-selection-requestor 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{requestor} of the @class{gdk-event-selection}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventDND
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-dnd 'type)
 "@version{2013-2-25}
  @begin{short}
    Generated during DND operations.
  @end{short}
  @begin{pre}
 struct GdkEventDND {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkDragContext *context;
   guint32 time;
   gshort x_root, y_root;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (@code{:drag-enter},
      @code{:drag-leave}, @code{:drag-motion}, @code{:drag-status},
      @code{:drop-start} or @code{:drop-finished}.}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{@em{true} if the event was sent explicitly (e.g.
      using @code{XSendEvent}).}
    @entry[GdkDragContext *context]{the @class{gdk-drag-context} for the current
      DND operation.}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[gshort x_root]{the x coordinate of the pointer relative to the root
      of the screen, only set for @code{:drag-motion} and @code{:drop-start}.}
    @entry[gshort y_root]{the y coordinate of the pointer relative to the root
      of the screen, only set for @code{:drag-motion} and @code{:drop-start}.}
  @end{table}
  @see-constructor{copy-gdk-event-dnd}
  @see-constructor{make-gdk-event-dnd}
  @see-slot{gdk-event-dnd-type}
  @see-slot{gdk-event-dnd-window}
  @see-slot{gdk-event-dnd-send-event}
  @see-slot{gdk-event-dnd-context}
  @see-slot{gdk-event-dnd-time}
  @see-slot{gdk-event-dnd-x-root}
  @see-slot{gdk-event-dnd-y-root}")

;;; --- copy-gdk-event-dnd -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-dnd 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-dnd} structure}
  Copy constructor of a @class{gdk-event-dnd} structure.")

;;; --- make-gdk-event-dnd -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-dnd 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-dnd} structure.")

;;; --- gdk-event-dnd-type -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-type 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; --- gdk-event-dnd-window ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-window atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-window 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; --- gdk-event-dnd-send-event -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-send-event atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-send-event 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; --- gdk-event-dnd-context --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-context atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-context 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{context} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; --- gdk-event-dnd-time -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-time atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-time 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{time} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; --- gdk-event-dnd-x-root ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-x-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-x-root 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{x-root} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; --- gdk-event-dnd-y-root ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-dnd-y-root atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-event-dnd-y-root 'function)
 "@version{2013-2-2}
  @begin{short}
    Accessor of the slot @code{y-root} of the @class{gdk-event-dnd} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventProximity
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-proximity 'type)
 "@version{2013-2-25}
  @begin{short}
    Proximity events are generated when using GDK's wrapper for the XInput
    extension. The XInput extension is an add-on for standard X that allows you
    to use nonstandard devices such as graphics tablets. A proximity event
    indicates that the stylus has moved in or out of contact with the tablet, or
    perhaps that the user's finger has moved in or out of contact with a touch
    screen.
  @end{short}

  This event type will be used pretty rarely. It only is important for XInput
  aware programs that are drawing their own cursor.
  @begin{pre}
 struct GdkEventProximity {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   guint32 time;
   GdkDevice *device;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_PROXIMITY_IN or
      GDK_PROXIMITY_OUT).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[guint32 time]{the time of the event in milliseconds.}
    @entry[GdkDevice *device]{the device where the event originated.}
  @end{table}
  @see-constructor{copy-gdk-event-proximity}
  @see-constructor{make-gdk-event-proximity}
  @see-slot{gdk-event-proximity-type}
  @see-slot{gdk-event-proximity-window}
  @see-slot{gdk-event-proximity-send-event}
  @see-slot{gdk-event-proximity-time}
  @see-slot{gdk-event-proximity-device}")

;;; --- copy-gdk-event ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-proximity 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-proximity} structure}
  Copy constructor of a @class{gdk-event-proximity} structure.")

;;; --- make-gdk-event-proximity -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-proximity 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-proximity} structure.")

;;; --- gdk-event-proximity-type -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-proximity-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-proximity-type 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-proximity} struct.
  @end{short}")

;;; --- gdk-event-proximity-window ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-proximity-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-proximity-window 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-proximity}
    struct.
  @end{short}")

;;; --- gdk-event-proximity-send-event -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-proximity-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-proximity-send-event 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-proximity}
    struct.
  @end{short}")

;;; --- gdk-event-proximity-time -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-proximity-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-proximity-time 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{time} of the @class{gdk-event-proximity} struct.
  @end{short}")

;;; --- gdk-event-proximity-device ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-proximity-device atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-proximity-device 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{device} of the @class{gdk-event-proximity}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventWindowState
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-window-state 'type)
 "@version{2013-2-25}
  @begin{short}
    Generated when the state of a toplevel window changes.
  @end{short}
  @begin{pre}
 struct GdkEventWindowState {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkWindowState changed_mask;
   GdkWindowState new_window_state;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_WINDOW_STATE).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[GdkWindowState changed_mask]{mask specifying what flags have
      changed.}
    @entry[GdkWindowState new_window_state]{the new window state, a combination
      of GdkWindowState bits.}
  @end{table}
  @see-constructor{copy-gdk-event-window-state}
  @see-constructor{make-gdk-event-window-state}
  @see-slot{gdk-event-window-state-type}
  @see-slot{gdk-event-window-state-window}
  @see-slot{gdk-event-window-state-send-event}
  @see-slot{gdk-event-window-state-changed-mask}
  @see-slot{gdk-event-window-state-new-window-state}")

;;; --- copy-gdk-event-window-state --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-window-state 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-window-state} structure}
  Copy constructor of a @class{gdk-event-window-state} structure.")

;;; --- make-gdk-event-window-state --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-window-state 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-window-state} structure.")

;;; --- gdk-event-window-state-type --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-window-state-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-window-state-type 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-window-state}
    struct.
  @end{short}")

;;; --- gdk-event-window-state-window ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-window-state-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-window-state-window 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-window-state}
    struct.
  @end{short}")

;;; --- gdk-event-window-state-send-event --------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-window-state-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-window-state-send-event 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-window-state}
    struct.
  @end{short}")

;;; --- gdk-event-window-state-changed-mask ------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-window-state-changed-mask atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-window-state-changed-mask 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{changed-mask} of the
    @class{gdk-event-window-state} struct.
  @end{short}")

;;; --- gdk-event-window-state-new-window-state --------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-window-state-new-window-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-window-state-new-window-state 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{new-window-state} of the
    @class{gdk-event-window-state} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventSetting
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-setting 'type)
 "@version{2013-2-25}
  @begin{short}
    Generated when a setting is modified.
  @end{short}
  @begin{pre}
 struct GdkEventSetting {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkSettingAction action;
   char *name;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_SETTING).}
    @entry[GdkWindow *window]{the window which received the event.}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent).}
    @entry[GdkSettingAction action]{what happened to the setting
      (GDK_SETTING_ACTION_NEW, GDK_SETTING_ACTION_CHANGED or
      GDK_SETTING_ACTION_DELETED).}
    @entry[char *name]{the name of the setting.}
  @end{table}
  @see-constructor{copy-gdk-event-setting}
  @see-constructor{make-gdk-event-setting}
  @see-slot{gdk-event-setting-type}
  @see-slot{gdk-event-setting-window}
  @see-slot{gdk-event-setting-send-event}
  @see-slot{gdk-event-setting-action}
  @see-slot{gdk-event-setting-name}")

;;; --- copy-gdk-event-setting -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-setting 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-setting} structure}
  Copy constructor of a @class{gdk-event-setting} structure.")

;;; --- make-gdk-event ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-setting 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-setting} structure.")

;;; --- gdk-event-setting-type -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-setting-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-setting-type 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-setting}
    struct.
  @end{short}")

;;; --- gdk-event-setting-window -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-setting-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-setting-window 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-setting}
    struct.
  @end{short}")

;;; --- gdk-event-setting-send-event -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-setting-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-setting-send-event 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-setting}
    struct.
  @end{short}")

;;; --- gdk-event-setting-action -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-setting-action atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-setting-action 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{action} of the @class{gdk-event-setting}
    struct.
  @end{short}")

;;; --- gdk-event-setting-name -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-setting-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-setting-name 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{name} of the @class{gdk-event-setting}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventOwnerChange
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-event-owner-change 'type)
 "@version{2013-2-25}
  @begin{short}
    Generated when the owner of a selection changes. On X11, this information is
    only available if the X server supports the XFIXES extension.
  @end{short}
  @begin{pre}
 struct GdkEventOwnerChange {
   GdkEventType type;
   GdkWindow *window;
   gint8 send_event;
   GdkWindow *owner;
   GdkOwnerChange reason;
   GdkAtom selection;
   guint32 time;
   guint32 selection_time;
 @};
  @end{pre}
  @begin[code]{table}
    @entry[GdkEventType type]{the type of the event (GDK_OWNER_CHANGE).}
    @entry[GdkWindow *window]{the window which received the event}
    @entry[gint8 send_event]{TRUE if the event was sent explicitly (e.g. using
      XSendEvent)}
    @entry[GdkWindow *owner]{the new owner of the selection, or NULL if there#
      is none}
    @entry[GdkOwnerChange reason]{the reason for the ownership change as a
      GdkOwnerChange value}
    @entry[GdkAtom selection]{the atom identifying the selection}
    @entry[guint32 time]{the timestamp of the event}
    @entry[guint32 selection_time]{the time at which the selection ownership
      was taken over}
  @end{table}
  Since 2.6
  @see-constructor{copy-gdk-event-owner-change}
  @see-constructor{make-gdk-event-owner-change}
  @see-slot{gdk-event-owner-change-type}
  @see-slot{gdk-event-owner-change-window}
  @see-slot{gdk-event-owner-change-send-event}
  @see-slot{gdk-event-owner-change-owner}
  @see-slot{gdk-event-owner-change-reason}
  @see-slot{gdk-event-owner-change-selection}
  @see-slot{gdk-event-owner-change-time}
  @see-slot{gdk-event-owner-change-selection-time}")

;;; --- copy-gdk-event-owner-change --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-owner-change 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-owner-change} structure}
  Copy constructor of a @class{gdk-event-owner-change} structure.")

;;; --- make-gdk-event-owner-change --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-owner-change 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-owner-change} structure.")

;;; --- gdk-event-owner-change-type --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-type 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-window ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-window 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-send-event --------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-send-event 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-owner -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-owner atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-owner 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{owner} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-reason ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-reason atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-reason 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{reason} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-selection ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-selection 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{selection} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-time --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-time 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{time} of the @class{gdk-event-owner-change}
    struct.
  @end{short}")

;;; --- gdk-event-owner-change-selection-time ----------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-owner-change-selection-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-owner-change-selection-time 'function)
 "@version{2013-2-25}
  @begin{short}
    Accessor of the slot @code{selection-time} of the
    @class{gdk-event-owner-change} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GdkEventGrabBroken
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
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
  @see-constructor{copy-gdk-event-grab-broken}
  @see-constructor{make-gdk-event-grab-broken}
  @see-slot{gdk-event-grab-broken-type}
  @see-slot{gdk-event-grab-broken-window}
  @see-slot{gdk-event-grab-broken-send-event}
  @see-slot{gdk-event-grab-broken-keyboard}
  @see-slot{gdk-event-grab-broken-implicit}
  @see-slot{gdk-event-grab-broken-grab-window}")

;;; --- copy-gdk-event-grab-broken ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-event-grab-broken 'function)
 "@version{2013-6-15}
  @argument[instance]{a @class{gdk-event-grab-broken} structure}
  Copy constructor of a @class{gdk-event-grab-broken} structure.")

;;; --- make-gdk-event-grab-broken ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-event-grab-broken 'function)
 "@version{2013-6-15}
  Creates a @class{gdk-event-grab-broken} structure.")

;;; --- gdk-event-grab-broken-type ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-grab-broken-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-grab-broken-type 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @code{type} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-window -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-grab-broken-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-grab-broken-window 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @code{window} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-send-event ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-grab-broken-send-event atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-grab-broken-send-event 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @code{send-event} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-keyboard -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-grab-broken-keyboard atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-grab-broken-keyboard 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @code{keyboard} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-implicit -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-grab-broken-implicit atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-grab-broken-implicit 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @code{implicit} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- gdk-event-grab-broken-grab-window --------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-event-grab-broken-grab-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-event-grab-broken-grab-window 'function)
 "@version{2013-1-19}
  @begin{short}
    Accessor of the slot @code{grab-window} of the @class{gdk-event-grab-broken}
    struct.
  @end{short}")

;;; --- End of file gdk.event-structures.lisp ----------------------------------
