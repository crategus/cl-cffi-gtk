;;; ----------------------------------------------------------------------------
;;; gdk.seat.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GdkSeat
;;;
;;;     Object representing an user seat.
;;;
;;; Types and Values
;;;
;;;     GdkSeat
;;;     GdkSeatCapabilities
;;;
;;; Functions
;;;
;;;     GdkSeatGrabPrepareFunc
;;;
;;;     gdk_seat_get_display                               Accessor
;;;     gdk_seat_grab
;;;     gdk_seat_ungrab
;;;     gdk_seat_get_capabilities
;;;     gdk_seat_get_pointer
;;;     gdk_seat_get_keyboard
;;;     gdk_seat_get_slaves
;;;
;;; Properties
;;;
;;;     GdkDisplay* display    Read / Write / Construct Only
;;;
;;; Signals
;;;
;;;     void  device-added      Run Last
;;;     void  device-removed    Run Last
;;;     void  tool-added        Run Last
;;;     void  tool-removed      Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkSeat
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkSeatCapabilities
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkSeatCapabilities" gdk-seat-capabilities
  (:export t
   :type-initializer "gdk_seat_capabilities_get_type")
  (:none 0)
  (:pointer       #.(ash 1 0))
  (:touch         #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard      #.(ash 1 3))
  (:all-pointing 7)          ; :pointer | :touch | :tablet-stylus
  (:all 15)                  ; :all-pointing | :keyboard
)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-seat-capabilities atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-seat-capabilities atdoc:*external-symbols*)
 "@version{2019-3-24}
  @begin{short}
    Flags describing the seat capabilities.
  @end{short}
  @begin{pre}
(define-g-flags \"GdkSeatCapabilities\" gdk-seat-capabilities
  (:export t
   :type-initializer \"gdk_seat_capabilities_get_type\")
  (:none 0)
  (:pointer #.(ash 1 0))
  (:touch #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard #.(ash 1 3))
  (:all-pointing 7)          ; :pointer | :touch | :tablet-stylus
  (:all 15)                  ; :all-pointing | :keyboard
)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No input capabilities.}
    @entry[:pointer]{The seat has a pointer, e. g. mouse.}
    @entry[:touch]{The seat has touchscreen(s) attached.}
    @entry[:tablet-stylus]{The seat has drawing tablet(s) attached.}
    @entry[:keyboard]{The seat has keyboard(s) attached.}
    @entry[:all-pointing]{The union of all pointing capabilities.}
    @entry[:all]{The union of all capabilities.}
  @end{table}
  @see-class{gdk-seat}")

;;; ----------------------------------------------------------------------------
;;; struct GdkSeat
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkSeat" gdk-seat
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_seat_get_type")
  ((display
    gdk-seat-display
    "display" "GdkDisplay" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-seat 'type)
 "@version{2019-3-24}
  @begin{short}
    The @sym{gdk-seat} object represents a collection of input devices that
    belong to a user.
  @end{short}

  Since 3.20
  @begin[Signal Details]{dictionary}
    @subheading{The \"device-added\" signal}
      @begin{pre}
 lambda (seat device)    : Run Last
      @end{pre}
      The \"device-added\" signal is emitted when a new input device is related
      to this seat.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk-seat} object on which the signal is emitted.}
        @entry[device]{The newly added @class{gdk-device} object.}
      @end{table}
    @subheading{The \"device-removed\" signal}
      @begin{pre}
 lambda (seat device)    : Run Last
      @end{pre}
      The \"device-removed\" signal is emitted when an input device is removed,
      e. g. unplugged.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk-seat} object on which the signal is emitted.}
        @entry[device]{The just removed @class{gdk-device} object.}
      @end{table}
    @subheading{The \"tool-added\" signal}
      @begin{pre}
 lambda (seat tool)    : Run Last
      @end{pre}
      The \"tool-added\" signal is emitted whenever a new tool is made known to
      the seat. The tool may later be assigned to a device, i. e. on proximity
      with a tablet. The device will emit the \"tool-changed\" signal
      accordingly. A same tool may be used by several devices.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk-seat} object on which the signal is emitted.}
        @entry[tool]{The new @class{gdk-device-tool} object known to the seat.}
      @end{table}
    @subheading{The \"tool-removed\" signal}
      @begin{pre}
 lambda (seat tool)    : Run Last
      @end{pre}
      This signal is emitted whenever a tool is no longer known to this seat.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk-seat} objeczt on which the signal is emitted.}
        @entry[tool]{The just removed @class{gdk-device-tool} object.}
      @end{table}
      Since 3.22
  @end{dictionary}
  @see-class{gdk-display}
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-seat-display -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display" 'gdk-seat) 't)
 "The @code{display} property of type @class{gdk-display}
  (Read / Write / Construct) @br{}
  The @class{gdk-display} object of this seat.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-seat-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-seat-display 'function)
 "@version{2019-3-24}
  @syntax[]{(gdk-seat-display object) => display}
  @argument[object]{a @class{gdk-seat} object}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Accessor of the @slot[gdk-seat]{display} slot of the
    @class{gdk-seat} class.
  @end{short}

  The @sym{gdk-seat-display} slot access function
  returns the @class{gdk-display} object this seat belongs to.
  @see-class{gdk-seat}")

;;; ----------------------------------------------------------------------------
;;; GdkSeatGrabPrepareFunc ()
;;;
;;; void
;;; (*GdkSeatGrabPrepareFunc) (GdkSeat *seat,
;;;                            GdkWindow *window,
;;;                            gpointer user_data);
;;;
;;; Type of the callback used to set up window so it can be grabbed. A typical
;;; action would be ensuring the window is visible, although there's room for
;;; other initialization actions.
;;;
;;; seat :
;;;     the GdkSeat being grabbed
;;;
;;; window :
;;;     the GdkWindow being grabbed
;;;
;;; user_data :
;;;     user data passed in gdk_seat_grab()
;;;
;;; Since: 3.20
;;; ----------------------------------------------------------------------------

(defcallback gdk-seat-grab-prepare-func-cb :void
    ((seat (g-object gdk-seat))
     (window (g-object gdk-window))
     (data :pointer))
  (funcall (glib:get-stable-pointer-value data) seat window))

;;; ----------------------------------------------------------------------------
;;; gdk_seat_grab ()
;;;
;;; GdkGrabStatus
;;; gdk_seat_grab (GdkSeat *seat,
;;;                GdkWindow *window,
;;;                GdkSeatCapabilities capabilities,
;;;                gboolean owner_events,
;;;                GdkCursor *cursor,
;;;                const GdkEvent *event,
;;;                GdkSeatGrabPrepareFunc prepare_func,
;;;                gpointer prepare_func_data);
;;;
;;; Grabs the seat so that all events corresponding to the given capabilities
;;; are passed to this application until the seat is ungrabbed with
;;; gdk_seat_ungrab(), or the window becomes hidden. This overrides any previous
;;; grab on the seat by this client.
;;;
;;; As a rule of thumb, if a grab is desired over GDK_SEAT_CAPABILITY_POINTER,
;;; all other "pointing" capabilities (eg. GDK_SEAT_CAPABILITY_TOUCH) should be
;;; grabbed too, so the user is able to interact with all of those while the
;;; grab holds, you should thus use GDK_SEAT_CAPABILITY_ALL_POINTING most
;;; commonly.
;;;
;;; Grabs are used for operations which need complete control over the events
;;; corresponding to the given capabilities. For example in GTK+ this is used
;;; for Drag and Drop operations, popup menus and such.
;;;
;;; Note that if the event mask of a GdkWindow has selected both button press
;;; and button release events, or touch begin and touch end, then a press event
;;; will cause an automatic grab until the button is released, equivalent to a
;;; grab on the window with owner_events set to TRUE. This is done because most
;;; applications expect to receive paired press and release events.
;;;
;;; If you set up anything at the time you take the grab that needs to be
;;; cleaned up when the grab ends, you should handle the GdkEventGrabBroken
;;; events that are emitted when the grab ends unvoluntarily.
;;;
;;; seat :
;;;     a GdkSeat
;;;
;;; window :
;;;     the GdkWindow which will own the grab
;;;
;;; capabilities :
;;;     capabilities that will be grabbed
;;;
;;; owner_events :
;;;     if FALSE then all device events are reported with respect to window
;;;     and are only reported if selected by event_mask . If TRUE then pointer
;;;     events for this application are reported as normal, but pointer events
;;;     outside this application are reported with respect to window and only
;;;     if selected by event_mask . In either mode, unreported events are
;;;     discarded.
;;;
;;; cursor :
;;;     the cursor to display while the grab is active. If this is NULL then
;;;     the normal cursors are used for window and its descendants, and the
;;;     cursor for window is used elsewhere.
;;;
;;; event :
;;;     the event that is triggering the grab, or NULL if none is available.
;;;
;;; prepare_func :
;;;     function to prepare the window to be grabbed, it can be NULL if window
;;;     is visible before this call.
;;;
;;; prepare_func_data
;;;     user data to pass to prepare_func
;;;
;;; Returns :
;;;     GDK_GRAB_SUCCESS if the grab was successful.
;;;
;;; Since: 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_seat_ungrab ()
;;;
;;; void gdk_seat_ungrab (GdkSeat *seat);
;;;
;;; Releases a grab added through gdk_seat_grab().
;;;
;;; seat :
;;;     a GdkSeat
;;;
;;; Since: 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_capabilities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_capabilities"
           gdk-seat-get-capabilities) gdk-seat-capabilities
 #+cl-cffi-gtk-documentation
 "@version{2019-3-30}
  @argument[seat]{a @class{gdk-seat} object}
  @return{The seat capabilities of type @symbol{gdk-seat-capabilities}.}
  @begin{short}
    Returns the capabilities this @class{gdk-seat} object currently has.
  @end{short}
  @see-class{gdk-seat}
  @see-symbol{gdk-seat-capabilities}"
  (seat (g-object gdk-seat)))

(export 'gdk-seat-get-capabilities)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_pointer () -> gdk-seat-pointer
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_pointer"
           gdk-seat-pointer) (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-23}
  @argument[seat]{a @class{gdk-seat} object}
  @return{A master @class{gdk-device} object with pointer capabilities.}
  @begin{short}
    Returns the master device that routes pointer events.
  @end{short}
  @see-class{gdk-seat}
  @see-class{gdk-device}"
  (seat (g-object gdk-seat)))

(export 'gdk-seat-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_keyboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_keyboard"
           gdk-seat-get-keyboard) (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-30}
  @argument[seat]{a @class{gdk-seat} object}
  @return{A master @class{gdk-device} object with keyboard capabilities.}
  @begin{short}
    Returns the master device that routes keyboard events.
  @end{short}
  @see-class{gdk-seat}
  @see-class{gdk-device}"
  (seat (g-object gdk-seat)))

(export 'gdk-seat-get-keyboard)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_slaves ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_slaves"
           gdk-seat-get-slaves) (g-list (g-object gdk-device))
 #+cl-cffi-gtk-documentation
 "@version{2019-3-30}
  @argument[seat]{a @class{gdk-seat} object}
  @argument[capabilities]{capabilities of the @symbol{gdk-seat-capabilities}
    flags to get devices for}
  @return{A list of @class{gdk-device} objects.}
  @begin{short}
    Returns the slave devices that match the given capabilities.
  @end{short}
  @see-class{gdk-seat}
  @see-class{gdk-device}"
  (seat (g-object gdk-seat))
  (capabilities gdk-seat-capabilities))

(export 'gdk-seat-get-slaves)

;;; --- End of file gdk.seat.lisp ----------------------------------------------
