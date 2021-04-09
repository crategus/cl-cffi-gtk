;;; ----------------------------------------------------------------------------
;;; gdk.device.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; GdkDevice
;;;
;;;     Object representing an input device
;;;
;;; Types and Values
;;;
;;;     GdkDevice
;;;     GdkDeviceTool                                      Since 3.22
;;;
;;;     GdkInputSource
;;;     GdkInputMode
;;;     GdkAxisUse
;;;     GdkAxisFlags                                       Since 3.22
;;;     GdkDeviceToolType                                  Since 3.22
;;;     GdkDeviceType
;;;     GdkGrabOwnership
;;;     GdkTimeCoord
;;;
;;;     GdkGrabStatus                                      <- gdk.general.lisp
;;;
;;; Functions
;;;
;;;     gdk_device_get_name                                Accessor
;;;     gdk_device_get_vendor_id                           Accessor
;;;     gdk_device_get_product_id                          Accessor
;;;     gdk_device_get_source
;;;     gdk_device_set_mode
;;;     gdk_device_get_mode
;;;     gdk_device_set_key
;;;     gdk_device_get_key
;;;     gdk_device_set_axis_use
;;;     gdk_device_get_axis_use
;;;     gdk_device_get_associated_device                   Accessor
;;;     gdk_device_list_slave_devices
;;;     gdk_device_get_device_type                         Accessor
;;;     gdk_device_get_display                             Accessor
;;;     gdk_device_get_has_cursor                          Accessor
;;;     gdk_device_get_n_axes                              Accessor
;;;     gdk_device_get_n_keys
;;;     gdk_device_get_axes                                Accessor
;;;     gdk_device_warp
;;;     gdk_device_get_seat                                Accessor
;;;     gdk_device_grab
;;;     gdk_device_ungrab
;;;     gdk_device_get_state
;;;     gdk_device_get_position
;;;     gdk_device_get_position_double
;;;     gdk_device_get_window_at_position
;;;     gdk_device_get_window_at_position_double
;;;     gdk_device_get_history
;;;     gdk_device_free_history
;;;     gdk_device_get_axis
;;;     gdk_device_list_axes
;;;     gdk_device_get_axis_value
;;;     gdk_device_get_last_event_window
;;;     gdk_device_tool_get_serial
;;;     gdk_device_tool_get_tool_type
;;;
;;; Properties
;;;
;;;        GdkDevice*   associated-device    Read
;;;     GdkAxisFlags    axes                 Read
;;; GdkDeviceManager*   device-manager       Read / Write / Construct Only
;;;       GdkDisplay*   display              Read / Write / Construct Only
;;;         gboolean    has-cursor           Read / Write / Construct Only
;;;     GdkInputMode    input-mode           Read / Write
;;;   GdkInputSource    input-source         Read / Write / Construct Only
;;;            guint    n-axes               Read
;;;            gchar*   name                 Read / Write / Construct Only
;;;            guint    num-touches          Read / Write / Construct Only
;;;            gchar*   product-id           Read / Write / Construct Only
;;;          GdkSeat*   seat                 Read / Write
;;;    GdkDeviceTool*   tool                 Read
;;;    GdkDeviceType    type                 Read / Write / Construct Only
;;;            gchar*   vendor-id            Read / Write / Construct Only
;;;
;;; Signals
;;;
;;;             void    changed              Run Last
;;;             void    tool-changed         Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDevice
;;;
;;; Known Derived Interfaces
;;;
;;;     GdkDevice is required by GdkDevicePad.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkInputSource
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInputSource" gdk-input-source
  (:export t
   :type-initializer "gdk_input_source_get_type")
  :mouse
  :pen
  :eraser
  :cursor
  :keyboard
  :touchscreen
  :touchpad
  :trackpoint
  :tablet-pad)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-input-source atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-input-source atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    An enumeration describing the type of an input device in general terms.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkInputSource\" gdk-input-source
  (:export t
   :type-initializer \"gdk_input_source_get_type\")
  :mouse
  :pen
  :eraser
  :cursor
  :keyboard
  :touchscreen
  :touchpad
  :trackpoint
  :tablet-pad)
  @end{pre}
  @begin[code]{table}
    @entry[:mouse]{The device is a mouse. This will be reported for the core
      pointer, even if it is something else, such as a trackball.}
    @entry[:pen]{The device is a stylus of a graphics tablet or similar device.}
    @entry[:eraser]{The device is an eraser. Typically, this would be the other
      end of a stylus on a graphics tablet.}
    @entry[:cursor]{The device is a graphics tablet \"puck\" or similar device.}
    @entry[:keyboard]{The device is a keyboard.}
    @entry[:touchscreen]{The device is a direct-input touch device, such as a
      touchscreen or tablet. This device type has been added in 3.4.}
    @entry[:touchpad]{The device is an indirect touch device, such as a
      touchpad. This device type has been added in 3.4.}
    @entry[:trackpoint]{The device is a trackpoint. This device type has been
      added in 3.22.}
    @entry[:tablet-pad]{The device is a \"pad\", a collection of buttons, rings
      and strips found in drawing tablets. This device type has been added in
      3.22.}
  @end{table}
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkInputMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInputMode" gdk-input-mode
  (:export t
   :type-initializer "gdk_input_mode_get_type")
  :disabled
  :screen
  :window)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-input-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-input-mode atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    An enumeration that describes the mode of an input device.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkInputMode\" gdk-input-mode
  (:export t
   :type-initializer \"gdk_input_mode_get_type\")
  (:disabled 0)
  (:screen 1)
  (:window 2))
  @end{pre}
  @begin[code]{table}
    @entry[:disabled]{The device is disabled and will not report any events.}
    @entry[:screen]{The device is enabled. The device's coordinate space maps
      to the entire screen.}
    @entry[:window]{The device is enabled. The device's coordinate space is
      mapped to a single window. The manner in which this window is chosen is
      undefined, but it will typically be the same way in which the focus
      window for key events is determined.}
  @end{table}
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkAxisUse
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkAxisUse" gdk-axis-use
  (:export t
   :type-initializer "gdk_axis_use_get_type")
  :ignore
  :x
  :y
  :pressure
  :xtilt
  :ytilt
  :wheel
  :distance
  :rotation
  :slider
  :last)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-axis-use atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-axis-use atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    An enumeration describing the way in which a device axis (valuator) maps
    onto the predefined valuator types that GTK+ understands.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkAxisUse\" gdk-axis-use
  (:export t
   :type-initializer \"gdk_axis_use_get_type\")
  :ignore
  :x
  :y
  :pressure
  :xtilt
  :ytilt
  :wheel
  :distance
  :rotation
  :slider
  :last)
  @end{pre}
  @begin[code]{table}
    @entry[:ignore]{The axis is ignored.}
    @entry[:x]{The axis is used as the x axis.}
    @entry[:y]{The axis is used as the y axis.}
    @entry[:pressure]{The axis is used for pressure information.}
    @entry[:xtilt]{The axis is used for x tilt information.}
    @entry[:ytilt]{The axis is used for y tilt information.}
    @entry[:wheel]{The axis is used for wheel information.}
    @entry[:distance]{The axis is used for pen/tablet distance information.
      Since 3.22}
    @entry[:rotation]{The axis is used for pen rotation information. Since 3.22}
    @entry[:slider]{The axis is used for pen slider information. Since 3.22}
    @entry[:last]{A constant equal to the numerically highest axis value.}
  @end{table}
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkAxisFlags
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(define-g-flags "GdkAxisFlags" gdk-axis-flags
  (:export t
   :type-initializer "gdk_axis_flags_get_type")
  (:x        #.(ash 1 1))
  (:y        #.(ash 1 2))
  (:pressure #.(ash 1 3))
  (:xtilt    #.(ash 1 4))
  (:ytilt    #.(ash 1 5))
  (:wheel    #.(ash 1 6))
  (:distance #.(ash 1 7))
  (:rotation #.(ash 1 8))
  (:slider   #.(ash 1 9)))

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-axis-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-axis-flags atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    Flags describing the current capabilities of a device/tool.
  @end{short}
  @begin{pre}
(define-g-flags \"GdkAxisFlags\" gdk-axis-flags
  (:export t
   :type-initializer \"gdk_axis_flags_get_type\")
  (:x        #.(ash 1 1))
  (:y        #.(ash 1 2))
  (:pressure #.(ash 1 3))
  (:xtilt    #.(ash 1 4))
  (:ytilt    #.(ash 1 5))
  (:wheel    #.(ash 1 6))
  (:distance #.(ash 1 7))
  (:rotation #.(ash 1 8))
  (:slider   #.(ash 1 9)))
  @end{pre}
  @begin[code]{table}
    @entry[:x]{x axis is present.}
    @entry[:y]{y axis is present.}
    @entry[:pressure]{Pressure axis is present.}
    @entry[:xtilt]{x tilt axis is present.}
    @entry[:ytilt]{y tilt axis is present.}
    @entry[:wheel]{Wheel axis is present.}
    @entry[:distance]{Distance axis is present.}
    @entry[:rotation]{z-axis rotation is present.}
    @entry[:slider]{Slider axis is present.}
  @end{table}
  Since 3.22
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkDeviceToolType
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(define-g-enum "GdkDeviceToolType" gdk-device-tool-type
  (:export t
   :type-initializer "gdk_device_tool_type_get_type")
  :unknown
  :pen
  :eraser
  :brush
  :pencil
  :airbrush
  :mouse
  :lens)

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-device-tool-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-device-tool-type atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    Indicates the specific type of tool being used being a tablet. Such as an
    airbrush, pencil, etc.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkDeviceToolType\" gdk-device-tool-type
  (:export t
   :type-initializer \"gdk_device_tool_type_get_type\")
  :unknown
  :pen
  :eraser
  :brush
  :pencil
  :airbrush
  :mouse
  :lens)
  @end{pre}
  @begin[code]{table}
    @entry[:unkown]{Tool is of an unknown type.}
    @entry[:pen]{Tool is a standard tablet stylus.}
    @entry[:eraser]{Tool is standard tablet eraser.}
    @entry[:brush]{Tool is a brush stylus.}
    @entry[:pencil]{Tool is a pencil stylus.}
    @entry[:airbrush]{Tool is an airbrush stylus.}
    @entry[:mouse]{Tool is a mouse.}
    @entry[:lens]{Tool is a lens cursor.}
  @end{table}
  Since 3.22
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkDeviceType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkDeviceType" gdk-device-type
  (:export t
   :type-initializer "gdk_device_type_get_type")
  (:master 0)
  (:slave 1)
  (:floating 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-device-type atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    Indicates the device type. See above for more information about the meaning
    of these device types.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkDeviceType\" gdk-device-type
  (:export t
   :type-initializer \"gdk_device_type_get_type\")
  (:master 0)
  (:slave 1)
  (:floating 2))
  @end{pre}
  @begin[code]{table}
    @entry[:master]{Device is a master (or virtual) device. There will be an
      associated focus indicator on the screen.}
    @entry[:slave]{Device is a slave (or physical) device.}
    @entry[:floating]{Device is a physical device, currently not attached to
      any virtual device.}
  @end{table}
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabOwnership
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGrabOwnership" gdk-grab-ownership
  (:export t
   :type-initializer "gdk_grab_ownership_get_type")
  (:none 0)
  (:window 1)
  (:application 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-grab-ownership atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-grab-ownership atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    Defines how device grabs interact with other devices.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkGrabOwnership\" gdk-grab-ownership
  (:export t
   :type-initializer \"gdk_grab_ownership_get_type\")
  (:none 0)
  (:window 1)
  (:application 2))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{All other devices' events are allowed.}
    @entry[:window]{Other devices' events are blocked for the grab window.}
    @entry[:application]{Other devices' events are blocked for the whole
      application.}
  @end{table}
  @see-class{gdk-device}")

;;; ----------------------------------------------------------------------------
;;; struct GdkTimeCoord
;;; ----------------------------------------------------------------------------

(defcstruct gdk-time-coord
  (time :uint32)
  (axes :double :count 128))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-time-coord atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gdk-time-coord atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    The @sym{gdk-time-coord} structure stores a single event in a motion
    history.
  @end{short}
  @begin{pre}
(defcstruct gdk-time-coord
  (time :uint32)
  (axes :double :count 128))
  @end{pre}
  @begin[code]{table}
    @entry[time]{The timestamp for this event.}
    @entry[axes]{The values of the device's axes.}
  @end{table}
  @see-class{gdk-device}
  @see-function{gdk-device-history}")

(export 'gdk-time-coord)

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabStatus
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGrabStatus" gdk-grab-status
  (:export t
   :type-initializer "gdk_grab_status_get_type")
  (:success 0)
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen
  :failed)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-grab-status atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-grab-status atdoc:*external-symbols*)
 "@version{2020-11-8}
  @begin{short}
    Returned by the function @fun{gdk-seat-grab} to indicate success or the
    reason for the failure of the grab attempt.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkGrabStatus\" gdk-grab-status
  (:export t
   :type-initializer \"gdk_grab_status_get_type\")
  :success
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen)
  @end{pre}
  @begin[code]{table}
    @entry[:success]{The resource was successfully grabbed.}
    @entry[:already-grabbed]{The resource is actively grabbed by another
      client.}
    @entry[:invalid-time]{The resource was grabbed more recently than the
      specified time.}
    @entry[:not-viewable]{The grab window or the @arg{confine-to} window are
      not viewable.}
    @entry[:frozen]{The resource is frozen by an active grab of another client.}
    @entry[:failed]{The grab failed for some other reason.}
  @end{table}
  @see-class{gdk-device}
  @see-function{gdk-seat-grab}")

;;; ----------------------------------------------------------------------------
;;; GdkDeviceTool
;;; ----------------------------------------------------------------------------

;; TODO: Implement the functions for GdkDeviceTool

#+gdk-3-22
(define-g-object-class "GdkDeviceTool" gdk-device-tool
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_tool_get_type")
  ((axes
    gdk-device-tool-axes
    "axes" "GdkAxisFlags" t nil)
   (hardware-id
    gdk-device-tool-hardware-id
    "hardware-id" "guint64" t nil)
   (serial
    gdk-device-tool-serial
    "serial" "guint64" t nil)
   (tool-type
    gdk-device-tool-tool-type
    "tool-type" "GdkDeviceToolType" t nil)))

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (documentation 'gdk-device-tool 'type)
 "@version{2020-11-8}
  @short{No documentation.}")

;;; ----------------------------------------------------------------------------
;;; GdkDevice
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDevice" gdk-device
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_get_type")
  ((associated-device
    gdk-device-associated-device
    "associated-device" "GdkDevice" t nil)
   #+gdk-3-22
   (axes
    gdk-device-axes
    "axes" "GdkAxisFlags" t nil)
   (device-manager
    gdk-device-device-manager
    "device-manager" "GdkDeviceManager" t t)
   (display
    gdk-device-display
    "display" "GdkDisplay" t t)
   (has-cursor
    gdk-device-has-cursor
    "has-cursor" "gboolean" t t)
   (input-mode
    gdk-device-input-mode
    "input-mode" "GdkInputMode" t t)
   (input-source
    gdk-device-input-source
    "input-source" "GdkInputSource" t t)
   (n-axes
    gdk-device-n-axes
    "n-axes" "gint" t nil)
   (name
    gdk-device-name
    "name" "gchararray" t t)
   #+gdk-3-20
   (num-touches
    gdk-device-num-touches
    "num-touches" "guint" t t)
   (product-id
    gdk-device-product-id
    "product-id" "gchararray" t t)
   #+gdk-3-20
   (seat
    gdk-device-seat
    "seat" "GdkSeat" t t)
   #+gdk-3-22
   (tool
    gdk-device-tool
    "tool" "GdkDeviceTool" t nil)
   (type
    gdk-device-type
    "type" "GdkDeviceType" t t)
   (vendor-id
    gdk-device-vendor-id
    "vendor-id" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-device 'type)
 "@version{2020-11-8}
  @begin{short}
    The @sym{gdk-device} object represents a single input device, such as a
    keyboard, a mouse, a touchpad, etc.
  @end{short}

  See the documentation of the @class{gdk-device-manager} class for more
  information about the various kinds of master and slave devices, and their
  relationships.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (device)   : Run Last
      @end{pre}
      The \"changed\" signal is emitted either when the @sym{gdk-device} has
      changed the number of either axes or keys. For example In X this will
      normally happen when the slave device routing events through the master
      device changes (for example, user switches from the USB mouse to a
      tablet), in that case the master device will change to reflect the new
      slave device axes and keys.
      @begin[code]{table}
        @entry[device]{The @sym{gdk-device} that changed.}
      @end{table}
    @subheading{The \"tool-changed\" signal}
      @begin{pre}
 lambda (device tool)    : Run Last
      @end{pre}
      The \"tool-changed\" signal is emitted on pen/eraser devices whenever
      tools enter or leave proximity.
      @begin[code]{table}
        @entry[device]{The @sym{gdk-device} that changed.}
        @entry[tool]{The new @class{gtk-device-tool} current tool.}
      @end{table}
      Since 3.22
  @end{dictionary}
  @see-slot{gdk-device-associated-device}
  @see-slot{gdk-device-axes}
  @see-slot{gdk-device-device-manager}
  @see-slot{gdk-device-display}
  @see-slot{gdk-device-has-cursor}
  @see-slot{gdk-device-input-mode}
  @see-slot{gdk-device-input-source}
  @see-slot{gdk-device-n-axes}
  @see-slot{gdk-device-name}
  @see-slot{gdk-device-num-touches}
  @see-slot{gdk-device-product-id}
  @see-slot{gdk-device-seat}
  @see-slot{gdk-device-tool}
  @see-slot{gdk-device-type}
  @see-slot{gdk-device-vendor-id}
  @see-class{gdk-device-manager}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;;-----------------------------------------------------------------------------

;;; --- gdk-device-associated-device -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "associated-device" 'gdk-device)
                     't)
 "The @code{associated-device} property of type @sym{gdk-device} (Read) @br{}
  Associated pointer or keyboard with this device, if any. Devices of type
  @code{:master} always come in keyboard/pointer pairs. Other device types will
  have a @code{nil} associated device.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-associated-device atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-associated-device 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-associated-device object) => associated-device}
  @argument[device]{a @class{gdk-device} object}
  @argument[associated-device]{the associated @class{gdk-device} object}
  @begin{short}
    Accessor of the @slot[gdk-device]{associated-device} slot of the
    @class{gdk-device} class.
  @end{short}

  The slot access function @sym{gdk-device-associated-device} returns the
  associated device to @arg{device}. If the device is of type @code{:master},
  it will return the paired pointer or keyboard. If the device is of type
  @code{:slave}, it will return the master device to which the device is
  attached to. If the device is of type @code{:floating}, @code{nil} will be
  returned, as there is no associated device.
  @see-class{gdk-device}")

;;; --- gdk-device-axes --------------------------------------------------------

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "axes" 'gdk-device) 't)
 "The @code{axes} property of type @symbol{gdk-axis-flag} (Read) @br{}
  The axes currently available for this device. Since 3.22")

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-device-axes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-axes 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-axes object) => axes}
  @argument[device]{a @class{gdk-device} object}
  @argument[axes]{the @symbol{gdk-axis-flag} flags}
  @begin{short}
    Accessor of the @slot[gdk-device]{axes} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access function @sym{gdk-device-axes} returns the axes currently
  available on the device.

  Since 3.22
  @see-class{gdk-device}
  @see-symbol{gdk-axis-flag}")

;;; --- gdk-device-device-manager ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "device-manager" 'gdk-device) 't)
 "The @code{device-manager} property of type @class{gdk-device-manager}
  (Read / Write / Construct) @br{}
  The device manager the @sym{gdk-device} pertains to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-device-manager atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-device-manager 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-device-manager object) => device-manager}
  @syntax[]{(setf (gdk-device-device-manager object) device-manager)}
  @argument[object]{a @class{gdk-device} object}
  @argument[device-manager]{a @class{gdk-device-manager} object}
  @begin{short}
    Accessor of the @slot[gdk-device]{device-manager} slot of the
    @class{gdk-device} class.
  @end{short}
  @see-class{gdk-device}
  @see-class{gdk-device-manager}")

;;; --- gdk-device-display -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display" 'gdk-device) 't)
 "The @code{display} property of type @class{gdk-display}
  (Read / Write / Construct) @br{}
  The display the device pertains to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-display 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-display object) => display}
  @syntax[]{(setf (gdk-device-display object) display)}
  @argument[object]{a @class{gdk-device} object}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Accessor of the @slot[gdk-device]{display} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access function @sym{gdk-device-display} returns the display to
  which the device pertains.
  @see-class{gdk-device}
  @see-class{gdk-display}")

;;; --- gdk-device-has-cursor --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-cursor" 'gdk-device) 't)
 "The @code{has-cursor} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the device is represented by a cursor on the screen. Devices of type
  @code{:master} will have @em{true} here. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-has-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-has-cursor 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-has-cursor object) => has-cursor}
  @syntax[]{(setf (gdk-device-has-cursor object) has-cursor)}
  @argument[object]{a @class{gdk-device} object}
  @argument[has-cursor]{a boolean wether the device is represented by a cursor
    on the screen}
  @begin{short}
    Accessor of the @slot[gdk-device]{has-cursor} slot of the
    @class{gdk-device} class.
  @end{short}

  The slot access function @sym{gdk-device-has-cursor} returns @em{true} if the
  pointer follows device motion.
  @see-class{gdk-device}")

;;; --- gdk-device-input-mode --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-mode" 'gdk-device) 't)
 "The @code{input-mode} property of type @symbol{gdk-input-mode} (Read / Write)
  @br{}
  Input mode for the device. @br{}
  Default value: @code{:disabled}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-input-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-input-mode 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-input-mode object) => mode}
  @syntax[]{(setf (gdk-device-input-mode object) mode)}
  @argument[object]{a @class{gdk-device} object}
  @argument[mode]{the input mode of type @symbol{gdk-input-mode}}
  @begin{short}
    Accessor of the @slot[gdk-device]{input-mode} slot of the
    @class{gdk-device} class.
  @end{short}

  The slot access function @sym{gdk-device-input-mode} returns the mode of the
  device. The slot access function @sym{(setf gdk-device-input-mode object)}
  sets he mode of an input device.

  The mode controls if the device is active and whether the device's range is
  mapped to the entire screen or to a single window.
  @see-class{gdk-device}
  @see-symbol{gdk-input-mode}")

;;; --- gdk-device-input-source ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-source" 'gdk-device) 't)
 "The @code{input-source} property of type @symbol{gdk-input-source}
  (Read / Write / Construct) @br{}
  Source type for the device. @br{}
  Default value: @code{:mouse}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-input-source atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-input-source 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-input-source object) => source}
  @syntax[]{(setf (gdk-device-input-source object) source)}
  @argument[object]{a @class{gdk-device} object}
  @argument[source]{the @symbol{gdk-input-source} source type for the device}
  @begin{short}
    Accessor of the @slot[gdk-device]{input-source} slot of the
    @class{gdk-device} class.
  @end{short}

  The slot access function @sym{gdk-device-input-source} returns the source
  type of the device.
  @see-class{gdk-device}
  @see-symbol{gdk-input-source}")

;;; --- gdk-device-n-axes ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-axes" 'gdk-device) 't)
 "The @code{n-axes} property of type @code{:uint} (Read) @br{}
  Number of axes in the device. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-n-axes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-n-axes 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-n-axis object) => n-axis}
  @argument[object]{a pointer @class{gdk-device} object}
  @argument[n-axis]{an unsigned integer with the number of axes in the device}
  @begin{short}
    Accessor of the @slot[gdk-device]{n-axes} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access functon @sym{gdk-device-n-axis} returns the number of axes
  the device currently has.
  @see-class{gdk-device}")

;;; --- gdk-device-name --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gdk-device) 't)
 "The @code{name} property of type @code{:string} (Read / Write / Construct)
  @br{}
  The device name. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-name 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-name object) => name}
  @syntax[]{(setf (gdk-device-name object) name)}
  @argument[object]{a @class{gdk-device} object}
  @argument[name]{a string with the device name}
  @begin{short}
    Accessor of the @slot[gdk-device]{name} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access function @sym{gdk-device-name} returns the name of the device.
  @see-class{gdk-device}")

;;; --- gdk-device-num-touches -------------------------------------------------

#+(and gdk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "num-touches" 'gdk-device) 't)
 "The @code{num-touches} property of type @code{:uint}
  (Read / Write / Construct) @br{}
  The maximal number of concurrent touches on a touch device. Will be 0 if the
  device is not a touch device or if the number of touches is unknown.
  Since 3.20 @br{}
  Default value: 0")

#+(and gdk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-device-num-touches atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-num-touches 'function)
 "@version{2020-10-28}
  @syntax[]{(gdk-device-num-touches object) => num-touches}
  @argument[object]{a @class{gdk-device} object}
  @argument[num-touches]{an unsigned integer with the number of touches}
  @begin{short}
    Accessor of the @slot[gdk-device]{num-touches} slot of the
    @class{gdk-device} class.
  @end{short}

  The maximal number of concurrent touches on a touch device. Will be 0 if the
  device is not a touch device or if the number of touches is unknown.

  Since 3.20
  @see-class{gdk-device}")

;;; --- gdk-device-product-id --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "product-id" 'gdk-device) 't)
 "The @code{product-id} property of type @code{:string}
  (Read / Write / Construct) @br{}
  Product ID of this device. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-product-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-product-id 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-product-id object) => product-id}
  @argument[object]{a @class{gdk-device} object}
  @argument[product-id]{a string with the product ID}
  @begin{short}
    Accessor of the @slot[gdk-device]{product-id} slot of the
    @class{gdk-device} class.
  @end{short}

  The slot access function @sym{gdk-device-product-id} returns the product ID
  of this device, or @code{nil} if this information could not be obtained. This
  ID is retrieved from the device, and is thus constant for it. See the
  function @fun{gdk-device-vendor-id} for more information.
  @see-class{gdk-device}
  @see-function{gdk-device-vendor-id}")

;;; --- gdk-device-seat --------------------------------------------------------

#+(and gdk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "seat" 'gdk-device) 't)
 "The @code{seat} property of type @class{gdk-seat} (Read / Write) @br{}
  The seat of this device. Since 3.20")

#+(and gdk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-device-seat atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-seat 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-seat object) => seat}
  @argument[object]{a @class{gdk-device} object}
  @argument[seat]{a @class{gdk-seat} object}
  @begin{short}
    Accessor of the @slot[gdk-device]{seat} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access function @sym{gdk-device-seat} returns the seat the device
  belongs to.

  Since 3.20
  @see-class{gdk-device}")

;;; --- gdk-device-tool --------------------------------------------------------

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "tool" 'gdk-device) 't)
 "The @code{tool} property of type @class{gdk-device-tool} (Read) @br{}
  The tool that is currently used with this device. Since 3.22")

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-device-tool atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-tool 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-tool object) => tool}
  @argument[object]{a @class{gdk-device} object}
  @argument[tool]{a @class{gdk-device-tool} object}
  @begin{short}
    Accessor of the @slot[gdk-device]{tool} slot of the @class{gdk-device}
    class.
  @end{short}

  The tool that is currently used with this device.

  Since 3.22
  @see-class{gdk-device}
  @see-class{gdk-device-tool}")

;;; --- gdk-device-type --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "type" 'gdk-device) 't)
 "The @code{type} property of type @symbol{gdk-device-type}
  (Read / Write / Construct) @br{}
  Device role in the device manager. @br{}
  Default value: @code{:master}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-type 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-type object) => type}
  @argument[object]{a @class{gdk-device} object}
  @argument[type]{a @symbol{gdk-device-type} value}
  @begin{short}
    Accessor of the @slot[gdk-device]{type} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access function @sym{gdk-device-type} returns the device type.
  @see-class{gdk-device}
  @see-symbol{gdk-device-type}")

;;; --- gdk-device-vendor-id ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vendor-id" 'gdk-device) 't)
 "The @code{vendor-id} property of type @code{:string}
  (Read / Write / Construct) @br{}
  Vendor ID of this device. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-vendor-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-vendor-id 'function)
 "@version{2020-11-8}
  @syntax[]{(gdk-device-vendor-id object) => vendor-id}
  @argument[object]{a @class{gdk-device} object}
  @argument[vendor-id]{a string with the vendor ID}
  @begin{short}
    Accessor of the @slot[gdk-device]{vendor-id} slot of the @class{gdk-device}
    class.
  @end{short}

  The slot access function @sym{gdk-device-vendor-id} returns the vendor ID of
  this device, or @code{nil} if this information could not be obtained. This ID
  is retrieved from the device, and is thus constant for it.

  This function, together with the function @fun{gdk-device-product-id}, can be
  used to e.g. compose @code{GSettings} paths to store settings for this
  device.
  @begin[Example]{dictionary}
    @begin{pre}
static GSettings *
get_device_settings (GdkDevice *device)
{
  const gchar *vendor, *product;
  GSettings *settings;
  GdkDevice *device;
  gchar *path;

  vendor = gdk_device_get_vendor_id (device);
  product = gdk_device_get_product_id (device);

  path = g_strdup_printf (\"/org/example/app/devices/%s:%s/\", vendor, product);
  settings = g_settings_new_with_path (DEVICE_SCHEMA, path);
  g_free (path);

  return settings;
@}
    @end{pre}
  @end{dictionary}
  @see-class{gdk-device}
  @see-function{gdk-device-product-id}")

;;; ----------------------------------------------------------------------------

#-windows
(define-g-object-class "GdkX11DeviceXI2" gdk-x11-device-xi2
  (:superclass gdk-device
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_device_xi2_get_type")
  ((device-id
    gdk-x11-device-xi2-device-id
    "device-id" "gint" t nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_source ()
;;; ----------------------------------------------------------------------------

;; This is equivalent to the slot access function gdk-device-input-source.

(defun gdk-device-get-source (device)
 #+cl-cffi-gtk-documentation
 "@version{2016-1-1}
  @argument[device]{a @class{gdk-device} object}
  @return{A @symbol{gdk-input-source}.}
  @short{Determines the type of the device.}

  @sym{gdk-device-get-source} is a synonym for the slot access function
  @fun{gdk-device-input-source}.
  @see-class{gdk-device}
  @see-function{gdk-device-input-source}"
  (gdk-device-input-source device))

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_mode ()
;;; ----------------------------------------------------------------------------

;; This is equivalent to the slot access function (setf gdk-device-input-mode).

(defun gdk-device-set-mode (device mode)
 #+cl-cffi-gtk-documentation
 "@version{2016-1-1}
  @argument[device]{a @class{gdk-device} object}
  @argument[mode]{the input mode}
  @return{@em{True} if the mode was successfully changed.}
  @begin{short}
    Sets a the mode of an input device.
  @end{short}
  The mode controls if the device is active and whether the device's range is
  mapped to the entire screen or to a single window.

  @sym{gdk-device-set-mode} is a synonym for the slot access function
  @fun{gdk-device-input-mode}.
  @see-class{gdk-device}
  @see-function{gdk-device-input-mode}"
  (setf (gdk-device-input-mode device) mode))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_mode ()
;;; ----------------------------------------------------------------------------

;; This is equivalent to the slot access function gdk-device-input-mode.

(defun gdk-device-get-mode (device)
 #+cl-cffi-gtk-documentation
 "@version{2016-1-1}
  @argument[device]{a @class{gdk-device} object}
  @return{A @symbol{gdk-input-source}.}
  @short{Determines the mode of the device.}

  @sym{gdk-device-set-mode} is a synonym for the slot access function
  @fun{gdk-device-input-mode}.
  @see-class{gdk-device}
  @see-function{gdk-device-input-mode}"
  (gdk-device-input-mode device))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_key ()
;;; gdk_device_set_key () -> gdk-device-key
;;; ----------------------------------------------------------------------------

(defun (setf gdk-device-key) (value device index)
  (destructuring-bind (keyval modifiers) value
    (foreign-funcall "gdk_device_set_key"
                     (g-object gdk-device) device
                     :uint index
                     :uint keyval
                     gdk-modifier-type modifiers
                     :void)
    (values keyval modifiers)))

(defcfun ("gdk_device_get_key" %gdk-device-key) :boolean
  (device (g-object gdk-device))
  (index :int)
  (keyval (:pointer :uint))
  (modifiers (:pointer gdk-modifier-type)))

(defun gdk-device-key (device index)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @syntax[]{(gdk-device-key device index) => keyval, modifiers}
  @syntax[]{(setf (gdk-device-key device index) (list keyval modifiers))}
  @argument[device]{a @class{gdk-device} object}
  @argument[index]{an unsigned integer with the index of the macro button
    to get}
  @argument[keyval]{an unsigned integer with the keyval to generate}
  @argument[modifiers]{the modifiers of type @symbol{gdk-modifier-type}}
  @begin{short}
    Accessor of the keyval and modifiers of a device.
  @end{short}

  If @arg{index} has a valid keyval, the function @sym{gdk-device-key} will
  return @arg{keyval} and @arg{modifiers} with the keyval settings. The
  function @sym{(setf gdk-device-key)} specifies the X key event to generate
  when a macro button of a device is pressed.
  @see-class{gdk-device}
  @see-symbol{gdk-modifier-type}"
  (with-foreign-objects ((keyval :uint)
                         (modifiers 'gdk-modifier-type))
    (when (%gdk-device-key device index keyval modifiers)
      (values (mem-ref keyval :uint)
              (mem-ref modifiers 'gdk-modifier-type)))))

(export 'gdk-device-key)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_use ()
;;; gdk_device_set_axis_use () -> gdk-device-axis-use
;;; ----------------------------------------------------------------------------

(defun (setf gdk-device-axis-use) (use device index)
  (foreign-funcall "gdk_device_set_axis_use"
                   (g-object gdk-device) device
                   :uint index
                   gdk-axis-use use
                   :void)
  use)

(defcfun ("gdk_device_get_axis_use" gdk-device-axis-use) gdk-axis-use
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @syntax[]{(gdk-device-axis-use device index) => use}
  @syntax[]{(setf (gdk-device-axis-use device index) use)}
  @argument[device]{a @class{gdk-device} pointer device}
  @argument[index]{an unsigned integer with the index of the axis}
  @argument[use]{a @symbol{gdk-axis-use} value specifying how the axis is used}
  @begin{short}
    Accessor of the value specifying how the axis is used.
  @end{short}

  The function @sym{gdk-device-axis-use} returns the axis use for @arg{index}.
  The function @sym{(setf gdk-device-axis-use)} sets the axis use for
  @arg{index}.

  @see-class{gdk-device}
  @see-symbol{gdk-axis-use}"
  (device (g-object gdk-device))
  (index :uint))

(export 'gdk-device-axis-use)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_slave_devices ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_list_slave_devices" gdk-device-list-slave-devices)
    (g-list (g-object gdk-device))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object}
  @return{The list of slave @class{gdk-device} objects, or @code{nil}.}
  @begin{short}
    If the device is of type @code{:master}, it will return the list of
    slave devices attached to it, otherwise it will return @code{nil}.
  @end{short}
  @see-class{gdk-device}"
  (device (g-object gdk-device)))

(export 'gdk-device-list-slave-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_n_keys () -> gdk-device-n-keys
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_n_keys" gdk-device-n-keys) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object}
  @return{An integer with the number of keys.}
  @short{Returns the number of keys the device currently has.}
  @see-class{gdk-device}"
  (device (g-object gdk-device)))

(export 'gdk-device-n-keys)

;;; ----------------------------------------------------------------------------
;;; gdk_device_warp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_warp" gdk-device-warp) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{the @class{gdk-device} object to warp}
  @argument[screen]{the @class{gdk-screen} object to warp @arg{device} to}
  @argument[x]{an integer with the x coordinate of the destination}
  @argument[y]{an integer with the y coordinate of the destination}
  @begin{short}
    Warps the device in display to the point x,y on the screen, unless the
    device is confined to a window by a grab, in which case it will be moved
    as far as allowed by the grab.
  @end{short}
  Warping the pointer creates events as if the user had moved the mouse
  instantaneously to the destination.

  Note that the pointer should normally be under the control of the user. This
  function was added to cover some rare use cases like keyboard navigation
  support for the color picker in the @class{gtk-color-selection-dialog}.
  @see-class{gdk-device}
  @see-class{gtk-color-selection-dialog}"
  (device (g-object gdk-device))
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-device-warp)

;;; ----------------------------------------------------------------------------
;;; gdk_device_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_grab" gdk-device-grab) gdk-grab-status
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object. To get the device you can use
    the functions @fun{gtk-current-event-device} or @fun{gdk-event-device}
    if the grab is in reaction to an event. Also, you can use the function
    @fun{gdk-device-manager-client-pointer} but only in code that is not
    triggered by a @class{gdk-event} and there are not other means to get a
    meaningful @class{gdk-device} object to operate on.}
  @argument[window]{the @class{gdk-window} object which will own the grab,
    the grab window}
  @argument[grab-ownership]{a @symbol{gdk-grab-ownership} value which specifies
    the grab ownership}
  @argument[owner-events]{if @em{false} then all device events are reported
    with respect to @arg{window} and are only reported if selected by
    @arg{event-mask}. If @em{true} then pointer events for this application are
    reported as normal, but pointer events outside this application are
    reported with respect to @arg{window} and only if selected by
    @arg{event-mask}. In either mode, unreported events are discarded.}
  @argument[event-mask]{specifies the event mask of type
    @symbol{gdk-event-mask}, which is used in accordance with
    @arg{owner-events}.}
  @argument[cursor]{the @class{gdk-cursor} object to display while the grab is
    active if the device is a pointer. If this is @code{nil} then the normal
    cursors are used for window and its descendants, and the cursor for window
    is used elsewhere.}
  @argument[time]{a @code{:uint32} with the timestamp of the event which led to
    this pointer grab. This usually comes from the @class{gdk-event} structure,
    though @var{+gdk-current-time+} can be used if the time is not known.}
  @return{@code{:sucess} if the grab was successful.}
  @begin{short}
    Grabs the device so that all events coming from this device are passed to
    this application until the device is ungrabbed with the function
    @fun{gdk-device-ungrab}, or the window becomes unviewable.
  @end{short}
  This overrides any previous grab on the device by this client.

  Device grabs are used for operations which need complete control over the
  given device events, either pointer or keyboard. For example in GTK+ this
  is used for Drag and Drop operations, popup menus and such.

  Note that if the event mask of an X window has selected both button press
  and button release events, then a button press event will cause an automatic
  pointer grab until the button is released. X does this automatically since
  most applications expect to receive button press and release events in
  pairs. It is equivalent to a pointer grab on the window with
  @arg{owner-events} set to @em{true}.

  If you set up anything at the time you take the grab that needs to be
  cleaned up when the grab ends, you should handle the
  @class{gdk-event-grab-broken} events that are emitted when the grab ends
  unvoluntarily.
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-class{gdk-event}
  @see-class{gdk-cursor}
  @see-class{gdk-event-grab-broken}
  @see-function{gdk-device-ungrab}
  @see-function{gdk-event-device}
  @see-function{gtk-current-event-device}
  @see-function{gdk-device-manager-client-pointer}
  @see-variable{+gdk-current-time+}"
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (grab-ownership gdk-grab-ownership)
  (owner-events :boolean)
  (event-mask gdk-event-mask)
  (cursor (g-object gdk-cursor))
  (time :uint32))

(export 'gdk-device-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_ungrab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_ungrab" gdk-device-ungrab) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[device]{a @class{gdk-device} object}
  @argument[time]{a @code{:uint32} with the timestamp, e.g.
    @var{+gdk-current-time+}}
  @short{Release any grab on the device.}
  @see-class{gdk-device}
  @see-function{gdk-device-grab}"
  (device (g-object gdk-device))
  (time :uint32))

(export 'gdk-device-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_state () -> gdk-device-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_state" %gdk-device-state) :void
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (axes (:pointer :double))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-device-state (device window)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{axes} -- a list of @code{:double} numbers with the axes value @br{}
    @code{mask} -- the modifiers of type @symbol{gdk-modifier-type},
    or @code{nil}
  @end{return}
  @begin{short}
    Gets the current state of a pointer device relative to the window.
  @end{short}
  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the function @fun{gdk-device-grab}.
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-symbol{gdk-modifier-type}
  @see-function{gdk-device-grab}"
  (with-foreign-objects ((axes :double (gdk-device-n-axes device))
                         (mask 'gdk-modifier-type))
    (%gdk-device-state device window axes mask)
    (values (iter (for i from 0 below (gdk-device-n-axes device))
                  (collect (mem-aref axes :double i)))
            (mem-ref mask 'gdk-modifier-type))))

(export 'gdk-device-state)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_position () -> gdk-device-position
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_position" %gdk-device-position) :void
  (display (g-object gdk-device))
  (screen :pointer)
  (x :pointer)
  (y :pointer))

(defun gdk-device-position (device)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} pointer device to query status about}
  @begin{return}
    @code{screen} -- the @class{gdk-screen} object the device is on,
    or @code{nil} @br{}
    @code{x} -- an integer with the root window x coordinate of device,
    or @code{nil} @br{}
    @code{y} -- an integer with the root window y coordinate of device,
    or @code{nil}
  @end{return}
  @begin{short}
    Gets the current location of the device.
  @end{short}
  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the function @fun{gdk-device-grab}.
  @see-class{gdk-device}
  @see-class{gdk-screen}
  @see-function{gdk-device-grab}"
  (with-foreign-objects ((screen :pointer) (x :int) (y :int))
    (%gdk-device-position device screen x y)
    (values (mem-ref screen '(g-object gdk-screen))
            (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-device-position)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_position_double () -> gdk-device-position-double
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_position_double" %gdk-device-position-double) :void
  (device (g-object gdk-device))
  (screen (g-object gdk-screen))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gdk-device-position-double (device)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} pointer device to query status about}
  @begin{return}
    @code{screen} -- a @class{gdk-screen} object the device is on,
    or @code{nil} @br{}
    @code{x} -- a @code{:double} with the root window x coordinate of the
    device, or @code{nil} @br{}
    @code{y} -- a @code{:double} with the root window y coordinate of the
    device, or @code{nil}
  @end{return}
  @begin{short}
    Gets the current location of the device in double precision.
  @end{short}
  As a slave device's coordinates are those of its master pointer, this
  function may not be called on devices of type @code{:slave}, unless there is
  an ongoing grab on them. See the function @fun{gdk-device-grab}.
  @see-class{gdk-device}
  @see-class{gdk-screen}
  @see-function{gdk-device-grab}"
  (with-foreign-objects ((screen :pointer) (x :double) (y :double))
    (%gdk-device-position-double device screen x y)
    (values (mem-ref screen '(g-object gdk-screen))
            (mem-ref x :double)
            (mem-ref y :double))))

(export 'gdk-device-position-double)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_window_at_position () -> gdk-device-window-at-position
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_window_at_position" %gdk-device-window-at-position)
    (g-object gdk-window)
  (device (g-object gdk-device))
  (win-x (:pointer :int))
  (win-y (:pointer :int)))

(defun gdk-device-window-at-position (device)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[device]{a @class{gdk-device} pointer device to query info to}
  @begin{return}
    @code{window} -- a @class{gdk-window} object @br{}
    @code{win-x} -- an integer with the x coordinate of the device location,
    relative to the window origin, or @code{nil} @br{}
    @code{win-y} -- an integer with the y coordinate of the device location,
    relative to the window origin, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the window underneath the device, returning the location of the
    device.
  @end{short}
  Returns @code{nil} if the window tree under the device is not known to GDK
  (for example, belongs to another application).

  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the function @fun{gdk-device-grab}.
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-function{gdk-device-grab}"
  (with-foreign-objects ((win-x :int) (win-y :int))
    (let ((window (%gdk-device-window-at-position device win-x win-y)))
      (values window
              (mem-ref win-x :int)
              (mem-ref win-y :int)))))

(export 'gdk-device-window-at-position)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_window_at_position_double ()
;;; -> gdk-device-window-at-position-double
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_window_at_position_double"
          %gdk-device-window-at-position-double) (g-object gdk-window)

  (device (g-object gdk-device))
  (win-x (:pointer :double))
  (win-y (:pointer :double)))

(defun gdk-device-window-at-position-double (device)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} pointer device to query info to}
  @begin{return}
    @code{window} -- the @class{gdk-window} object under the device position,
    or @code{nil} @br{}
    @code{win-x} -- a @code{:double} for the x coordinate of the device
    location, relative to the window origin, or @code{nil} @br{}
    @code{win-y} -- a @code{:double} for the y coordinate of the device
    location, relative to the window origin, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the window underneath the device, returning the location of the
    device in double precision.
  @end{short}
  Returns @code{nil} if the window tree under the device is not known to GDK
  (for example, belongs to another application).

  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the function @fun{gdk-device-grab}.
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-function{gdk-device-grab}"
  (with-foreign-objects ((win-x :double) (win-y :double))
    (let ((window (%gdk-device-window-at-position-double device win-x win-y)))
      (values window
              (mem-ref win-x :double)
              (mem-ref win-y :double)))))

(export 'gdk-device-window-at-position-double)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_history () -> gdk-device-history
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_history" %gdk-device-history) :boolean
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (start :uint32)
  (stop :uint32)
  (events (:pointer (:pointer (:pointer (:struct gdk-time-coord)))))
  (n-events (:pointer :int)))

(defun gdk-device-history (device window start stop)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object}
  @argument[window]{the @class{gdk-window} object with respect to which which
    the event coordinates will be reported}
  @argument[start]{an unsigned integer with the starting timestamp for range of
    events to return}
  @argument[stop]{an unsigned integer with the ending timestamp for the range
    of events to return}
  @begin{return}
    A list of @class{gdk-time-coord} structures if the windowing system
    supports motion history and at least one event was found, or @code{nil}.
  @end{return}
  @begin{short}
    Obtains the motion history for a pointer device.
  @end{short}
  Given a starting and ending timestamp, return all events in the motion
  history for the device in the given range of time.

  Some windowing systems do not support motion history, in which case,
  @code{nil} will be returned. This is not distinguishable from the case where
  motion history is supported and no events were found.
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-symbol{gdk-time-coord}"
  (with-foreign-objects ((events :pointer) (n-events :int))
    (when (%gdk-device-history device window start stop events n-events)
      (prog1
        (iter (with events-ar = (mem-ref events :pointer))
              (for i from 0 below (mem-ref n-events :int))
              (for coord = (mem-aref events-ar
                                     '(:struct gdk-time-coord)
                                     i))
              (collect coord))
        (%gdk-device-free-history (mem-ref events :pointer)
                                  (mem-ref n-events :int))))))

(export 'gdk-device-history)

;;; ----------------------------------------------------------------------------
;;; gdk_device_free_history ()
;;; ----------------------------------------------------------------------------

;; For internal use in gdk-device-history and not exported.

(defcfun ("gdk_device_free_history" %gdk-device-free-history) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[events]{an array of @class{gdk-time-coord}}
  @argument[n-events]{the length of the array}
  @begin{short}
    Frees an array of @class{gdk-time-coord} structures that was returned by
    the function @fun{gdk-device-history}.
  @end{short}
  @see-class{gdk-time-coord}
  @see-function{gdk-device-history}"
  (events (:pointer (:pointer (:struct gdk-time-coord))))
  (n-events :int))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis () -> gdk-device-axis
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_axis" %gdk-device-axis) :boolean
  (device (g-object gdk-device))
  (axes (:pointer :double))
  (use gdk-axis-use)
  (value (:pointer :double)))

(defun gdk-device-axis (device axes axis-use)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object}
  @argument[axes]{a list of @code{:double} values of axes}
  @argument[use]{the @symbol{gdk-axis-use} value use to look for}
  @return{The found value of type @code{:double}, otherwise @code{nil}.}
  @begin{short}
    Interprets a list of double as axis values for a given device, and locates
    the value in the array for a given axis use.
  @end{short}
  @see-class{gdk-device}
  @see-symbol{gdk-axis-use}"
  (assert (= (gdk-device-n-axes device) (length axes)))
  (with-foreign-objects ((axes-ar :double (gdk-device-n-axes device))
                         (value :double))
    (let ((i 0))
      (map nil
           (lambda (v)
             (setf (mem-aref axes-ar :double i) v)
             (incf i))
           axes))
    (when (%gdk-device-axis device axes-ar axis-use value)
      (mem-ref value :double))))

(export 'gdk-device-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_axes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_list_axes" gdk-device-list-axes)
    (g-list gdk-atom-as-string)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[device]{a @class{gdk-device} object}
  @return{A list of atoms as a string.}
  @begin{short}
    Returns a list of atoms as string, containing the labels for the axes that
    the device currently has.
  @end{short}
  @see-class{gdk-device}
  @see-symbol{gdk-atom}"
  (device (g-object gdk-device)))

(export 'gdk-device-list-axes)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_value () -> gdk-device-axis-value
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_axis_value" %gdk-device-axis-value) :boolean
  (device (g-object gdk-device))
  (axes (:pointer :double))
  (axis-label gdk-atom-as-string)
  (value (:pointer :double)))

(defun gdk-device-axis-value (device axes axis-label)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object for a pointer device}
  @argument[axes]{a list of double float of axes}
  @argument[axis-label]{an atom as a string with the axis label}
  @return{A double float with the found value, or @code{nil}.}
  @begin{short}
    Interprets a list of double floats as axis values for a given device, and
    locates the value in the array for a given axis label, as returned by the
    function @fun{gdk-device-list-axes}.
  @end{short}
  @see-class{gdk-device}
  @see-symbol{gdk-atom}
  @see-function{gdk-device-list-axes}"
  (assert (= (gdk-device-n-axes device) (length axes)))
  (with-foreign-objects ((axes-ar :double (gdk-device-n-axes device))
                         (value :double))
    (let ((i 0))
      (map nil
           (lambda (v)
             (setf (mem-aref axes-ar :double i) v)
             (incf i))
           axes))
    (when (%gdk-device-axis-value device axes-ar axis-label value)
      (mem-ref value :double))))

(export 'gdk-device-axis-value)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_last_event_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_last_event_window" gdk-device-last-event-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-5}
  @argument[device]{a @class{gdk-device} object with a source other than
    @code{:keyboard}}
  @return{The last @class{gdk-window} object the device is in.}
  @begin{short}
    Gets information about which window the given pointer device is in, based
    on events that have been received so far from the display server.
  @end{short}
  If another application has a pointer grab, or this application has a grab
  with @code{owner-events} = @em{false}, @code{nil} may be returned even if the
  pointer is physically over one of this application's windows.
  @see-class{gdk-device}
  @see-class{gdk-window}"
  (device (g-object gdk-device)))

(export 'gdk-device-last-event-window)

;;; ----------------------------------------------------------------------------
;;; gdk_device_tool_get_serial ()
;;;
;;; guint64 gdk_device_tool_get_serial (GdkDeviceTool *tool);
;;;
;;; Gets the serial of this tool, this value can be used to identify a physical
;;; tool (e.g. a tablet pen) across program executions.
;;;
;;; tool :
;;;     a GdkDeviceTool
;;;
;;; Returns :
;;;     The serial ID for this tool
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_tool_get_tool_type ()
;;;
;;; GdkDeviceToolType gdk_device_tool_get_tool_type (GdkDeviceTool *tool);
;;;
;;; Gets the GdkDeviceToolType of the tool.
;;;
;;; tool :
;;;     a GdkDeviceTool
;;;
;;; Returns :
;;;     The physical type for this tool. This can be used to figure out what
;;;     sort of pen is being used, such as an airbrush or a pencil.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_tool_get_hardware_id ()
;;;
;;; guint64
;;; gdk_device_tool_get_hardware_id (GdkDeviceTool *tool);
;;;
;;; Gets the hardware ID of this tool, or 0 if it's not known. When non-zero,
;;; the identificator is unique for the given tool model, meaning that two
;;; identical tools will share the same hardware_id , but will have different
;;; serial numbers (see gdk_device_tool_get_serial()).
;;;
;;; This is a more concrete (and device specific) method to identify a
;;; GdkDeviceTool than gdk_device_tool_get_tool_type(), as a tablet may support
;;; multiple devices with the same GdkDeviceToolType, but having different
;;; hardware identificators.
;;;
;;; tool :
;;;     a GdkDeviceTool
;;;
;;; Returns :
;;;     The hardware identificator of this tool.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.device.lisp --------------------------------------------
