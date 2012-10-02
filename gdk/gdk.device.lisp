;;; ----------------------------------------------------------------------------
;;; gdk.device.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GdkDevice
;;;
;;; Object representing an input device
;;;
;;; Synopsis
;;;
;;;     GdkDevice
;;;     GdkInputSource
;;;     GdkInputMode
;;;     GdkAxisUse
;;;     GdkDeviceType
;;;     GdkGrabOwnership
;;;
;;;     gdk_device_get_name
;;;     gdk_device_get_source
;;;     gdk_device_set_mode
;;;     gdk_device_get_mode
;;;     gdk_device_set_key
;;;     gdk_device_get_key
;;;     gdk_device_set_axis_use
;;;     gdk_device_get_axis_use
;;;     gdk_device_get_associated_device
;;;     gdk_device_list_slave_devices
;;;     gdk_device_get_device_type
;;;     gdk_device_get_display
;;;     gdk_device_get_has_cursor
;;;     gdk_device_get_n_axes
;;;     gdk_device_get_n_keys
;;;     gdk_device_warp
;;;
;;;     GdkGrabStatus               * from gdk.general.lisp *
;;;
;;;     gdk_device_grab
;;;     gdk_device_ungrab
;;;
;;;     gdk_device_get_state
;;;     gdk_device_get_position
;;;     gdk_device_get_window_at_position
;;;
;;;     GdkTimeCoord
;;;
;;;     gdk_device_get_history
;;;     gdk_device_free_history
;;;
;;;     gdk_device_get_axis
;;;     gdk_device_list_axes
;;;     gdk_device_get_axis_value
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkDevice
;;;
;;; Properties
;;;
;;;   "associated-device"        GdkDevice*           : Read
;;;   "device-manager"           GdkDeviceManager*    : Read / Write / Construct
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;   "has-cursor"               gboolean             : Read / Write / Construct
;;;   "input-mode"               GdkInputMode         : Read / Write
;;;   "input-source"             GdkInputSource       : Read / Write / Construct
;;;   "n-axes"                   guint                : Read
;;;   "name"                     gchar*               : Read / Write / Construct
;;;   "type"                     GdkDeviceType        : Read / Write / Construct
;;;
;;; Signals
;;;
;;;   "changed"                                       : Run Last
;;;
;;; Description
;;;
;;; The GdkDevice object represents a single input device, such as a keyboard, a
;;; mouse, a touchpad, etc.
;;;
;;; See the GdkDeviceManager documentation for more information about the
;;; various kinds of master and slave devices, and their relationships.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;;-----------------------------------------------------------------------------
;;; The "associated-device" property
;;;
;;;   "associated-device"        GdkDevice*            : Read
;;;
;;; Associated pointer or keyboard with this device, if any. Devices of type
;;; GDK_DEVICE_TYPE_MASTER always come in keyboard/pointer pairs. Other device
;;; types will have a NULL associated device.
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "device-manager" property
;;;
;;;   "device-manager"           GdkDeviceManager*    : Read / Write / Construct
;;;
;;; The GdkDeviceManager the GdkDevice pertains to.
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "display" property
;;;
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; The GdkDisplay the GdkDevice pertains to.
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "has-cursor" property
;;;
;;;   "has-cursor"               gboolean             : Read / Write / Construct
;;;
;;; Whether the device is represented by a cursor on the screen. Devices of type
;;; GDK_DEVICE_TYPE_MASTER will have TRUE here.
;;;
;;; Default value: FALSE
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "input-mode" property
;;;
;;;   "input-mode"               GdkInputMode          : Read / Write
;;;
;;; Input mode for the device.
;;;
;;; Default value: GDK_MODE_DISABLED
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "input-source" property
;;;
;;;   "input-source"             GdkInputSource       : Read / Write / Construct
;;;
;;; Source type for the device.
;;;
;;; Default value: GDK_SOURCE_MOUSE
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "n-axes" property
;;;
;;;   "n-axes"                   guint                 : Read
;;;
;;; Number of axes in the device.
;;;
;;; Default value: 0
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "name" property
;;;
;;;   "name"                     gchar*               : Read / Write / Construct
;;;
;;; The device name.
;;;
;;; Default value: NULL
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;; The "type" property
;;;
;;;   "type"                     GdkDeviceType        : Read / Write / Construct
;;;
;;; Device role in the device manager.
;;;
;;; Default value: GDK_DEVICE_TYPE_MASTER
;;;
;;; Since 3.0
;;;
;;;-----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;;-----------------------------------------------------------------------------
;;; The "changed" signal
;;;
;;; void user_function (GdkDevice *device,
;;;                     gpointer   user_data)      : Run Last
;;;
;;; The ::changed signal is emitted either when the GdkDevice has changed the
;;; number of either axes or keys. For example In X this will normally happen
;;; when the slave device routing events through the master device changes (for
;;; example, user switches from the USB mouse to a tablet), in that case the
;;; master device will change to reflect the new slave device axes and keys.
;;;
;;; device :
;;;     the GdkDevice that changed.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDevice
;;;
;;; typedef struct _GdkDevice GdkDevice;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDevice" gdk-device
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_get_type")
  ((associated-device
    gdk-device-associated-device
    "associated-device" "GdkDevice" t nil)
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
    "name" "gchar" t t)
   (type
    gdk-device-type
    "type" "GdkDeviceType" t t)))

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
;;; enum GdkInputSource
;;;
;;; typedef enum {
;;;   GDK_SOURCE_MOUSE,
;;;   GDK_SOURCE_PEN,
;;;   GDK_SOURCE_ERASER,
;;;   GDK_SOURCE_CURSOR,
;;;   GDK_SOURCE_KEYBOARD,
;;;   GDK_SOURCE_TOUCHSCREEN,
;;;   GDK_SOURCE_TOUCHPAD
;;; } GdkInputSource;
;;;
;;; An enumeration describing the type of an input device in general terms.
;;;
;;; GDK_SOURCE_MOUSE
;;;     the device is a mouse. (This will be reported for the core pointer, even
;;;     if it is something else, such as a trackball.)
;;;
;;; GDK_SOURCE_PEN
;;;     the device is a stylus of a graphics tablet or similar device.
;;;
;;; GDK_SOURCE_ERASER
;;;     the device is an eraser. Typically, this would be the other end of a
;;;     stylus on a graphics tablet.
;;;
;;; GDK_SOURCE_CURSOR
;;;     the device is a graphics tablet "puck" or similar device.
;;;
;;; GDK_SOURCE_KEYBOARD
;;;     the device is a keyboard.
;;;
;;; GDK_SOURCE_TOUCHSCREEN
;;;     the device is a direct-input touch device, such as a touchscreen or
;;;     tablet. This device type has been added in 3.4.
;;;
;;; GDK_SOURCE_TOUCHPAD
;;;     the device is an indirect touch device, such as a touchpad. This device
;;;     type has been added in 3.4.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInputSource" gdk-input-source
  (:export t
   :type-initializer "gdk_input_source_get_type")
  (:mouse 0)
  (:pen 1)
  (:eraser 2)
  (:cursor 3)
  (:keyboard 4)
  (:touchscreen 5)
  (:touchpad 6))

;;; ----------------------------------------------------------------------------
;;; enum GdkInputMode
;;;
;;; typedef enum {
;;;   GDK_MODE_DISABLED,
;;;   GDK_MODE_SCREEN,
;;;   GDK_MODE_WINDOW
;;; } GdkInputMode;
;;;
;;; An enumeration that describes the mode of an input device.
;;;
;;; GDK_MODE_DISABLED
;;;     the device is disabled and will not report any events.
;;;
;;; GDK_MODE_SCREEN
;;;     the device is enabled. The device's coordinate space maps to the entire
;;;     screen.
;;;
;;; GDK_MODE_WINDOW
;;;     the device is enabled. The device's coordinate space is mapped to a
;;;     single window. The manner in which this window is chosen is undefined,
;;;     but it will typically be the same way in which the focus window for key
;;;     events is determined.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInputMode" gdk-input-mode
  (:export t
   :type-initializer "gdk_input_mode_get_type")
  (:disabled 0)
  (:screen 1)
  (:window 2))

;;; ----------------------------------------------------------------------------
;;; enum GdkAxisUse
;;;
;;; typedef enum {
;;;   GDK_AXIS_IGNORE,
;;;   GDK_AXIS_X,
;;;   GDK_AXIS_Y,
;;;   GDK_AXIS_PRESSURE,
;;;   GDK_AXIS_XTILT,
;;;   GDK_AXIS_YTILT,
;;;   GDK_AXIS_WHEEL,
;;;   GDK_AXIS_LAST
;;; } GdkAxisUse;
;;;
;;; An enumeration describing the way in which a device axis (valuator) maps
;;; onto the predefined valuator types that GTK+ understands.
;;;
;;; GDK_AXIS_IGNORE
;;;     the axis is ignored.
;;;
;;; GDK_AXIS_X
;;;     the axis is used as the x axis.
;;;
;;; GDK_AXIS_Y
;;;     the axis is used as the y axis.
;;;
;;; GDK_AXIS_PRESSURE
;;;     the axis is used for pressure information.
;;;
;;; GDK_AXIS_XTILT
;;;     the axis is used for x tilt information.
;;;
;;; GDK_AXIS_YTILT
;;;     the axis is used for y tilt information.
;;;
;;; GDK_AXIS_WHEEL
;;;     the axis is used for wheel information.
;;;
;;; GDK_AXIS_LAST
;;;     a constant equal to the numerically highest axis value.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkAxisUse" gdk-axis-use
  (:export t
   :type-initializer "gdk_axis_use_get_type")
  (:ignore 0)
  (:x 1)
  (:y 2)
  (:pressure 3)
  (:xtilt 4)
  (:ytilt 5)
  (:wheel 6)
  (:last 7))

;;; ----------------------------------------------------------------------------
;;; enum GdkDeviceType
;;;
;;; typedef enum {
;;;   GDK_DEVICE_TYPE_MASTER,
;;;   GDK_DEVICE_TYPE_SLAVE,
;;;   GDK_DEVICE_TYPE_FLOATING
;;; } GdkDeviceType;
;;;
;;; Indicates the device type. See above for more information about the meaning
;;; of these device types.
;;;
;;; GDK_DEVICE_TYPE_MASTER
;;;     Device is a master (or virtual) device. There will be an associated
;;;     focus indicator on the screen.
;;;
;;; GDK_DEVICE_TYPE_SLAVE
;;;     Device is a slave (or physical) device.
;;;
;;; GDK_DEVICE_TYPE_FLOATING
;;;     Device is a physical device, currently not attached to any virtual
;;;     device.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkDeviceType" gdk-device-type
  (:export t
   :type-initializer "gdk_device_type_get_type")
  (:master 0)
  (:slave 1)
  (:floating 2))

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabOwnership
;;;
;;; typedef enum {
;;;   GDK_OWNERSHIP_NONE,
;;;   GDK_OWNERSHIP_WINDOW,
;;;   GDK_OWNERSHIP_APPLICATION
;;; } GdkGrabOwnership;
;;;
;;; Defines how device grabs interact with other devices.
;;;
;;; GDK_OWNERSHIP_NONE
;;;     All other devices' events are allowed.
;;;
;;; GDK_OWNERSHIP_WINDOW
;;;     Other devices' events are blocked for the grab window.
;;;
;;; GDK_OWNERSHIP_APPLICATION
;;;     Other devices' events are blocked for the whole application.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGrabOwnership" gdk-grab-ownership
  (:export t
   :type-initializer "gdk_grab_ownership_get_type")
  (:none 0)
  (:window 1)
  (:application 2))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_name ()
;;;
;;; const gchar * gdk_device_get_name (GdkDevice *device);
;;;
;;; Determines the name of the device.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     a name
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-name))

(defun gdk-device-get-name (device)
  (gdk-device-name device))

(export 'gdk-device-get-name)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_source ()
;;;
;;; GdkInputSource gdk_device_get_source (GdkDevice *device);
;;;
;;; Determines the type of the device.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     a GdkInputSource
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-source))

(defun gdk-device-get-source (device)
  (gdk-device-input-source device))

(export 'gdk-device-get-source)

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_mode ()
;;;
;;; gboolean gdk_device_set_mode (GdkDevice *device, GdkInputMode mode);
;;;
;;; Sets a the mode of an input device. The mode controls if the device is
;;; active and whether the device's range is mapped to the entire screen or to a
;;; single window.
;;;
;;; device :
;;;     a GdkDevice.
;;;
;;; mode :
;;;     the input mode.
;;;
;;; Returns :
;;;     TRUE if the mode was successfully changed.
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-set-mode))

(defun gdk-device-set-mode (device mode)
  (setf (gdk-device-input-mode device) mode))

(export 'gdk-device-set-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_mode ()
;;;
;;; GdkInputMode gdk_device_get_mode (GdkDevice *device);
;;;
;;; Determines the mode of the device.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     a GdkInputSource
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-mode))

(defun gdk-device-get-mode (device)
  (gdk-device-input-mode device))

(export 'gdk-device-get-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_key ()
;;;
;;; void gdk_device_set_key (GdkDevice *device,
;;;                          guint index_,
;;;                          guint keyval,
;;;                          GdkModifierType modifiers);
;;;
;;; Specifies the X key event to generate when a macro button of a device is
;;; pressed.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; index_ :
;;;     the index of the macro button to set
;;;
;;; keyval :
;;;     the keyval to generate
;;;
;;; modifiers :
;;;     the modifiers to set
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_set_key" gdk-device-set-key) :void
  (device (g-object gdk-device))
  (index :uint)
  (keyval :uint)
  (modifiers gdk-modifier-type))

(export 'gdk-device-set-key)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_key ()
;;;
;;; gboolean gdk_device_get_key (GdkDevice *device,
;;;                              guint index_,
;;;                              guint *keyval,
;;;                              GdkModifierType *modifiers);
;;;
;;; If index_ has a valid keyval, this function will return TRUE and fill in
;;; keyval and modifiers with the keyval settings.
;;;
;;; device :
;;;     a GdkDevice.
;;;
;;; index_ :
;;;     the index of the macro button to get.
;;;
;;; keyval :
;;;     return value for the keyval
;;;
;;; modifiers :
;;;     return value for modifiers
;;;
;;; Returns :
;;;     TRUE if keyval is set for index.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_key" %gdk-device-get-key) :void
  (device (g-object gdk-device))
  (index :int)
  (keyval (:pointer :uint))
  (modifiers (:pointer gdk-modifier-type)))

(defun gdk-device-get-key (device index)
  (with-foreign-objects ((keyval :int)
                         (modifiers 'gdk-modifier-type))
    (when (%gdk-device-get-key device index keyval modifiers)
      (values keyval
              modifiers))))

(export 'gdk-device-get-key)

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_axis_use ()
;;;
;;; void gdk_device_set_axis_use (GdkDevice *device,
;;;                               guint index_,
;;;                               GdkAxisUse use);
;;;
;;; Specifies how an axis of a device is used.
;;;
;;; device :
;;;     a pointer GdkDevice
;;;
;;; index_ :
;;;     the index of the axis
;;;
;;; use :
;;;     specifies how the axis is used
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_set_axis_use" gdk-device-set-axis-use) :void
  (device (g-object gdk-device))
  (index :uint)
  (use gdk-axis-use))

(export 'gdk-device-set-axis-use)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_use ()
;;;
;;; GdkAxisUse gdk_device_get_axis_use (GdkDevice *device, guint index_);
;;;
;;; Returns the axis use for index_.
;;;
;;; device :
;;;     a pointer GdkDevice.
;;;
;;; index_ :
;;;     the index of the axis.
;;;
;;; Returns :
;;;     a GdkAxisUse specifying how the axis is used.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_axis_use" gdk-device-get-axis-use) gdk-axis-use
  (device (g-object gdk-device))
  (index :uint))

(export 'gdk-device-get-axis-use)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_associated_device ()
;;;
;;; GdkDevice * gdk_device_get_associated_device (GdkDevice *device);
;;;
;;; Returns the associated device to device, if device is of type
;;; GDK_DEVICE_TYPE_MASTER, it will return the paired pointer or keyboard.
;;;
;;; If device is of type GDK_DEVICE_TYPE_SLAVE, it will return the master device
;;; to which device is attached to.
;;;
;;; If device is of type GDK_DEVICE_TYPE_FLOATING, NULL will be returned, as
;;; there is no associated device.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     The associated device, or NULL.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-associated-device))

(defun gdk-device-get-associated-device (device)
  (gdk-device-associated-device device))

(export 'gdk-device-get-associated-device)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_slave_devices ()
;;;
;;; GList * gdk_device_list_slave_devices (GdkDevice *device);
;;;
;;; If the device is of type GDK_DEVICE_TYPE_MASTER, it will return the list of
;;; slave devices attached to it, otherwise it will return NULL
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     the list of slave devices, or NULL. The list must be freed with
;;;     g_list_free(), the contents of the list are owned by GTK+ and should not
;;;     be freed.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_list_slave_devices" gdk-device-list-slave-devices)
    (g-list (g-object gdk-device))
  (device (g-object gdk-device)))

(export 'gdk-device-list-slave-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_device_type ()
;;;
;;; GdkDeviceType gdk_device_get_device_type (GdkDevice *device);
;;;
;;; Returns the device type for device.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     the GdkDeviceType for device.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-device-type))

(defun gdk-device-get-device-type (device)
  (gdk-device-type device))

(export 'gdk-device-get-device-type)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_display ()
;;;
;;; GdkDisplay * gdk_device_get_display (GdkDevice *device);
;;;
;;; Returns the GdkDisplay to which device pertains.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     a GdkDisplay. This memory is owned by GTK+, and must not be freed or
;;;     unreffed.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-display))

(defun gdk-device-get-display (device)
  (gdk-device-display device))

(export 'gdk-device-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_has_cursor ()
;;;
;;; gboolean gdk_device_get_has_cursor (GdkDevice *device);
;;;
;;; Determines whether the pointer follows device motion.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     TRUE if the pointer follows device motion
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-has-cursor))

(defun gdk-device-get-has-cursor (device)
  (gdk-device-has-cursor device))

(export 'gdk-device-get-has-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_n_axes ()
;;;
;;; gint gdk_device_get_n_axes (GdkDevice *device);
;;;
;;; Returns the number of axes the device currently has.
;;;
;;; device :
;;;     a pointer GdkDevice
;;;
;;; Returns :
;;;     the number of axes.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-n-axes))

(defun gdk-device-get-n-axes (device)
  (gdk-device-n-axes device))

(export 'gdk-device-get-n-axes)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_n_keys ()
;;;
;;; gint gdk_device_get_n_keys (GdkDevice *device);
;;;
;;; Returns the number of keys the device currently has.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     the number of keys.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_n_keys" gdk-device-get-n-keys) :int
  (device (g-object gdk-device)))

(export 'gdk-device-get-n-keys)

;;; ----------------------------------------------------------------------------
;;; gdk_device_warp ()
;;;
;;; void gdk_device_warp (GdkDevice *device,
;;;                       GdkScreen *screen,
;;;                       gint x,
;;;                       gint y);
;;;
;;; Warps device in display to the point x,y on the screen screen, unless the
;;; device is confined to a window by a grab, in which case it will be moved as
;;; far as allowed by the grab. Warping the pointer creates events as if the
;;; user had moved the mouse instantaneously to the destination.
;;;
;;; Note that the pointer should normally be under the control of the user. This
;;; function was added to cover some rare use cases like keyboard navigation
;;; support for the color picker in the GtkColorSelectionDialog.
;;;
;;; device :
;;;     the device to warp.
;;;
;;; screen :
;;;     the screen to warp device to.
;;;
;;; x :
;;;     the X coordinate of the destination.
;;;
;;; y :
;;;     the Y coordinate of the destination.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_warp" gdk-device-warp) :void
  (device (g-object gdk-device))
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-device-warp)

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabStatus
;;;
;;; typedef enum {
;;;   GDK_GRAB_SUCCESS         = 0,
;;;   GDK_GRAB_ALREADY_GRABBED = 1,
;;;   GDK_GRAB_INVALID_TIME    = 2,
;;;   GDK_GRAB_NOT_VIEWABLE    = 3,
;;;   GDK_GRAB_FROZEN          = 4
;;; } GdkGrabStatus;
;;;
;;; Returned by gdk_pointer_grab() and gdk_keyboard_grab() to indicate success
;;; or the reason for the failure of the grab attempt.
;;;
;;; GDK_GRAB_SUCCESS
;;;     the resource was successfully grabbed.
;;;
;;; GDK_GRAB_ALREADY_GRABBED
;;;     the resource is actively grabbed by another client.
;;;
;;; GDK_GRAB_INVALID_TIME
;;;     the resource was grabbed more recently than the specified time.
;;;
;;; GDK_GRAB_NOT_VIEWABLE
;;;     the grab window or the confine_to window are not viewable.
;;;
;;; GDK_GRAB_FROZEN
;;;     the resource is frozen by an active grab of another client.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGrabStatus" gdk-grab-status
  (:export t
   :type-initializer "gdk_grab_status_get_type")
  :success
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen)

;;; ----------------------------------------------------------------------------
;;; gdk_device_grab ()
;;;
;;; GdkGrabStatus gdk_device_grab (GdkDevice *device,
;;;                                GdkWindow *window,
;;;                                GdkGrabOwnership grab_ownership,
;;;                                gboolean owner_events,
;;;                                GdkEventMask event_mask,
;;;                                GdkCursor *cursor,
;;;                                guint32 time_);
;;;
;;; Grabs the device so that all events coming from this device are passed to
;;; this application until the device is ungrabbed with gdk_device_ungrab(), or
;;; the window becomes unviewable. This overrides any previous grab on the
;;; device by this client.
;;;
;;; Device grabs are used for operations which need complete control over the
;;; given device events (either pointer or keyboard). For example in GTK+ this
;;; is used for Drag and Drop operations, popup menus and such.
;;;
;;; Note that if the event mask of an X window has selected both button press
;;; and button release events, then a button press event will cause an automatic
;;; pointer grab until the button is released. X does this automatically since
;;; most applications expect to receive button press and release events in
;;; pairs. It is equivalent to a pointer grab on the window with owner_events
;;; set to TRUE.
;;;
;;; If you set up anything at the time you take the grab that needs to be
;;; cleaned up when the grab ends, you should handle the GdkEventGrabBroken
;;; events that are emitted when the grab ends unvoluntarily.
;;;
;;; device :
;;;     a GdkDevice. To get the device you can use
;;;     gtk_get_current_event_device() or gdk_event_get_device() if the grab is
;;;     in reaction to an event. Also, you can use
;;;     gdk_device_manager_get_client_pointer() but only in code that isn't
;;;     triggered by a GdkEvent and there aren't other means to get a meaningful
;;;     GdkDevice to operate on.
;;;
;;; window :
;;;     the GdkWindow which will own the grab (the grab window)
;;;
;;; grab_ownership :
;;;     specifies the grab ownership.
;;;
;;; owner_events :
;;;     if FALSE then all device events are reported with respect to window and
;;;     are only reported if selected by event_mask. If TRUE then pointer events
;;;     for this application are reported as normal, but pointer events outside
;;;     this application are reported with respect to window and only if
;;;     selected by event_mask. In either mode, unreported events are discarded.
;;;
;;; event_mask :
;;;     specifies the event mask, which is used in accordance with owner_events.
;;;
;;; cursor :
;;;     the cursor to display while the grab is active if the device is a
;;;     pointer. If this is NULL then the normal cursors are used for window and
;;;     its descendants, and the cursor for window is used elsewhere.
;;;
;;; time_ :
;;;     the timestamp of the event which led to this pointer grab. This usually
;;;     comes from the GdkEvent struct, though GDK_CURRENT_TIME can be used if
;;;     the time isn't known.
;;;
;;; Returns :
;;;     GDK_GRAB_SUCCESS if the grab was successful.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_grab" gdk-device-grab) gdk-grab-status
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (grab-ownership gdk-grab-ownership)
  (owner-events :boolean)
  (event-mask gdk-event-mask)
  (cursor (g-boxed-foreign gdk-cursor))
  (time :uint32))

(export 'gdk-device-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_ungrab ()
;;;
;;; void gdk_device_ungrab (GdkDevice *device, guint32 time_);
;;;
;;; Release any grab on device.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; time_ :
;;;     a timestap (e.g. GDK_CURRENT_TIME).
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_ungrab" gdk-device-ungrab) :void
  (device (g-object gdk-device))
  (time :uint32))

(export 'gdk-device-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_state ()
;;;
;;; void gdk_device_get_state (GdkDevice *device,
;;;                            GdkWindow *window,
;;;                            gdouble *axes,
;;;                            GdkModifierType *mask);
;;;
;;; Gets the current state of a pointer device relative to window. As a slave
;;; device coordinates are those of its master pointer, This function may not be
;;; called on devices of type GDK_DEVICE_TYPE_SLAVE, unless there is an ongoing
;;; grab on them, see gdk_device_grab().
;;;
;;; device :
;;;     a GdkDevice.
;;;
;;; window :
;;;     a GdkWindow.
;;;
;;; axes :
;;;     an array of doubles to store the values of the axes of device in, or
;;;     NULL.
;;;
;;; mask :
;;;     location to store the modifiers, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_state" %gdk-device-get-state) :void
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (axes (:pointer :double))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-device-get-state (device window)
  (with-foreign-objects ((axes :double (gdk-device-n-axes device))
                         (mask 'gdk-modifier-type))
    (%gdk-device-get-state device window axes mask)
    (values (iter (for i from 0 below (gdk-device-get-n-axes device))
                  (collect (mem-aref axes :double i)))
            (mem-ref mask 'gdk-modifier-type))))

(export 'gdk-device-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_position ()
;;;
;;; void gdk_device_get_position (GdkDevice *device,
;;;                               GdkScreen **screen,
;;;                               gint *x,
;;;                               gint *y);
;;;
;;; Gets the current location of device. As a slave device coordinates are those
;;; of its master pointer. This function may not be called on devices of type
;;; GDK_DEVICE_TYPE_SLAVE, unless there is an ongoing grab on them, see
;;; gdk_device_grab().
;;;
;;; device :
;;;     pointer device to query status about.
;;;
;;; screen :
;;;     location to store the GdkScreen the device is on, or NULL
;;;
;;; x :
;;;     location to store root window X coordinate of device, or NULL
;;;
;;; y :
;;;     location to store root window Y coordinate of device, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_position" %gdk-device-get-position) :void
  (display (g-object gdk-device))
  (screen :pointer)
  (x :pointer)
  (y :pointer))

(defun gdk-device-get-position (device)
  (with-foreign-objects ((screen :pointer)
                         (x :int)
                         (y :int))
    (%gdk-device-get-position device screen x y)
    (values (mem-ref screen '(g-object gdk-screen))
            (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-device-get-position)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_window_at_position ()
;;;
;;; GdkWindow * gdk_device_get_window_at_position (GdkDevice *device,
;;;                                                gint *win_x,
;;;                                                gint *win_y);
;;;
;;; Obtains the window underneath device, returning the location of the device
;;; in win_x and win_y. Returns NULL if the window tree under device is not
;;; known to GDK (for example, belongs to another application).
;;;
;;; As a slave device coordinates are those of its master pointer, This function
;;; may not be called on devices of type GDK_DEVICE_TYPE_SLAVE, unless there is
;;; an ongoing grab on them, see gdk_device_grab().
;;;
;;; device :
;;;     pointer GdkDevice to query info to.
;;;
;;; win_x :
;;;     return location for the X coordinate of the device location, relative to
;;;     the window origin, or NULL
;;;
;;; win_y :
;;;     return location for the Y coordinate of the device location, relative to
;;;     the window origin, or NULL
;;;
;;; Returns :
;;;     the GdkWindow under the device position, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_window_at_position"
          %gdk-device-get-window-at-position) (g-object gdk-window)
  (device (g-object gdk-device))
  (win-x :pointer)
  (win-y :pointer))

(defun gdk-device-get-window-at-position (device)
  (with-foreign-objects ((win-x :int) (win-y :int))
    (let ((win (%gdk-device-get-window-at-position device win-x win-y)))
      (values win
              (mem-ref win-x :int)
              (mem-ref win-y :int)))))

(export 'gdk-device-get-window-at-position)

;;; ----------------------------------------------------------------------------
;;; struct GdkTimeCoord
;;;
;;; struct GdkTimeCoord {
;;;   guint32 time;
;;;   gdouble axes[GDK_MAX_TIMECOORD_AXES];
;;; };
;;;
;;; The GdkTimeCoord structure stores a single event in a motion history.
;;;
;;; guint32 time;
;;;     The timestamp for this event.
;;;
;;; gdouble axes[GDK_MAX_TIMECOORD_AXES];
;;;     the values of the device's axes.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-time-coord "GdkTimeCoord"
  (time :uint32)
  (axes :double :count 128))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_history ()
;;;
;;; gboolean gdk_device_get_history (GdkDevice *device,
;;;                                  GdkWindow *window,
;;;                                  guint32 start,
;;;                                  guint32 stop,
;;;                                  GdkTimeCoord ***events,
;;;                                  gint *n_events);
;;;
;;; Obtains the motion history for a pointer device; given a starting and ending
;;; timestamp, return all events in the motion history for the device in the
;;; given range of time. Some windowing systems do not support motion history,
;;; in which case, FALSE will be returned. (This is not distinguishable from the
;;; case where motion history is supported and no events were found.)
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; window :
;;;     the window with respect to which which the event coordinates will be
;;;     reported
;;;
;;; start :
;;;     starting timestamp for range of events to return
;;;
;;; stop :
;;;     ending timestamp for the range of events to return
;;;
;;; events :
;;;     location to store a newly-allocated array of GdkTimeCoord, or NULL
;;;
;;; n_events :
;;;     location to store the length of events, or NULL
;;;
;;; Returns :
;;;     TRUE if the windowing system supports motion history and at least one
;;;     event was found.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_history" %gdk-device-get-history) :boolean
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (start :uint32)
  (stop :uint32)
  (events (:pointer (:pointer (:pointer gdk-time-coord-cstruct))))
  (n-events (:pointer :int)))

(defun gdk-device-get-history (device window start stop)
  (with-foreign-objects ((events :pointer) (n-events :int))
    (when (%gdk-device-get-history device window start stop events n-events)
      (prog1
        (iter (with events-ar = (mem-ref events :pointer))
              (for i from 0 below (mem-ref n-events :int))
              (for coord = (mem-aref events-ar
                                     '(g-boxed-foreign gdk-time-coord)
                                     i))
              (collect coord))
        (gdk-device-free-history (mem-ref events :pointer)
                                 (mem-ref n-events :int))))))

(export 'gdk-device-get-history)

;;; ----------------------------------------------------------------------------
;;; gdk_device_free_history ()
;;;
;;; void gdk_device_free_history (GdkTimeCoord **events, gint n_events);
;;;
;;; Frees an array of GdkTimeCoord that was returned by
;;; gdk_device_get_history().
;;;
;;; events :
;;;     an array of GdkTimeCoord.
;;;
;;; n_events :
;;;     the length of the array.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_free_history" gdk-device-free-history) :void
  (events (:pointer (:pointer gdk-time-coord-cstruct)))
  (n-events :int))

(export 'gdk-device-free-history)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis ()
;;;
;;; gboolean gdk_device_get_axis (GdkDevice *device,
;;;                               gdouble *axes,
;;;                               GdkAxisUse use,
;;;                               gdouble *value);
;;;
;;; Interprets an array of double as axis values for a given device, and locates
;;; the value in the array for a given axis use.
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; axes :
;;;     pointer to an array of axes
;;;
;;; use :
;;;     the use to look for
;;;
;;; value :
;;;     location to store the found value
;;;
;;; Returns :
;;;     TRUE if the given axis use was found, otherwise FALSE
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_axis" %gdk-device-get-axis) :boolean
  (device (g-object gdk-device))
  (axes (:pointer :double))
  (use gdk-axis-use)
  (value (:pointer :double)))

(defun gdk-device-get-axis (device axes axis-use)
  (assert (= (gdk-device-n-axes device) (length axes)))
  (with-foreign-objects ((axes-ar :double (gdk-device-n-axes device))
                         (value :double))
    (let ((i 0))
      (map nil
           (lambda (v)
             (setf (mem-aref axes-ar :double i) v)
             (incf i))
           axes))
    (when (%gdk-device-get-axis device axes-ar axis-use value)
      (mem-ref value :double))))

(export 'gdk-device-get-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_axes ()
;;;
;;; GList * gdk_device_list_axes (GdkDevice *device);
;;;
;;; Returns a GList of GdkAtoms, containing the labels for the axes that device
;;; currently has.
;;;
;;; device :
;;;     a pointer GdkDevice
;;;
;;; Returns :
;;;     A GList of GdkAtoms, free with g_list_free().
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_value ()
;;;
;;; gboolean gdk_device_get_axis_value (GdkDevice *device,
;;;                                     gdouble *axes,
;;;                                     GdkAtom axis_label,
;;;                                     gdouble *value);
;;;
;;; Interprets an array of double as axis values for a given device, and locates
;;; the value in the array for a given axis label, as returned by
;;; gdk_device_list_axes()
;;;
;;; device :
;;;     a pointer GdkDevice.
;;;
;;; axes :
;;;     pointer to an array of axes
;;;
;;; axis_label :
;;;     GdkAtom with the axis label.
;;;
;;; value :
;;;     location to store the found value.
;;;
;;; Returns :
;;;     TRUE if the given axis use was found, otherwise FALSE.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gdk.device.lisp --------------------------------------------
