;;; ----------------------------------------------------------------------------
;;; gdk.device.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gdk)

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-device 'type)
 "@version{2013-6-21}
  @begin{short}
    The @sym{gdk-device} object represents a single input device, such as a
    keyboard, a mouse, a touchpad, etc.
  @end{short}

  See the @class{gdk-device-manager} documentation for more information about
  the various kinds of master and slave devices, and their relationships.
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
  @end{dictionary}
  @see-slot{gdk-device-associated-device}
  @see-slot{gdk-device-device-manager}
  @see-slot{gdk-device-display}
  @see-slot{gdk-device-has-cursor}
  @see-slot{gdk-device-input-mode}
  @see-slot{gdk-device-input-source}
  @see-slot{gdk-device-n-axes}
  @see-slot{gdk-device-name}
  @see-slot{gdk-device-type}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;;-----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "associated-device"
                                               'gdk-device) 't)
 "The @code{\"associated-device\"} property of type @sym{gdk-device} (Read)@br{}
  Associated pointer or keyboard with this device, if any. Devices of type
  @code{:master} always come in keyboard/pointer pairs. Other device types will
  have a @code{nil} associated device. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "device-manager" 'gdk-device) 't)
 "The @code{\"device-manager\"} property of type @class{gdk-device-manager}
  (Read / Write / Construct) @br{}
  The @class{gdk-device-manager} the @sym{gdk-device} pertains to. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display" 'gdk-device) 't)
 "The @code{\"display\"} property of type @class{gdk-display}
  (Read / Write / Construct) @br{}
  The @class{gdk-display} the @sym{gdk-device} pertains to. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-cursor" 'gdk-device) 't)
 "The @code{\"has-cursor\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the device is represented by a cursor on the screen. Devices of type
  @code{:master} will have @em{true} here. @br{}
  Default value: @code{nil} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-mode" 'gdk-device) 't)
 "The @code{\"input-mode\"} property of type @symbol{gdk-input-mode}
  (Read / Write) @br{}
  Input mode for the device. @br{}
  Default value: @code{:disabled} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-source" 'gdk-device) 't)
 "The @code{\"input-source\"} property of type @symbol{gdk-input-source}
  (Read / Write / Construct) @br{}
  Source type for the device. @br{}
  Default value: @code{:mouse} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-axes" 'gdk-device) 't)
 "The @code{\"n-axes\"} property of type @code{:uint} (Read) @br{}
  Number of axes in the device. @br{}
  Default value: 0 @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gdk-device) 't)
 "The @code{\"name\"} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The device name. @br{}
  Default value: @code{nil} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "type" 'gdk-device) 't)
 "The @code{\"type\"} property of type @symbol{gdk-device-type}
  (Read / Write / Construct) @br{}
  Device role in the device manager. @br{}
  Default value: @code{:master} @br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-associated-device atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-associated-device 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"associated-device\"} of the @class{gdk-device}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-device-manager atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-device-manager 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"device-manager\"} of the @class{gdk-device}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-display 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"display\"} of the @class{gdk-device}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-has-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-has-cursor 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"has-cursor\"} of the @class{gdk-device}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-input-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-input-mode 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"input-mode\"} of the @class{gdk-device}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-input-source atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-input-source 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"input-source\"} of the @class{gdk-device}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-n-axes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-n-axes 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"n-axes\"} of the @class{gdk-device} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-name 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"name\"} of the @class{gdk-device} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-type 'function)
 "@version{2013-3-22}
  Accessor of the slot @code{\"type\"} of the @class{gdk-device} class.")

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-input-source atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-input-source atdoc:*external-symbols*)
 "@version{2013-6-21}
  @begin{short}
    An enumeration describing the type of an input device in general terms.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkInputSource\" gdk-input-source
  (:export t
   :type-initializer \"gdk_input_source_get_type\")
  (:mouse 0)
  (:pen 1)
  (:eraser 2)
  (:cursor 3)
  (:keyboard 4)
  (:touchscreen 5)
  (:touchpad 6))
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
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum GdkInputMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInputMode" gdk-input-mode
  (:export t
   :type-initializer "gdk_input_mode_get_type")
  (:disabled 0)
  (:screen 1)
  (:window 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-input-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-input-mode atdoc:*external-symbols*)
 "@version{2013-6-21}
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
    @entry[:screen]{The device is enabled. The device's coordinate space maps to
      the entire screen.}
    @entry[:window]{The device is enabled. The device's coordinate space is
      mapped to a single window. The manner in which this window is chosen is
      undefined, but it will typically be the same way in which the focus window
      for key events is determined.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum GdkAxisUse
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-axis-use atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-axis-use atdoc:*external-symbols*)
 "@version{2013-6-21}
  @begin{short}
    An enumeration describing the way in which a device axis (valuator) maps
    onto the predefined valuator types that GTK+ understands.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkAxisUse\" gdk-axis-use
  (:export t
   :type-initializer \"gdk_axis_use_get_type\")
  (:ignore 0)
  (:x 1)
  (:y 2)
  (:pressure 3)
  (:xtilt 4)
  (:ytilt 5)
  (:wheel 6)
  (:last 7))
  @end{pre}
  @begin[code]{table}
    @entry[:ignore]{The axis is ignored.}
    @entry[:x]{The axis is used as the x axis.}
    @entry[:y]{The axis is used as the y axis.}
    @entry[:pressure]{The axis is used for pressure information.}
    @entry[:xtilt]{The axis is used for x tilt information.}
    @entry[:ytilt]{The axis is used for y tilt information.}
    @entry[:wheel]{The axis is used for wheel information.}
    @entry[:last]{A constant equal to the numerically highest axis value.}
  @end{table}")

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
 "@version{2013-6-21}
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
    @entry[:floating]{Device is a physical device, currently not attached to any
    virtual device.}
  @end{table}")

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
 "@version{2013-6-21}
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
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-name))

(defun gdk-device-get-name (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{A name.}
  @short{Determines the name of the device.}

  Since 2.20"
  (gdk-device-name device))

(export 'gdk-device-get-name)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_source ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-source))

(defun gdk-device-get-source (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{A @symbol{gdk-input-source}.}
  @short{Determines the type of the device.}

  Since 2.20"
  (gdk-device-input-source device))

(export 'gdk-device-get-source)

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-set-mode))

(defun gdk-device-set-mode (device mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[mode]{the input mode}
  @return{@em{True} if the mode was successfully changed.}
  Sets a the mode of an input device. The mode controls if the device is
  active and whether the device's range is mapped to the entire screen or to a
  single window."
  (setf (gdk-device-input-mode device) mode))

(export 'gdk-device-set-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-mode))

(defun gdk-device-get-mode (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{A @symbol{gdk-input-source}.}
  @short{Determines the mode of the device.}

  Since 2.20"
  (gdk-device-input-mode device))

(export 'gdk-device-get-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_set_key" gdk-device-set-key) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[index]{the index of the macro button to set}
  @argument[keyval]{the keyval to generate}
  @argument[modifiers]{the modifiers to set}
  Specifies the X key event to generate when a macro button of a device is
  pressed."
  (device (g-object gdk-device))
  (index :uint)
  (keyval :uint)
  (modifiers gdk-modifier-type))

(export 'gdk-device-set-key)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_key" %gdk-device-get-key) :void
  (device (g-object gdk-device))
  (index :int)
  (keyval (:pointer :uint))
  (modifiers (:pointer gdk-modifier-type)))

(defun gdk-device-get-key (device index)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[index]{the index of the macro button to get}
  @begin{return}
    @code{keyval} -- value for the keyval @br{}
    @code{modifiers} -- value for modifiers
  @end{return}
  @begin{short}
    If @arg{index} has a valid keyval, this function will return @arg{keyval}
    and @arg{modifiers} with the keyval settings.
  @end{short}

  Since 2.20"
  (with-foreign-objects ((keyval :int)
                         (modifiers 'gdk-modifier-type))
    (when (%gdk-device-get-key device index keyval modifiers)
      (values keyval
              modifiers))))

(export 'gdk-device-get-key)

;;; ----------------------------------------------------------------------------
;;; gdk_device_set_axis_use ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_set_axis_use" gdk-device-set-axis-use) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a pointer @class{gdk-device} object}
  @argument[index]{the index of the axis}
  @argument[use]{specifies how the axis is used}
  Specifies how an axis of a device is used."
  (device (g-object gdk-device))
  (index :uint)
  (use gdk-axis-use))

(export 'gdk-device-set-axis-use)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_use ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_axis_use" gdk-device-get-axis-use) gdk-axis-use
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a pointer @class{gdk-device} object}
  @argument[index]{the index of the axis}
  @return{A @symbol{gdk-axis-use} specifying how the axis is used.}
  @short{Returns the axis use for @arg{index}.}

  Since 2.20"
  (device (g-object gdk-device))
  (index :uint))

(export 'gdk-device-get-axis-use)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_associated_device ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-associated-device))

(defun gdk-device-get-associated-device (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{The associated device, or @code{nil}.}
  @begin{short}
    Returns the associated device to device, if device is of type
    @code{:master}, it will return the paired pointer or keyboard.
  @end{short}

  If device is of type @code{:slave}, it will return the master device to which
  device is attached to.

  If device is of type @code{:floating}, @code{nil} will be returned, as
  there is no associated device.

  Since 3.0"
  (gdk-device-associated-device device))

(export 'gdk-device-get-associated-device)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_slave_devices ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_list_slave_devices" gdk-device-list-slave-devices)
    (g-list (g-object gdk-device))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @begin{return}
    The list of slave devices, or @code{nil}.
  @end{return}
  If the device is of type @code{:master}, it will return the list of
  slave devices attached to it, otherwise it will return @code{nil}."
  (device (g-object gdk-device)))

(export 'gdk-device-list-slave-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_device_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-device-type))

(defun gdk-device-get-device-type (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{The @symbol{gdk-device-type} for @arg{device}.}
  @short{Returns the device type for @arg{device}.}

  Since 3.0"
  (gdk-device-type device))

(export 'gdk-device-get-device-type)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_display ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-display))

(defun gdk-device-get-display (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @begin{return}
    A @class{gdk-display} object.
  @end{return}
  @short{Returns the @class{gdk-display} to which @arg{device} pertains.}

  Since 3.0"
  (gdk-device-display device))

(export 'gdk-device-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_has_cursor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-has-cursor))

(defun gdk-device-get-has-cursor (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{@em{True} if the pointer follows device motion.}
  @short{Determines whether the pointer follows device motion.}

  Since 2.20"
  (gdk-device-has-cursor device))

(export 'gdk-device-get-has-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_n_axes ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-get-n-axes))

(defun gdk-device-get-n-axes (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a pointer @class{gdk-device} object}
  @return{The number of axes.}
  @short{Returns the number of axes the @arg{device} currently has.}

  Since 3.0"
  (gdk-device-n-axes device))

(export 'gdk-device-get-n-axes)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_n_keys ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_n_keys" gdk-device-get-n-keys) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @return{The number of keys.}
  @short{Returns the number of keys the @arg{device} currently has.}

  Since 2.24"
  (device (g-object gdk-device)))

(export 'gdk-device-get-n-keys)

;;; ----------------------------------------------------------------------------
;;; gdk_device_warp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_warp" gdk-device-warp) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{the device to warp}
  @argument[screen]{the screen to warp device to}
  @argument[x]{the x coordinate of the destination}
  @argument[y]{the y coordinate of the destination}
  @begin{short}
    Warps @arg{device} in display to the point x,y on the screen screen, unless
    the device is confined to a window by a grab, in which case it will be moved
    as far as allowed by the grab. Warping the pointer creates events as if the
    user had moved the mouse instantaneously to the destination.
  @end{short}

  Note that the pointer should normally be under the control of the user. This
  function was added to cover some rare use cases like keyboard navigation
  support for the color picker in the @class{gtk-color-selection-dialog}.

  Since 3.0"
  (device (g-object gdk-device))
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-device-warp)

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabStatus
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGrabStatus" gdk-grab-status
  (:export t
   :type-initializer "gdk_grab_status_get_type")
  :success
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-grab-status atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-grab-status atdoc:*external-symbols*)
 "@version{2013-6-21}
  @begin{short}
    Returned by the functions @fun{gdk-pointer-grab} and @fun{gdk-keyboard-grab}
    to indicate success or the reason for the failure of the grab attempt.
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
    @entry[:not-viewable]{The grab window or the @arg{confine-to} window are not
      viewable.}
    @entry[:frozen]{The resource is frozen by an active grab of another client.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gdk_device_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_grab" gdk-device-grab) gdk-grab-status
 #+cl-cffi-gtk-documentation
 "@version{2013-7-7}
  @argument[device]{a @class{gdk-device} object. To get the device you can use
    the functions @fun{gtk-get-current-event-device} or
    @fun{gdk-event-get-device} if the grab is in reaction to an event. Also, you
    can use the function @fun{gdk-device-manager-get-client-pointer} but only in
    code that is not triggered by a @class{gdk-event} and there are not other
    means to get a meaningful @class{gdk-device} to operate on.}
  @argument[window]{the @class{gdk-window} which will own the grab (the grab
    window)}
  @argument[grab-ownership]{specifies the grab ownership}
  @argument[owner-events]{if @code{nil} then all device events are reported with
    respect to window and are only reported if selected by @arg{event-mask}. I
    @em{true} then pointer events for this application are reported as normal,
    but pointer events outside this application are reported with respect to
    window and only if selected by @arg{event-mask}. In either mode, unreported
    events are discarded.}
  @argument[event-mask]{specifies the event mask, which is used in accordance
    with @arg{owner-events}.}
  @argument[cursor]{the cursor to display while the grab is active if the device
    is a pointer. If this is @code{nil} then the normal cursors are used for
    window and its descendants, and the cursor for window is used elsewhere.}
  @argument[time]{the timestamp of the event which led to this pointer grab.
    This usually comes from the @class{gdk-event} structure, though
    @var{+gdk-current-time+} can be used if the time is not known.}
  @return{@code{:sucess} if the grab was successful.}
  @begin{short}
    Grabs the device so that all events coming from this device are passed to
    this application until the device is ungrabbed with the function
    @fun{gdk-device-ungrab}, or the window becomes unviewable. This overrides
    any previous grab on the device by this client.
  @end{short}

  Device grabs are used for operations which need complete control over the
  given device events (either pointer or keyboard). For example in GTK+ this
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

  Since 3.0"
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
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[time]{a timestap (e. g. @var{+gkd-current-time+}}
  @short{Release any grab on device.}

  Since 3.0"
  (device (g-object gdk-device))
  (time :uint32))

(export 'gdk-device-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_state" %gdk-device-get-state) :void
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (axes (:pointer :double))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-device-get-state (device window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[window]{a @class{gdk-window} object}
  @return{@code{mask} -- the modifiers, or @code{nil}.}
  Gets the current state of a pointer device relative to window. As a slave
  device coordinates are those of its master pointer, This function may not be
  called on devices of type @code{:slave}, unless there is an ongoing grab on
  them, see the function @fun{gdk-device-grab}."
  (with-foreign-objects ((axes :double (gdk-device-n-axes device))
                         (mask 'gdk-modifier-type))
    (%gdk-device-get-state device window axes mask)
    (values (iter (for i from 0 below (gdk-device-get-n-axes device))
                  (collect (mem-aref axes :double i)))
            (mem-ref mask 'gdk-modifier-type))))

(export 'gdk-device-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_position" %gdk-device-get-position) :void
  (display (g-object gdk-device))
  (screen :pointer)
  (x :pointer)
  (y :pointer))

(defun gdk-device-get-position (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{pointer device to query status about}
  @begin{return}
    @code{screen} -- the @class{gdk-screen} the device is on, or @code{nil}@br{}
    @code{x} -- root window x coordinate of device, or @code{nil} @br{}
    @code{y} -- root window y coordinate of device, or @code{nil}
  @end{return}
  @begin{short}
    Gets the current location of device. As a slave device coordinates are those
    of its master pointer. This function may not be called on devices of type
    @code{:slave}, unless there is an ongoing grab on them, see the function
    @fun{gdk-device-grab}.
  @end{short}

  Since 3.0"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_window_at_position"
          %gdk-device-get-window-at-position) (g-object gdk-window)
  (device (g-object gdk-device))
  (win-x :pointer)
  (win-y :pointer))

(defun gdk-device-get-window-at-position (device)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{pointer @class{gdk-device} to query info to}
  @begin{return}
    @code{win-x} -- x coordinate of the device location, relative to the window
    origin, or @code{nil} @br{}
    @code{win-y} -- y coordinate of the device location, relative to the window
    origin, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the window underneath device, returning the location of the device
    in @arg{win-x} and @arg{win-y}. Returns @code{nil} if the window tree under
    @arg{device} is not known to GDK (for example, belongs to another
    application).
  @end{short}

  As a slave device coordinates are those of its master pointer, This function
  may not be called on devices of type @code{:slave}, unless there is an ongoing
  grab on them, see @fun{gdk-device-grab}.

  Since 3.0"
  (with-foreign-objects ((win-x :int) (win-y :int))
    (let ((win (%gdk-device-get-window-at-position device win-x win-y)))
      (values win
              (mem-ref win-x :int)
              (mem-ref win-y :int)))))

(export 'gdk-device-get-window-at-position)

;;; ----------------------------------------------------------------------------
;;; struct GdkTimeCoord
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-time-coord "GdkTimeCoord"
  (time :uint32)
  (axes :double :count 128))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-time-coord atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-time-coord 'type)
 "@version{2013-6-21}
  @begin{short}
    The @sym{gdk-time-coord} structure stores a single event in a motion
    history.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gdk-time-coord \"GdkTimeCoord\"
  (time :uint32)
  (axes :double :count 128))
  @end{pre}
  @begin[code]{table}
    @entry[time]{The timestamp for this event.}
    @entry[axes]{The values of the device's axes.}
  @end{table}
  @see-constructor{copy-gdk-time-coord}
  @see-constructor{make-gdk-time-coord}
  @see-slot{gdk-time-coord-time}
  @see-slot{gdk-time-coord-axes}")

(export (boxed-related-symbols 'gdk-time-coord))

;;; --- copy-gdk-time-coord ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-time-coord 'function)
 "@version{2013-4-5}
  @argument[instance]{a @class{gdk-time-coord} struct}
  Copy constructor of a @class{gdk-time-coord} struct.")

;;; --- make-gdk-time-coord ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-time-coord 'function)
 "@version{2013-4-5}
  @argument[time]{The timestamp for this event.}
  @argument[axes]{the values of the device's axes.}
  @begin{short}
    Creates a @class{gdk-time-coord} struct.
  @end{short}")

;;; --- gdk-time-coord-time ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-time-coord-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-time-coord-time 'function)
 "@version{2013-4-5}
  Accessor of the slot @code{time} of the @class{gdk-time-coord} struct.")

;;; --- gdk-time-coord-axes ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-time-coord-axes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-time-coord-axes 'function)
 "@version{2013-4-5}
  Accessor of the slot @code{axes} of the @class{gdk-time-coord} struct.")

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_history ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_history" %gdk-device-get-history) :boolean
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (start :uint32)
  (stop :uint32)
  (events (:pointer (:pointer (:pointer gdk-time-coord-cstruct))))
  (n-events (:pointer :int)))

(defun gdk-device-get-history (device window start stop)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[window]{the window with respect to which which the event coordinates
    will be reported}
  @argument[start]{starting timestamp for range of events to return}
  @argument[stop]{ending timestamp for the range of events to return}
  @begin{return}
    A list of @symbol{gdk-time-coord} if the windowing system supports motion
    history and at least one event was found, or @code{nil}.
  @end{return}
  Obtains the motion history for a pointer device; given a starting and ending
  timestamp, return all events in the motion history for the device in the
  given range of time. Some windowing systems do not support motion history,
  in which case, @code{nil} will be returned. (This is not distinguishable from
  the case where motion history is supported and no events were found.)"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_free_history" gdk-device-free-history) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[events]{an array of @symbol{gdk-time-coord}}
  @argument[n-events]{the length of the array}
  Frees an array of @symbol{gdk-time-coord} that was returned by the function
  @fun{gdk-device-get-history}.
  @see-function{gdk-device-get-history}"
  (events (:pointer (:pointer gdk-time-coord-cstruct)))
  (n-events :int))

(export 'gdk-device-free-history)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_get_axis" %gdk-device-get-axis) :boolean
  (device (g-object gdk-device))
  (axes (:pointer :double))
  (use gdk-axis-use)
  (value (:pointer :double)))

(defun gdk-device-get-axis (device axes axis-use)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[device]{a @class{gdk-device} object}
  @argument[axes]{pointer to an array of axes}
  @argument[use]{the use to look for}
  @begin{return}
    @code{value} -- the found value, otherwise @code{nil}
  @end{return}
  Interprets an array of double as axis values for a given device, and locates
  the value in the array for a given axis use."
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
