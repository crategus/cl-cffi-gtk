;;; ----------------------------------------------------------------------------
;;; gdk.device-manager.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; GdkDeviceManager
;;;
;;;     Functions for handling input devices
;;;
;;; Types and Values
;;;
;;;     GdkDeviceManager
;;;
;;; Functions
;;;
;;;     gdk_disable_multidevice
;;;     gdk_device_manager_get_display                     Accessor
;;;     gdk_device_manager_list_devices
;;;     gdk_device_manager_get_client_pointer
;;;
;;; Properties
;;;
;;;     GdkDisplay* display    Read / Write / Construct Only
;;;
;;; Signals
;;;
;;;     void  device-added      Run Last
;;;     void  device-changed    Run Last
;;;     void  device-removed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDeviceManager
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDeviceManager
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDeviceManager" gdk-device-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_manager_get_type")
  ((display
    gdk-device-manager-display
    "display" "GdkDisplay" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-device-manager 'type)
 "@version{2021-12-11}
  @begin{short}
    In addition to a single pointer and keyboard for user interface input, GDK
    contains support for a variety of input devices, including graphics tablets,
    touchscreens and multiple pointers/keyboards interacting simultaneously
    with the user interface. Such input devices often have additional features,
    such as sub-pixel positioning information and additional device-dependent
    information.
  @end{short}

  In order to query the device hierarchy and be aware of changes in the device
  hierarchy, such as virtual devices being created or removed, or physical
  devices being plugged or unplugged, GDK provides the @sym{gdk-device-manager}
  object.

  By default, and if the platform supports it, GDK is aware of multiple
  keyboard/pointer pairs and multitouch devices. This behavior can be changed
  by calling the @fun{gdk-disable-multidevice} function before the
  @fun{gdk-display-open} function. There should rarely be a need to do that
  though, since GDK defaults to a compatibility mode in which it will emit just
  one enter/leave event pair for all devices on a window. To enable per-device
  enter/leave events and other multi-pointer interaction features, the
  @fun{gdk-window-support-multidevice} function must be called on
  @class{gdk-window} objects, or the @fun{gtk-widget-support-multidevice}
  function on widgets. See the @fun{gdk-window-support-multidevice}
  documentation for more information.

  On X11, multi-device support is implemented through XInput 2. Unless the
  @fun{gdk-disable-multidevice} function is called, the XInput 2
  @sym{gdk-device-manager} implementation will be used as the input source.
  Otherwise either the core or XInput 1 implementations will be used.

  For simple applications that do not have any special interest in input
  devices, the so-called client pointer provides a reasonable approximation to
  a simple setup with a single pointer and keyboard. The device that has been
  set as the client pointer can be accessed via the
  @fun{gdk-device-manager-client-pointer} function.

  Conceptually, in multidevice mode there are 2 device types. Virtual devices,
  or master devices. are represented by the pointer cursors and keyboard foci
  that are seen on the screen. Physical devices, or slave devices, represent
  the hardware that is controlling the virtual devices, and thus have no
  visible cursor on the screen.

  Virtual devices are always paired, so there is a keyboard device for every
  pointer device. Associations between devices may be inspected through the
  @fun{gdk-device-associated-device} function.

  There may be several virtual devices, and several physical devices could be
  controlling each of these virtual devices. Physical devices may also be
  \"floating\", which means they are not attached to any virtual device.

  @b{Example:} Master and slave devices
  @begin{pre}
 carlossacarino:~$ xinput list
 ⎡ Virtual core pointer                         id=2    [master pointer  (3)]
 ⎜   ↳ Virtual core XTEST pointer               id=4    [slave  pointer  (2)]
 ⎜   ↳ Wacom ISDv4 E6 Pen stylus                id=10   [slave  pointer  (2)]
 ⎜   ↳ Wacom ISDv4 E6 Finger touch              id=11   [slave  pointer  (2)]
 ⎜   ↳ SynPS/2 Synaptics TouchPad               id=13   [slave  pointer  (2)]
 ⎜   ↳ TPPS/2 IBM TrackPoint                    id=14   [slave  pointer  (2)]
 ⎜   ↳ Wacom ISDv4 E6 Pen eraser                id=16   [slave  pointer  (2)]
 ⎣ Virtual core keyboard                        id=3    [master keyboard (2)]
     ↳ Virtual core XTEST keyboard              id=5    [slave  keyboard (3)]
     ↳ Power Button                             id=6    [slave  keyboard (3)]
     ↳ Video Bus                                id=7    [slave  keyboard (3)]
     ↳ Sleep Button                             id=8    [slave  keyboard (3)]
     ↳ Integrated Camera                        id=9    [slave  keyboard (3)]
     ↳ AT Translated Set 2 keyboard             id=12   [slave  keyboard (3)]
     ↳ ThinkPad Extra Buttons                   id=15   [slave  keyboard (3)]
  @end{pre}
  By default, GDK will automatically listen for events coming from all master
  devices, setting the @class{gdk-device} object for all events coming from
  input devices. Events containing device information are @code{:motion-notify},
  @code{:button-press}, @code{:2button-press}, @code{:3button-press},
  @code{:button-release}, @code{:scroll}, @code{:key-press},
  @code{:key-release}, @code{:enter-notify}, @code{:leave-notify},
  @code{:focus-change}, @code{:proximity-in}, @code{:proximity-out},
  @code{:drag-enter}, @code{:drag-leave}, @code{:drag-motion},
  @code{:drag-status}, @code{:drop-start}, @code{:drop-finished} and
  @code{:grab-broken}. When dealing with an event on a master device, it is
  possible to get the source (slave) device that the event originated from via
  the @fun{gdk-event-source-device} function.

  In order to listen for events coming from devices other than a virtual device,
  the @fun{gdk-window-device-events} function must be called. Generally, this
  function can be used to modify the event mask for any given device.

  Input devices may also provide additional information besides x/y. For
  example, graphics tablets may also provide pressure and x/y tilt
  information. This information is device-dependent, and may be queried
  through the @fun{gdk-device-axis} function. In multidevice mode, virtual
  devices will change axes in order to always represent the physical device
  that is routing events through it. Whenever the physical device changes, the
  \"n-axes\" property will be notified, and the @fun{gdk-device-list-axes}
  function will return the new device axes.

  Devices may also have associated keys or macro buttons. Such keys can be
  globally set to map into normal X keyboard events. The mapping is set using
  the @fun{gdk-device-key} function.

  In GTK 3.20, a new @class{gdk-seat} object has been introduced that
  supersedes the @sym{gdk-device-manager} object and should be preferred in
  newly written code.
  @begin[Signal Details]{dictionary}
    @subheading{The \"device-added\" signal}
      @begin{pre}
 lambda (manager device)    :run-last
      @end{pre}
      The signal is emitted either when a new master pointer is created, or
      when a slave (hardware) input device is plugged in.
      @begin[code]{table}
        @entry[manager]{The @sym{gdk-device-manager} object on which the signal
          is emitted.}
        @entry[device]{The newly added @class{gdk-device} object.}
      @end{table}
    @subheading{The \"device-changed\" signal}
      @begin{pre}
 lambda (manager device)    :run-last
      @end{pre}
      The signal is emitted whenever a device has changed in the hierarchy,
      either slave devices being disconnected from their master device or
      connected to another one, or master devices being added or removed a
      slave device. If a slave device is detached from all master devices its
      @symbol{gdk-device-type} value will change to the @code{:floating} value,
      if it is attached, it will change to the @code{:slave} value.
      @begin[code]{table}
        @entry[manager]{The @sym{gdk-device-manager} object on which the signal
          is emitted.}
        @entry[device]{The @class{gdk-device} object that changed.}
      @end{table}
    @subheading{The \"device-removed\" signal}
      @begin{pre}
 lambda (manager device)    :run-last
      @end{pre}
      The signal is emitted either when a master pointer is removed, or when a
      slave (hardware) input device is unplugged.
      @begin[code]{table}
        @entry[manager]{The @sym{gdk-device-manager} object on which the signal
          is emitted.}
        @entry[device]{The just removed @class{gdk-device} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk-device-manager-display}
  @see-class{gdk-device}
  @see-class{gdk-seat}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display"
                                               'gdk-device-manager) 't)
 "The @code{display} property of type @code{gdk-display}
  (Read / Write / Construct) @br{}
  Display for the device manager.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-manager-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-manager-display 'function)
 "@version{2021-12-11}
  @syntax[]{(gdk-device-manager-display object) => display}
  @argument[object]{a @class{gdk-device-manager} object}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Accessor of the @slot[gdk-device-manager]{display} slot of the
    @class{gdk-device-manager} class.
  @end{short}

  The @sym{gdk-device-manager-display} slot access function returns the
  display to which the device manager is associated to.
  @see-class{gdk-device-manager}
  @see-class{gdk-display}")

;;; ----------------------------------------------------------------------------

#-windows
(define-g-object-class "GdkX11DeviceManagerCore" gdk-x11-device-manager-core
  (:superclass gdk-device-manager
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_device_manager_core_get_type")
  nil)

#-windows
(define-g-object-class "GdkX11DeviceManagerXI2" gdk-x11-device-manager-xi2
  (:superclass gdk-x11-device-manager-core
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_device_manager_xi2_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gdk_disable_multidevice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_disable_multidevice" gdk-disable-multidevice) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @begin{short}
    Disables multidevice support in GDK.
  @end{short}
  This call must happen prior to the @fun{gdk-display-open}, @code{gtk_init()},
  @code{gtk_init_with_args()} or @code{gtk_init_check()} functions in order to
  take effect.

  Most common GTK applications will not ever need to call this. Only
  applications that do mixed GDK/Xlib calls could want to disable multidevice
  support if such Xlib code deals with input devices in any way and does not
  observe the presence of XInput 2.
  @see-class{gdk-device-manager}
  @see-function{gdk-display-open}")

(export 'gdk-disable-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_list_devices ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_manager_list_devices" gdk-device-manager-list-devices)
    (g-list (g-object gdk-device) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[manager]{a @class{gdk-device-manager} object}
  @argument[type]{a @symbol{gdk-device-type} device type to get}
  @return{A list of @class{gdk-device} objects.}
  @begin{short}
    Returns the list of devices currently attached to the device manager.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-device-manager-list-devices} function has been deprecated since
    version 3.20 and should not be used in newly written code. Use the
    @fun{gdk-seat-pointer}, @fun{gdk-seat-keyboard} and @fun{gdk-seat-slaves}
    functions instead.
  @end{dictionary}
  @see-class{gdk-device-manager}
  @see-class{gdk-device}
  @see-symbol{gdk-device-type}
  @see-function{gdk-seat-pointer}
  @see-function{gdk-seat-keyboard}
  @see-function{gdk-seat-slaves}"
  (manager (g-object gdk-device-manager))
  (type gdk-device-type))

(export 'gdk-device-manager-list-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_get_client_pointer ()
;;; -> gdk-device-manager-client-pointer
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_manager_get_client_pointer"
           gdk-device-manager-client-pointer) (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[manager]{a @class{gdk-device-manager} object}
  @return{The @class{gdk-device} client pointer.}
  @begin{short}
    Returns the client pointer, that is, the master pointer that acts as the
    core pointer for this application.
  @end{short}
  In X11, window managers may change this depending on the interaction pattern
  under the presence of several pointers.

  You should use this function sheldomly, only in code that is not triggered by
  a @class{gdk-event} event and there are not other means to get a meaningful
  @class{gdk-device} object to operate on.
  @begin[Warning]{dictionary}
    The @sym{gdk-device-manager-client-pointer} function has been deprecated
    since version 3.20 and should not be used in newly written code. Use the
    @fun{gdk-seat-pointer} function instead.
  @end{dictionary}
  @see-class{gdk-device-manager}
  @see-class{gdk-device}
  @see-function{gdk-seat-pointer}"
  (manager (g-object gdk-device-manager)))

(export 'gdk-device-manager-client-pointer)

;;; --- End of file gdk.device-manager.lisp ------------------------------------
