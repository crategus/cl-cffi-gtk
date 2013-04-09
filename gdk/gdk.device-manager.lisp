;;; ----------------------------------------------------------------------------
;;; gdk.device-manager.lisp
;;; 
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;; 
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; Functions for handling input devices
;;;
;;; Synopsis
;;;
;;;     GdkDeviceManager
;;;
;;;     gdk_disable_multidevice
;;;     gdk_device_manager_get_display
;;;     gdk_device_manager_list_devices
;;;     gdk_device_manager_get_client_pointer
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkDeviceManager
;;;
;;;
;;; Signals
;;;
;;;   "device-added"                                  : Run Last
;;;   "device-changed"                                : Run Last
;;;   "device-removed"                                : Run Last
;;;
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-device-manager 'type)
 "@version{2013-4-7}
  @begin{short}
    In addition to a single pointer and keyboard for user interface input, GDK
    contains support for a variety of input devices, including graphics tablets,
    touchscreens and multiple pointers/keyboards interacting simultaneously with
    the user interface. Such input devices often have additional features, such
    as sub-pixel positioning information and additional device-dependent
    information.

    In order to query the device hierarchy and be aware of changes in the device
    hierarchy (such as virtual devices being created or removed, or physical
    devices being plugged or unplugged), GDK provides @sym{gdk-device-manager}.
  @end{short}

  By default, and if the platform supports it, GDK is aware of multiple
  keyboard/pointer pairs and multitouch devices. This behavior can be changed
  by calling @fun{gdk-disable-multidevice} before @fun{gdk-display-open}. There
  should rarely be a need to do that though, since GDK defaults to a
  compatibility mode in which it will emit just one enter/leave event pair for
  all devices on a window. To enable per-device enter/leave events and other
  multi-pointer interaction features, @fun{gdk-window-set-support-multidevice}
  must be called on @class{gdk-window} objects (or
  @fun{gtk-widget-set-support-multidevice} on widgets). window. See the
  @fun{gdk-window-set-support-multidevice} documentation for more information.

  On X11, multi-device support is implemented through XInput 2. Unless
  @fun{gdk-disable-multidevice} is called, the XInput 2 @sym{gdk-device-manager}
  implementation will be used as the input source. Otherwise either the core
  or XInput 1 implementations will be used.

  For simple applications that don't have any special interest in input
  devices, the so-called client pointer provides a reasonable approximation to
  a simple setup with a single pointer and keyboard. The device that has been
  set as the client pointer can be accessed via
  @fun{gdk-device-manager-get-client-pointer}.

  Conceptually, in multidevice mode there are 2 device types. Virtual devices
  (or master devices) are represented by the pointer cursors and keyboard foci
  that are seen on the screen. Physical devices (or slave devices) represent
  the hardware that is controlling the virtual devices, and thus have no
  visible cursor on the screen.

  Virtual devices are always paired, so there is a keyboard device for every
  pointer device. Associations between devices may be inspected through
  @fun{gdk-device-get-associated-device}.

  There may be several virtual devices, and several physical devices could be
  controlling each of these virtual devices. Physical devices may also be
  \"floating\", which means they are not attached to any virtual device.

  Example 3. Master and slave devices
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
  @fun{gdk-event-get-source-device}.

  In order to listen for events coming from devices other than a virtual
  device, @fun{gdk-window-set-device-events} must be called. Generally, this
  function can be used to modify the event mask for any given device.

  Input devices may also provide additional information besides X/Y. For
  example, graphics tablets may also provide pressure and X/Y tilt
  information. This information is device-dependent, and may be queried
  through @fun{gdk-device-get-axis}. In multidevice mode, virtual devices will
  change axes in order to always represent the physical device that is routing
  events through it. Whenever the physical device changes, the \"n-axes\"
  property will be notified, and @fun{gdk-device-list-axes} will return the new
  device axes.

  Devices may also have associated keys or macro buttons. Such keys can be
  globally set to map into normal X keyboard events. The mapping is set using
  @fun{gdk-device-set-key}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"device-added\" signal}
      @begin{pre}
 lambda (device-manager device)   : Run Last
      @end{pre}
      The \"device-added\" signal is emitted either when a new master pointer
      is created, or when a slave (Hardware) input device is plugged in.
      @begin[code]{table}
        @entry[device-manager]{The object on which the signal is emitted.}
        @entry[device]{The newly added @class{gdk-device} object.}
      @end{table}
    @subheading{The \"device-changed\" signal}
      @begin{pre}
 lambda (device-manager device)   : Run Last
      @end{pre}
      The \"device-changed\" signal is emitted whenever a device has changed in
      the hierarchy, either slave devices being disconnected from their master
      device or connected to another one, or master devices being added or
      removed a slave device.
      If a slave device is detached from all master devices
      (@fun{gdk-device-get-associated-device} returns NULL), its
      @symbol{gdk-device-type} will change to @code{:floating}, if it's
      attached, it will change to @code{:slave}.
      @begin[code]{table}
        @entry[device-manager]{The object on which the signal is emitted.}
        @entry[device]{The @class{gdk-device} object that changed.}
      @end{table}
    @subheading{The \"device-removed\" signal}
      @begin{pre}
 lambda (device-manager device)   : Run Last
      @end{pre}
      The \"device-removed\" signal is emitted either when a master pointer is
      removed, or when a slave (Hardware) input device is unplugged.
      @begin[code]{table}
        @entry[device-manager]{The object on which the signal is emitted.}
        @entry[device]{The just removed @class{gdk-device} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk-device-manager-display}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display"
                                               'gdk-device-manager) 't)
 "The @code{\"display\"} property of type @code{gdk-display}
  (Read / Write / Construct)@br{}
  Display for the device manager.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-device-manager-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-device-manager-display 'function)
 "@version{2013-4-7}
  Accessor of the slot @code{\"display\"} of the @class{gdk-device-manager}
  class.")

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
;;;
;;; void gdk_disable_multidevice (void);
;;;
;;; Disables multidevice support in GDK. This call must happen prior to
;;; gdk_display_open(), gtk_init(), gtk_init_with_args() or gtk_init_check() in
;;; order to take effect.
;;;
;;; Most common GTK+ applications won't ever need to call this. Only
;;; applications that do mixed GDK/Xlib calls could want to disable multidevice
;;; support if such Xlib code deals with input devices in any way and doesn't
;;; observe the presence of XInput 2.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_get_display ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-device-manager-get-display))

(defun gdk-device-manager-get-display (device-manager)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[device_manager]{a @class{gdk-device-manager} object}
  @begin{return}
    The @class{gdk-display} object to which @arg{device-manager} is associated
    to, or NULL. This memory is owned by GDK and must not be freed or
    unreferenced.
  @end{return}
  @begin{short}
    Gets the @class{gdk-display} object associated to device_manager.
  @end{short}

  Since 3.0"
  (gdk-device-manager-display device-manager))

(export 'gdk-device-manager-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_list_devices ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_manager_list_devices" gdk-device-manager-list-devices)
    (g-list (g-object gdk-device) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[device-manager]{a @class{gdk-device-manager} object}
  @argument[type]{device type to get}
  @return{A list of @class{gdk-device} objects.}
  @begin{short}
    Returns the list of devices of type type currently attached to
    @arg{device-manager}.
  @end{short}

  Since 3.0"
  (device-manager (g-object gdk-device-manager))
  (type gdk-device-type))

(export 'gdk-device-manager-list-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_manager_get_client_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_device_manager_get_client_pointer"
           gdk-device-manager-get-client-pointer) (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-7}
  @argument[device-manager]{a @class{gdk-device-manager} object}
  @return{The client pointer.}
  @begin{short}
    Returns the client pointer, that is, the master pointer that acts as the
    core pointer for this application. In X11, window managers may change this
    depending on the interaction pattern under the presence of several pointers.
  @end{short}

  You should use this function sheldomly, only in code that isn't triggered by
  a @class{gdk-event} event and there aren't other means to get a meaningful
  @class{gdk-device} to operate on.

  Since 3.0"
  (device-manager (g-object gdk-device-manager)))

(export 'gdk-device-manager-get-client-pointer)

;;; --- End of file gdk.device-manager.lisp ------------------------------------
