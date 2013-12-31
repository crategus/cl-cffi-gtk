;;; ----------------------------------------------------------------------------
;;; cairo.device.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; cairo_device_t
;;;
;;; Interface to underlying rendering system
;;;
;;; Synopsis
;;;
;;;     cairo_device_t
;;;
;;;     cairo_device_reference
;;;     cairo_device_destroy
;;;     cairo_device_status
;;;     cairo_device_finish
;;;     cairo_device_flush
;;;
;;;     cairo_device_type_t
;;;
;;;     cairo_device_get_type
;;;     cairo_device_get_reference_count
;;;     cairo_device_set_user_data
;;;     cairo_device_get_user_data
;;;     cairo_device_acquire
;;;     cairo_device_release
;;;
;;; Description
;;;
;;; Devices are the abstraction Cairo employs for the rendering system used by
;;; a cairo_surface_t. You can get the device of a surface using
;;; cairo_surface_get_device().
;;;
;;; Devices are created using custom functions specific to the rendering system
;;; you want to use. See the documentation for the surface types for those
;;; functions.
;;;
;;; An important function that devices fulfill is sharing access to the
;;; rendering system between Cairo and your application. If you want to access
;;; a device directly that you used to draw to with Cairo, you must first call
;;; cairo_device_flush() to ensure that Cairo finishes all operations on the
;;; device and resets it to a clean state.
;;;
;;; Cairo also provides the functions cairo_device_acquire() and
;;; cairo_device_release() to synchronize access to the rendering system in a
;;; multithreaded environment. This is done internally, but can also be used by
;;; applications.
;;;
;;; Putting this all together, a function that works with devices should look
;;; something like this:
;;;
;;; void
;;; my_device_modifying_function (cairo_device_t *device)
;;; {
;;;   cairo_status_t status;
;;;
;;;   // Ensure the device is properly reset
;;;   cairo_device_flush (device);
;;;   // Try to acquire the device
;;;   status = cairo_device_acquire (device);
;;;   if (status != CAIRO_STATUS_SUCCESS) {
;;;     printf ("Failed to acquire the device: %s\n",
;;;             cairo_status_to_string (status));
;;;     return;
;;;   }
;;;
;;;   // Do the custom operations on the device here.
;;;   // But do not call any Cairo functions that might acquire devices.
;;;
;;;   // Release the device when done.
;;;   cairo_device_release (device);
;;; }
;;;
;;; Note
;;;
;;; Please refer to the documentation of each backend for additional usage
;;; requirements, guarantees provided, and interactions with existing surface
;;; API of the device functions for surfaces of that type.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_device_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-device-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-device-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-device-t atdoc:*external-symbols*)
 "@version{2013-11-16}
  @begin{short}
    A @sym{cairo-device-t} represents the driver interface for drawing
    operations to a @symbol{cairo-surface-t}.
  @end{short}
  There are different subtypes of @sym{cairo-device-t} for different drawing
  backends; for example, the function @code{cairo-egl-device-create} creates
  a device that wraps an EGL display and context.

  The type of a device can be queried with the function
  @fun{cairo-device-get-type}.

  Memory management of @sym{cairo-device-t} is done with the functions
  @fun{cairo-device-reference} and @fun{cairo-device-destroy}.

  Since 1.10
  @see-symbol{cairo-surface-t}
  @see-function{cairo-device-get-type}
  @see-function{cairo-device-reference}
  @see-function{cairo-device-destroy}")

(export 'cairo-device-t)

;;; ----------------------------------------------------------------------------
;;; cairo_device_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_reference" cairo-device-reference)
    (:pointer (:struct cairo-device-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[device]{a @symbol{cairo-device-t}}
  @return{The referenced @symbol{cairo-device-t}.}
  @begin{short}
    Increases the reference count on @arg{device} by one.
  @end{short}
  This prevents @arg{device} from being destroyed until a matching call to
  the function @fun{cairo-device-destroy} is made.

  The number of references to a @symbol{cairo-device-t} can be get using
  the function @fun{cairo-device-get-reference-count}.

  Since 1.10
  @see-symbol{cairo-device-t}
  @see-function{cairo-device-destroy}
  @see-function{cairo-device-get-reference-count}"
  (device (:pointer (:struct cairo-device-t))))

(export 'cairo-device-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_device_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_destroy" cairo-device-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[device]{a @symbol{cairo-device-t}}
  @begin{short}
    Decreases the reference count on device by one.
  @end{short}
  If the result is zero, then @arg{device} and all associated resources are
  freed. See the function @fun{cairo-device-reference}.

  This function may acquire devices if the last reference was dropped.

  Since 1.10
  @see-symbol{cairo-device-t}
  @see-function{cairo-device-reference}"
  (device (:pointer (:struct cairo-device-t))))

(export 'cairo-device-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_device_status ()
;;;
;;; cairo_status_t cairo_device_status (cairo_device_t *device);
;;;
;;; Checks whether an error has previously occurred for this device.
;;;
;;; device :
;;;     a cairo_device_t
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS on success or an error code if the device is in an
;;;     error state.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_finish ()
;;;
;;; void cairo_device_finish (cairo_device_t *device);
;;;
;;; This function finishes the device and drops all references to external
;;; resources. All surfaces, fonts and other objects created for this device
;;; will be finished, too. Further operations on the device will not affect the
;;; device but will instead trigger a CAIRO_STATUS_DEVICE_FINISHED error.
;;;
;;; When the last call to cairo_device_destroy() decreases the reference count
;;; to zero, cairo will call cairo_device_finish() if it hasn't been called
;;; already, before freeing the resources associated with the device.
;;;
;;; This function may acquire devices.
;;;
;;; device :
;;;     the cairo_device_t to finish
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_flush ()
;;;
;;; void cairo_device_flush (cairo_device_t *device);
;;;
;;; Finish any pending operations for the device and also restore any temporary
;;; modifications cairo has made to the device's state. This function must be
;;; called before switching from using the device with Cairo to operating on it
;;; directly with native APIs. If the device doesn't support direct access, then
;;; this function does nothing.
;;;
;;; This function may acquire devices.
;;;
;;; device :
;;;     a cairo_device_t
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_device_type_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-device-type-t
  :drm
  :gl
  :script
  :xcb
  :xlib
  :xml
  :cogl
  :win32
  (:invalid -1))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-device-type-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-device-type-t atdoc:*external-symbols*)
 "@version{2013-12-8}
  @begin{short}
    @sym{cairo-device-type-t} is used to describe the type of a given device.
    The devices types are also known as \"backends\" within cairo.
  @end{short}

  The device type can be queried with the function @fun{cairo-device-get-type}.

  The various @symbol{cairo-device-t} functions can be used with devices of any
  type, but some backends also provide type-specific functions that must only be
  called with a device of the appropriate type. These functions have names
  that begin with @code{cairo-type-device} such as
  @code{cairo-xcb-device-debug-cap-xrender-version}.

  The behavior of calling a type-specific function with a device of the wrong
  type is undefined.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-device-type-t
  :drm
  :gl
  :script
  :xcb
  :xlib
  :xml
  :cogl
  :win32
  (:invalid -1))
  @end{pre}
  @begin[code]{table}
    @entry[:drm]{The device is of type Direct Render Manager, since 1.10.}
    @entry[:gl]{The device is of type OpenGL, since 1.10.}
    @entry[:script]{The device is of type script, since 1.10.}
    @entry[:xcb]{The device is of type xcb, since 1.10.}
    @entry[:xlib]{The device is of type xlib, since 1.10.}
    @entry[:xml]{The device is of type XML, since 1.10.}
    @entry[:cogl]{The device is of type cogl, since 1.12.}
    @entry[:win32]{The device is of type win32, since 1.12.}
    @entry[:invalid]{The device is invalid, since 1.10.}
  @end{table}
  Since 1.10
  @see-symbol{cairo-device-t}
  @see-function{cairo-device-get-type}")

(export 'cairo-device-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_get_type" cairo-device-get-type) cairo-device-type-t
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[device]{a @symbol{cairo-device-t}}
  @return{The type of device.}
  @begin{short}
    This function returns the type of the device.
  @end{short}
  See the @symbol{cairo-device-type-t} enumeration for available types.

  Since 1.10
  @see-symbol{cairo-device-t}
  @see-symbol{cairo-device-type-t}"
  (device (:pointer (:struct cairo-device-t))))

(export 'cairo-device-get-type)

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_reference_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_get_reference_count" cairo-device-get-reference-count)
    :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-12-28}
  @argument[device]{a @symbol{cairo-device-t}}
  @begin{return}
    The current reference count of device. If the object is a nil object,
    0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of device.
  @end{short}

  Since 1.10
  @see-symbol{cairo-device-t}"
  (device (:pointer (:struct cairo-device-t))))

(export 'cairo-device-get-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_device_set_user_data ()
;;;
;;; cairo_status_t cairo_device_set_user_data (cairo_device_t *device,
;;;                                            const cairo_user_data_key_t *key,
;;;                                            void *user_data,
;;;                                            cairo_destroy_func_t destroy);
;;;
;;; Attach user data to device. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; device :
;;;     a cairo_device_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_device_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_get_user_data ()
;;;
;;; void * cairo_device_get_user_data (cairo_device_t *device,
;;;                                    const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to device using the specified key. If
;;; no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; device :
;;;     a cairo_device_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_device_acquire ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_acquire" cairo-device-acquire) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2013-11-16}
  @argument[device]{a @symbol{cairo-device-t}}
  @begin{return}
    @code{:success} on success or an error code if the device is in an
    error state and could not be acquired. After a successful call to
    @sym{cairo-device-acquire}, a matching call to the function
    @fun{cairo-device-release} is required.
  @end{return}
  @begin{short}
    Acquires the device for the current thread. This function will block until
    no other thread has acquired the device.
  @end{short}

  If the return value is @code{:sucess}, you successfully acquired the device.
  From now on your thread owns the device and no other thread will be able to
  acquire it until a matching call to the function @fun{cairo-device-release}.
  It is allowed to recursively acquire the device multiple times from the same
  thread.

  @subheading{Note}
    You must never acquire two different devices at the same time unless this is
    explicitly allowed. Otherwise the possibility of deadlocks exist.

    As various Cairo functions can acquire devices when called, these functions
    may also cause deadlocks when you call them with an acquired device. So you
    must not have a device acquired when calling them. These functions are
    marked in the documentation.

  Since 1.10
  @see-symbol{cairo-device-t}
  @see-symbol{cairo-status-t}
  @see-function{cairo-device-release}"
  (device (:pointer (:struct cairo-device-t))))

(export 'cairo-device-acquire)

;;; ----------------------------------------------------------------------------
;;; cairo_device_release ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_device_release" cairo-device-release) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-5}
  @argument[device]{a @symbol{cairo-device-t}}
  @begin{short}
    Releases a device previously acquired using the function
    @fun{cairo-device-acquire}.
  @end{short}
  See that function for details.

  Since 1.10
  @see-symbol{cairo-device-t}"
  (device (:pointer (:struct cairo-device-t))))

(export 'cairo-device-release)

;;; --- End of file cairo.device.lisp ------------------------------------------
