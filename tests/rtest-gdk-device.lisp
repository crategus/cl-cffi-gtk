;;; ----------------------------------------------------------------------------
;;; rtest-gdk-device.lisp
;;;
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

(in-package :gdk-tests)

(define-test gdk-device
  (assert-true  (g-type-is-object "GdkDevice"))
  (assert-true  (g-type-is-abstract "GdkDevice"))
  (assert-true  (g-type-is-derived "GdkDevice"))
  (assert-false (g-type-is-fundamental "GdkDevice"))
  (assert-true  (g-type-is-value-type "GdkDevice"))
  (assert-true  (g-type-has-value-table "GdkDevice"))
  (assert-true  (g-type-is-classed "GdkDevice"))
  (assert-true  (g-type-is-instantiatable "GdkDevice"))
  (assert-true  (g-type-is-derivable "GdkDevice"))
  (assert-true  (g-type-is-deep-derivable "GdkDevice"))
  (assert-false (g-type-is-interface "GdkDevice"))

  ;; Check the registered name
  (assert-eq 'gdk-device
             (registered-object-type-by-name "GdkDevice"))
  
  (let ((class (g-type-class-ref (gtype "GdkDevice"))))
    (assert-equal (gtype "GdkDevice")  (g-type-from-class class))
    (assert-equal (gtype "GdkDevice") (g-object-class-type class))
    (assert-equal "GdkDevice" (g-object-class-name class))
    (assert-equal (gtype "GdkDevice")
                  (g-type-from-class (g-type-class-peek "GdkDevice")))
    (assert-equal (gtype "GdkDevice")
                  (g-type-from-class (g-type-class-peek-static "GdkDevice")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-device)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-device (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkDevice" (gobject-class-g-type-name class))
    (assert-equal "GdkDevice" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_device_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GdkDevice"))
  (assert-eql 2 (g-type-depth "GdkDevice"))
  (assert-eql   (gtype "GdkDevice")
                (g-type-next-base "GdkDevice" "GObject"))
  (assert-true  (g-type-is-a "GdkDevice" "GObject"))
  (assert-false (g-type-is-a "GdkDevice" "gboolean"))
  (assert-false (g-type-is-a "GdkDevice" "GtkWindow"))
  (assert-equal '("GdkX11DeviceXI2")
                (mapcar #'gtype-name (g-type-children "GdkDevice")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkDevice")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkDevice" query)
    (assert-equal (gtype "GdkDevice")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkDevice"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 104 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  60 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties of "GdkDevice".
  (assert-equal
      ' ("display" "device-manager" "name" "associated-device" "type"
         "input-source" "input-mode" "has-cursor" "n-axes")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkDevice"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkDevice" GDK-DEVICE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_device_get_type")
                               ((ASSOCIATED-DEVICE GDK-DEVICE-ASSOCIATED-DEVICE
                                 "associated-device" "GdkDevice" T NIL)
                                (DEVICE-MANAGER GDK-DEVICE-DEVICE-MANAGER
                                 "device-manager" "GdkDeviceManager" T NIL)
                                (DISPLAY GDK-DEVICE-DISPLAY "display"
                                 "GdkDisplay" T NIL)
                                (HAS-CURSOR GDK-DEVICE-HAS-CURSOR "has-cursor"
                                 "gboolean" T NIL)
                                (INPUT-MODE GDK-DEVICE-INPUT-MODE "input-mode"
                                 "GdkInputMode" T T)
                                (INPUT-SOURCE GDK-DEVICE-INPUT-SOURCE
                                 "input-source" "GdkInputSource" T NIL)
                                (N-AXES GDK-DEVICE-N-AXES "n-axes" "guint" T
                                 NIL)
                                (NAME GDK-DEVICE-NAME "name" "gchararray" T
                                 NIL)
                                (TYPE GDK-DEVICE-TYPE "type" "GdkDeviceType" T
                                 NIL)))
     (get-g-class-definition (gtype "GdkDevice"))))

(define-test gdk-x11-device-xi2
  (assert-true  (g-type-is-object "GdkX11DeviceXI2"))
  (assert-false (g-type-is-abstract "GdkX11DeviceXI2"))
  (assert-true  (g-type-is-derived "GdkX11DeviceXI2"))
  (assert-false (g-type-is-fundamental "GdkX11DeviceXI2"))
  (assert-true  (g-type-is-value-type "GdkX11DeviceXI2"))
  (assert-true  (g-type-has-value-table "GdkX11DeviceXI2"))
  (assert-true  (g-type-is-classed "GdkX11DeviceXI2"))
  (assert-true  (g-type-is-instantiatable "GdkX11DeviceXI2"))
  (assert-true  (g-type-is-derivable "GdkX11DeviceXI2"))
  (assert-true  (g-type-is-deep-derivable "GdkX11DeviceXI2"))
  (assert-false (g-type-is-interface "GdkX11DeviceXI2"))

  ;; Check the registered name
  (assert-eq 'gdk-x11-device-xi2
             (registered-object-type-by-name "GdkX11DeviceXI2"))

  (let ((class (g-type-class-ref (gtype "GdkX11DeviceXI2"))))
    (assert-equal (gtype "GdkX11DeviceXI2")  (g-type-from-class class))
    (assert-equal (gtype "GdkX11DeviceXI2")
                  (g-type-from-class (g-type-class-peek "GdkX11DeviceXI2")))
    (assert-equal (gtype "GdkX11DeviceXI2")
                  (g-type-from-class (g-type-class-peek-static "GdkX11DeviceXI2")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-device)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-device (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkDevice" (gobject-class-g-type-name class))
    (assert-equal "GdkDevice" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_device_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GdkDevice") (g-type-parent "GdkX11DeviceXI2"))
  (assert-eql 3 (g-type-depth "GdkX11DeviceXI2"))
  (assert-eql   (gtype "GdkDevice")
                (g-type-next-base "GdkDevice" "GObject"))
  (assert-true  (g-type-is-a "GdkX11DeviceXI2" "GObject"))
  (assert-false (g-type-is-a "GdkX11DeviceXI2" "gboolean"))
  (assert-false (g-type-is-a "GdkX11DeviceXI2" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GdkX11DeviceXI2")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkX11DeviceXI2")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkX11DeviceXI2" query)
    (assert-equal (gtype "GdkX11DeviceXI2")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkX11DeviceXI2"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 104 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  68 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties
  (assert-equal
      ' ("display" "device-manager" "name" "associated-device" "type"
         "input-source" "input-mode" "has-cursor" "n-axes" "device-id")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkX11DeviceXI2"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkX11DeviceXI2" GDK-X11-DEVICE-X-I2
                               (:SUPERCLASS GDK-DEVICE :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gdk_x11_device_xi2_get_type")
                               ((DEVICE-ID GDK-X11-DEVICE-X-I2-DEVICE-ID
                                 "device-id" "gint" T NIL)))
     (get-g-class-definition (gtype "GdkX11DeviceXI2")))

  (let* ((device (make-instance 'gdk-x11-device-xi2))
         (ptr (pointer device)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11DeviceXI2") (g-object-type device))
    (assert-equal "GdkX11DeviceXI2" (g-object-type-name device))
    (assert-true (g-type-is-a "GdkX11DeviceXI2" (g-type-from-instance ptr)))
    ;; Access the properties
    (assert-false (gdk-device-associated-device device))
    (assert-false (gdk-device-device-manager device))
    (assert-false (gdk-device-display device))
    (assert-false (gdk-device-has-cursor device))
    (assert-eq :disabled (gdk-device-input-mode device))
    (assert-eq :mouse (gdk-device-input-source device))
    (assert-eql 0 (gdk-device-n-axes device))
    (assert-false (gdk-device-name device))
    (assert-eq :master (gdk-device-type device))
    ;; Call functions
    (assert-false (gdk-device-get-name device))
    (assert-eq :mouse (gdk-device-get-source device))
    (assert-eq :disabled (gdk-device-set-mode device (gdk-device-get-mode device)))
    (assert-eq :disabled (gdk-device-get-mode device))
    ;;     gdk_device_set_key
    ;;     gdk_device_get_key
    ;;     gdk_device_set_axis_use
    ;;     gdk_device_get_axis_use

    (assert-false (gdk-device-get-associated-device device))

    ;;     gdk_device_list_slave_devices

    (assert-eq :master (gdk-device-get-device-type device))
    (assert-false (gdk-device-get-display device))
    (assert-false (gdk-device-get-has-cursor device))
    (assert-eq 0 (gdk-device-get-n-axes device))
    (assert-eq 0 (gdk-device-get-n-keys device))

    ;;     gdk_device_warp
    ;;     gdk_device_grab
    ;;     gdk_device_ungrab
    ;;     gdk_device_get_state
    ;;     gdk_device_get_position
    ;;     gdk_device_get_window_at_position
    ;;     gdk_device_get_history
    ;;     gdk_device_free_history
    ;;     gdk_device_get_axis
    ;;     gdk_device_list_axes
    ;;     gdk_device_get_axis_value
    ))

;;; --- End of file rtest-gdk-device.lisp --------------------------------------
