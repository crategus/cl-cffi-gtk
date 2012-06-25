;;; ----------------------------------------------------------------------------
;;; rtest-gdk-device-manager.lisp
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

(define-test gdk-device-manager
  (assert-true  (g-type-is-object "GdkDeviceManager"))
  (assert-true  (g-type-is-abstract "GdkDeviceManager"))
  (assert-true  (g-type-is-derived "GdkDeviceManager"))
  (assert-false (g-type-is-fundamental "GdkDeviceManager"))
  (assert-true  (g-type-is-value-type "GdkDeviceManager"))
  (assert-true  (g-type-has-value-table "GdkDeviceManager"))
  (assert-true  (g-type-is-classed "GdkDeviceManager"))
  (assert-true  (g-type-is-instantiatable "GdkDeviceManager"))
  (assert-true  (g-type-is-derivable "GdkDeviceManager"))
  (assert-true  (g-type-is-deep-derivable "GdkDeviceManager"))
  (assert-false (g-type-is-interface "GdkDeviceManager"))

  ;; Check the registered name
  (assert-eq 'gdk-device-manager
             (registered-object-type-by-name "GdkDeviceManager"))

  (let ((class (g-type-class-ref (gtype "GdkDeviceManager"))))
    (assert-equal (gtype "GdkDeviceManager") (g-type-from-class class))
    (assert-equal (gtype "GdkDeviceManager") (g-object-class-type class))
    (assert-equal "GdkDeviceManager" (g-object-class-name class))
    (assert-equal (gtype "GdkDeviceManager")
                  (g-type-from-class (g-type-class-peek "GdkDeviceManager")))
    (assert-equal (gtype "GdkDeviceManager")
                  (g-type-from-class
                    (g-type-class-peek-static "GdkDeviceManager")))
    (g-type-class-unref class))

  (let ((class (find-class 'gdk-device-manager)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-device-manager (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkDeviceManager" (gobject-class-g-type-name class))
    (assert-equal "GdkDeviceManager" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_device_manager_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))

  (assert-equal (gtype "GObject") (g-type-parent "GdkDeviceManager"))
  (assert-eql 2 (g-type-depth "GdkDeviceManager"))
  (assert-eql   (gtype "GdkDeviceManager")
                (g-type-next-base "GdkDeviceManager" "GObject"))
  (assert-true  (g-type-is-a "GdkDeviceManager" "GObject"))
  (assert-false (g-type-is-a "GdkDeviceManager" "gboolean"))
  (assert-false (g-type-is-a "GdkDeviceManager" "GtkWindow"))
  (assert-equal '("GdkX11DeviceManagerCore")
                (mapcar #'gtype-name (g-type-children "GdkDeviceManager")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkDeviceManager")))

  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query  "GdkDeviceManager" query)
    (assert-equal  (gtype "GdkDeviceManager")
                   (foreign-slot-value query 'g-type-query :type))
    (assert-equal  "GdkDeviceManager"
                   (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql  88 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  16 (foreign-slot-value query 'g-type-query :instance-size)))

  ;; Get the names of the class properties
  (assert-equal
      '("display")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkDeviceManager"))))

  (let* ((display (gdk-display-get-default))
         (device-manager (gdk-display-get-device-manager display)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11DeviceManagerXI2")
                  (g-object-type device-manager))
    (assert-equal "GdkX11DeviceManagerXI2" (g-object-type-name device-manager))
    (assert-true (g-type-is-a "GdkX11DeviceManagerXI2"
                              (g-type-from-instance (pointer device-manager))))
    ;; Access the properties
    (assert-eq 'gdk-display
               (type-of (gdk-device-manager-display device-manager)))
    ;; Call functions
    (assert-eql 2 (length (gdk-device-manager-list-devices device-manager :master)))
    (assert-eql 9 (length (gdk-device-manager-list-devices device-manager :slave)))
    (assert-eql 0 (length (gdk-device-manager-list-devices device-manager :floating)))
    (assert-eq 'gdk-x11-device-xi2
               (type-of (gdk-device-manager-get-client-pointer device-manager)))
  ))

