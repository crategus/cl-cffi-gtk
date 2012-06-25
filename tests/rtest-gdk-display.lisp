;;; ----------------------------------------------------------------------------
;;; rtest-gdk-display.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(define-test gdk-display
  (assert-true  (g-type-is-object "GdkDisplay"))
  (assert-false (g-type-is-abstract "GdkDisplay"))
  (assert-true  (g-type-is-derived "GdkDisplay"))
  (assert-false (g-type-is-fundamental "GdkDisplay"))
  (assert-true  (g-type-is-value-type "GdkDisplay"))
  (assert-true  (g-type-has-value-table "GdkDisplay"))
  (assert-true  (g-type-is-classed "GdkDisplay"))
  (assert-true  (g-type-is-instantiatable "GdkDisplay"))
  (assert-true  (g-type-is-derivable "GdkDisplay"))
  (assert-true  (g-type-is-deep-derivable "GdkDisplay"))
  (assert-false (g-type-is-interface "GdkDisplay"))

  ;; Check the registered name
  (assert-eq 'gdk-display
             (registered-object-type-by-name "GdkDisplay"))
  
  (let ((class (g-type-class-ref (gtype "GdkDisplay"))))
    (assert-equal (gtype "GdkDisplay")  (g-type-from-class class))
    (assert-equal (gtype "GdkDisplay") (g-object-class-type class))
    (assert-equal "GdkDisplay" (g-object-class-name class))
    (assert-equal (gtype "GdkDisplay")
                  (g-type-from-class (g-type-class-peek "GdkDisplay")))
    (assert-equal (gtype "GdkDisplay")
                  (g-type-from-class (g-type-class-peek-static "GdkDisplay")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-display)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-display (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkDisplay" (gobject-class-g-type-name class))
    (assert-equal "GdkDisplay" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_display_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GdkDisplay"))
  (assert-eql 2 (g-type-depth "GdkDisplay"))
  (assert-eql   (gtype "GdkDisplay")
                (g-type-next-base "GdkDisplay" "GObject"))
  (assert-true  (g-type-is-a "GdkDisplay" "GObject"))
  (assert-false (g-type-is-a "GdkDisplay" "gboolean"))
  (assert-false (g-type-is-a "GdkDisplay" "GtkWindow"))
  (assert-equal '("GdkX11Display")
                (mapcar #'gtype-name (g-type-children "GdkDisplay")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkDisplay")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkDisplay" query)
    (assert-equal (gtype "GdkDisplay")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkDisplay"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 248 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  64 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties
  (assert-equal
      ' ()
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkDisplay"))))

  (let ((display (gdk-display-get-default)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11Display") (g-object-type display))
    (assert-equal "GdkX11Display" (g-object-type-name display))
    (assert-true (g-type-is-a "GdkX11Display"
                              (g-type-from-instance (pointer display))))
    (assert-true (g-type-is-a "GdkX11Display"
                              (g-type-from-instance (pointer display))))
    ;; Access the properties
    ;; GDKDisplay has no properties
    ;; Check some functions

    ;;     gdk_display_open

    (assert-eq 'gdk-display (type-of (gdk-display-get-default)))
    (assert-equal ":0" (gdk-display-get-name display))
    (assert-eq 1 (gdk-display-get-n-screens display))
    (assert-eq 'gdk-screen (type-of (gdk-display-get-screen display 0)))
    (assert-eq 'gdk-screen (type-of (gdk-display-get-default-screen display)))
    (assert-eq 'gdk-x11-device-manager-xi2
               (type-of (gdk-display-get-device-manager display)))

    ;;     gdk_display_device_is_grabbed
    ;;     gdk_display_beep
    ;;     gdk_display_sync
    ;;     gdk_display_flush
    ;;     gdk_display_close

    (assert-false (gdk-display-is-closed display))

    ;;     gdk_display_get_event

    (assert-false (gdk-display-peek-event display))

    ;;     gdk_display_put_event

    (assert-true (gdk-display-has-pending display))

    ;;     gdk_display_set_double_click_time
    ;;     gdk_display_set_double_click_distance
    ;;     gdk_display_get_pointer
    ;;     gdk_display_list_devices
    ;;     gdk_display_get_window_at_pointer
    ;;     gdk_display_warp_pointer

    (assert-true (gdk-display-supports-cursor-color display))
    (assert-true (gdk-display-supports-cursor-alpha display))
    (assert-eql 24 (gdk-display-get-default-cursor-size display))
    (assert-equal (values 64 64)
                  (gdk-display-get-maximal-cursor-size display))
    (assert-eq 'gdk-window (type-of (gdk-display-get-default-group display)))
    (assert-true (gdk-display-supports-selection-notification display))
    (assert-true (gdk-display-request-selection-notification display ""))
    (assert-true (gdk-display-supports-clipboard-persistence display))

    ;;     gdk_display_store_clipboard

    (assert-true (gdk-display-supports-shapes display))
    (assert-true (gdk-display-supports-input-shapes display))
    (assert-true (gdk-display-supports-composite display))
    (assert-eq 'gdk-app-launch-context
               (type-of (gdk-display-get-app-launch-context display)))

    ;;     gdk_display_notify_startup_complete
  ))

