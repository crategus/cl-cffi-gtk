;;; ----------------------------------------------------------------------------
;;; rtest-gdk-screen.lisp
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

(define-test gdk-screen
  (assert-true  (g-type-is-object "GdkScreen"))
  (assert-false (g-type-is-abstract "GdkScreen"))
  (assert-true  (g-type-is-derived "GdkScreen"))
  (assert-false (g-type-is-fundamental "GdkScreen"))
  (assert-true  (g-type-is-value-type "GdkScreen"))
  (assert-true  (g-type-has-value-table "GdkScreen"))
  (assert-true  (g-type-is-classed "GdkScreen"))
  (assert-true  (g-type-is-instantiatable "GdkScreen"))
  (assert-true  (g-type-is-derivable "GdkScreen"))
  (assert-true  (g-type-is-deep-derivable "GdkScreen"))
  (assert-false (g-type-is-interface "GdkScreen"))

  ;; Check the registered name
  (assert-eq 'gdk-Screen
             (registered-object-type-by-name "GdkScreen"))
  
  (let ((class (g-type-class-ref (gtype "GdkScreen"))))
    (assert-equal (gtype "GdkScreen")  (g-type-from-class class))
    (assert-equal (gtype "GdkScreen") (g-object-class-type class))
    (assert-equal "GdkScreen" (g-object-class-name class))
    (assert-equal (gtype "GdkScreen")
                  (g-type-from-class (g-type-class-peek "GdkScreen")))
    (assert-equal (gtype "GdkScreen")
                  (g-type-from-class (g-type-class-peek-static "GdkScreen")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-Screen)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-Screen (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkScreen" (gobject-class-g-type-name class))
    (assert-equal "GdkScreen" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_screen_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GdkScreen"))
  (assert-eql 2 (g-type-depth "GdkScreen"))
  (assert-eql   (gtype "GdkScreen")
                (g-type-next-base "GdkScreen" "GObject"))
  (assert-true  (g-type-is-a "GdkScreen" "GObject"))
  (assert-false (g-type-is-a "GdkScreen" "gboolean"))
  (assert-false (g-type-is-a "GdkScreen" "GtkWindow"))
  (assert-equal '("GdkX11Screen")
                (mapcar #'gtype-name (g-type-children "GdkScreen")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkScreen")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkScreen" query)
    (assert-equal (gtype "GdkScreen")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkScreen"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 204 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties of "GdkScreen".
  (assert-equal
      ' ("font-options" "resolution")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkScreen"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkScreen" GDK-SCREEN
        (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
          :TYPE-INITIALIZER "gdk_screen_get_type")
        ((FONT-OPTIONS GDK-SCREEN-FONT-OPTIONS "font-options" "gpointer" T T)
         (RESOLUTION GDK-SCREEN-RESOLUTION "resolution" "gdouble" T T)))
     (get-g-type-definition (gtype "GdkScreen")))

  (let* ((screen (gdk-screen-get-default))
         (ptr (pointer screen)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11Screen") (g-object-type screen))
    (assert-equal "GdkX11Screen" (g-object-type-name screen))
    (assert-true (g-type-is-a "GdkX11Screen" (g-type-from-instance ptr)))
    ;; Access the properties
    (assert-true (pointerp (gdk-screen-font-options screen)))
    (assert-eql 96.0d0 (gdk-screen-resolution screen))
    ;; Call functions
    (assert-eq 'gdk-screen (type-of (gdk-screen-get-default)))
    (assert-eq 'gdk-visual (type-of (gdk-screen-get-system-visual screen)))
    (assert-eq 'gdk-visual (type-of (gdk-screen-get-rgba-visual screen)))
    (assert-true (gdk-screen-is-composited screen))
    (assert-eq 'gdk-window (type-of (gdk-screen-get-root-window screen)))
    (assert-eq 'gdk-display (type-of (gdk-screen-get-display screen)))
    (assert-eql 0 (gdk-screen-get-number screen))
    (assert-eql 1280 (gdk-screen-get-width screen))
    (assert-eql 800 (gdk-screen-get-height screen))
    (assert-eql 339 (gdk-screen-get-width-mm screen))
    (assert-eql 212 (gdk-screen-get-height-mm screen))
    (assert-eq 'gdk-visual (type-of (first (gdk-screen-list-visuals screen))))
    (assert-eq 'gdk-window (type-of (first (gdk-screen-get-toplevel-windows screen))))
    (assert-equal ":0.0" (gdk-screen-make-display-name screen))
    (assert-eql 1 (gdk-screen-get-n-monitors screen))
    (assert-eql 0 (gdk-screen-get-primary-monitor screen))
    (assert-eq 'gdk-rectangle (type-of (gdk-screen-get-monitor-geometry screen 0)))
    (assert-eq 'gdk-rectangle (type-of (gdk-screen-get-monitor-workarea screen 0)))

    ;;     gdk_screen_get_monitor_at_point
    ;;     gdk_screen_get_monitor_at_window

    (assert-eql 207 (gdk-screen-get-monitor-height-mm screen 0))
    (assert-eql 331 (gdk-screen-get-monitor-width-mm screen 0))
    (assert-equal "LVDS1" (gdk-screen-get-monitor-plug-name screen 0))

    ;;     gdk_screen_get_setting

    (assert-true (pointerp (gdk-screen-get-font-options screen)))
    (assert-true (pointerp (gdk-screen-set-font-options screen (gdk-screen-get-font-options screen))))
    (assert-eql 96.0d0 (gdk-screen-get-resolution screen))
    (assert-eql 96.0d0 (gdk-screen-set-resolution screen (gdk-screen-get-resolution screen)))
    (assert-eq 'gdk-window (type-of (gdk-screen-get-active-window screen)))
    (assert-eq 'gdk-window (type-of (first (gdk-screen-get-window-stack screen))))
))

;;; --- End of file rtest-gdk-screen.lisp --------------------------------------
