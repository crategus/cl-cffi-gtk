;;; ----------------------------------------------------------------------------
;;; rtest-gdk-display-manager.lisp
;;;
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

(in-package :gdk-tests)

#-windows
(define-test gdk-display-manager
  (assert-true  (g-type-is-object "GdkDisplayManager"))
  (assert-false (g-type-is-abstract "GdkDisplayManager"))
  (assert-true  (g-type-is-derived "GdkDisplayManager"))
  (assert-false (g-type-is-fundamental "GdkDisplayManager"))
  (assert-true  (g-type-is-value-type "GdkDisplayManager"))
  (assert-true  (g-type-has-value-table "GdkDisplayManager"))
  (assert-true  (g-type-is-classed "GdkDisplayManager"))
  (assert-true  (g-type-is-instantiatable "GdkDisplayManager"))
  (assert-true  (g-type-is-derivable "GdkDisplayManager"))
  (assert-true  (g-type-is-deep-derivable "GdkDisplayManager"))
  (assert-false (g-type-is-interface "GdkDisplayManager"))

  ;; Check the registered name
  (assert-eq 'gdk-display-manager
             (registered-object-type-by-name "GdkDisplayManager"))

  (let ((class (g-type-class-ref (gtype "GdkDisplayManager"))))
    (assert-equal (gtype "GdkDisplayManager") (g-type-from-class class))
    (assert-equal (gtype "GdkDisplayManager") (g-object-class-type class))
    (assert-equal "GdkDisplayManager" (g-object-class-name class))
    (assert-equal (gtype "GdkDisplayManager")
                  (g-type-from-class (g-type-class-peek "GdkDisplayManager")))
    (assert-equal (gtype "GdkDisplayManager")
                  (g-type-from-class
                    (g-type-class-peek-static "GdkDisplayManager")))
    (g-type-class-unref class))

  (let ((class (find-class 'gdk-display-manager)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-display-manager (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkDisplayManager" (gobject-class-g-type-name class))
    (assert-equal "GdkDisplayManager" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_display_manager_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))

  (assert-equal (gtype "GObject") (g-type-parent "GdkDisplayManager"))
  (assert-eql 2 (g-type-depth "GdkDisplayManager"))
  (assert-eql   (gtype "GdkDisplayManager")
                (g-type-next-base "GdkDisplayManager" "GObject"))
  (assert-true  (g-type-is-a "GdkDisplayManager" "GdkDisplayManager"))
  (assert-true  (g-type-is-a "GdkDisplayManager" "GObject"))
  (assert-false (g-type-is-a "GdkDisplayManager" "gboolean"))
  (assert-false (g-type-is-a "GdkDisplayManager" "GtkWindow"))
  (assert-equal '("GdkX11DisplayManager")
                (mapcar #'gtype-name (g-type-children "GdkDisplayManager")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkDisplayManager")))

  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query  "GdkDisplayManager" query)
    (assert-equal  (gtype "GdkDisplayManager")
                   (foreign-slot-value query 'g-type-query :type))
    (assert-equal  "GdkDisplayManager"
                   (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 108 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  12 (foreign-slot-value query 'g-type-query :instance-size)))

  ;; Get the names of the class properties
  (assert-equal
      '("default-display")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkDisplayManager"))))

  (let* ((display-manager (gdk-display-manager-get))
         (display (gdk-display-manager-get-default-display display-manager)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11DisplayManager")
                  (g-object-type display-manager))
    (assert-equal "GdkX11DisplayManager" (g-object-type-name display-manager))
    (assert-true (g-type-is-a "GdkX11DisplayManager"
                              (g-type-from-instance (pointer display-manager))))
    (assert-true (g-type-is-a "GdkX11Display"
                              (g-type-from-instance (pointer display))))
    ;; Access the properties
    (assert-eq 'gdk-display
               (type-of (gdk-display-manager-default-display display-manager)))
    ;; Check some functions
  ))

#+windows
(define-test gdk-display-manager
  (assert-true  (g-type-is-object "GdkDisplayManager"))
  (assert-false (g-type-is-abstract "GdkDisplayManager"))
  (assert-true  (g-type-is-derived "GdkDisplayManager"))
  (assert-false (g-type-is-fundamental "GdkDisplayManager"))
  (assert-true  (g-type-is-value-type "GdkDisplayManager"))
  (assert-true  (g-type-has-value-table "GdkDisplayManager"))
  (assert-true  (g-type-is-classed "GdkDisplayManager"))
  (assert-true  (g-type-is-instantiatable "GdkDisplayManager"))
  (assert-true  (g-type-is-derivable "GdkDisplayManager"))
  (assert-true  (g-type-is-deep-derivable "GdkDisplayManager"))
  (assert-false (g-type-is-interface "GdkDisplayManager"))

  ;; Check the registered name
  (assert-eq 'gdk-display-manager
             (registered-object-type-by-name "GdkDisplayManager"))

  (let ((class (g-type-class-ref (gtype "GdkDisplayManager"))))
    (assert-equal (gtype "GdkDisplayManager") (g-type-from-class class))
    (assert-equal (gtype "GdkDisplayManager") (g-object-class-type class))
    (assert-equal "GdkDisplayManager" (g-object-class-name class))
    (assert-equal (gtype "GdkDisplayManager")
                  (g-type-from-class (g-type-class-peek "GdkDisplayManager")))
    (assert-equal (gtype "GdkDisplayManager")
                  (g-type-from-class
                    (g-type-class-peek-static "GdkDisplayManager")))
    (g-type-class-unref class))

  (let ((class (find-class 'gdk-display-manager)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-display-manager (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkDisplayManager" (gobject-class-g-type-name class))
    (assert-equal "GdkDisplayManager" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_display_manager_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))

  (assert-equal (gtype "GObject") (g-type-parent "GdkDisplayManager"))
  (assert-eql 2 (g-type-depth "GdkDisplayManager"))
  (assert-eql   (gtype "GdkDisplayManager")
                (g-type-next-base "GdkDisplayManager" "GObject"))
  (assert-true  (g-type-is-a "GdkDisplayManager" "GdkDisplayManager"))
  (assert-true  (g-type-is-a "GdkDisplayManager" "GObject"))
  (assert-false (g-type-is-a "GdkDisplayManager" "gboolean"))
  (assert-false (g-type-is-a "GdkDisplayManager" "GtkWindow"))
  (assert-equal '("GdkWin32DisplayManager")
                (mapcar #'gtype-name (g-type-children "GdkDisplayManager")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkDisplayManager")))

  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query  "GdkDisplayManager" query)
    (assert-equal  (gtype "GdkDisplayManager")
                   (foreign-slot-value query 'g-type-query :type))
    (assert-equal  "GdkDisplayManager"
                   (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 108 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  12 (foreign-slot-value query 'g-type-query :instance-size)))

  ;; Get the names of the class properties
  (assert-equal
      '("default-display")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkDisplayManager"))))

  (let* ((display-manager (gdk-display-manager-get))
         (display (gdk-display-manager-get-default-display display-manager)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkWin32DisplayManager")
                  (g-object-type display-manager))
    (assert-equal "GdkWin32DisplayManager" (g-object-type-name display-manager))
    (assert-true (g-type-is-a "GdkWin32DisplayManager"
                              (g-type-from-instance (pointer display-manager))))
    (assert-true (g-type-is-a "GdkWin32Display"
                              (g-type-from-instance (pointer display))))
    ;; Access the properties
    (assert-eq 'gdk-display
               (type-of (gdk-display-manager-default-display display-manager)))
    ;; Check some functions
  ))

;;; --- End of file gdk.display-manager.lisp -----------------------------------
