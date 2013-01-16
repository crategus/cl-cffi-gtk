;;; ----------------------------------------------------------------------------
;;; rtest-gdk-keymap.lisp
;;;
;;; Copyright (C) 2012 - 2013 Dieter Kaiser
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Initialize the type GdkX11Keymap
  (foreign-funcall "gdk_x11_keymap_get_type" :int))

#-windows
(define-test gdk-keymap
  (assert-true  (g-type-is-object "GdkKeymap"))
  (assert-false (g-type-is-abstract "GdkKeymap"))
  (assert-true  (g-type-is-derived "GdkKeymap"))
  (assert-false (g-type-is-fundamental "GdkKeymap"))
  (assert-true  (g-type-is-value-type "GdkKeymap"))
  (assert-true  (g-type-has-value-table "GdkKeymap"))
  (assert-true  (g-type-is-classed "GdkKeymap"))
  (assert-true  (g-type-is-instantiatable "GdkKeymap"))
  (assert-true  (g-type-is-derivable "GdkKeymap"))
  (assert-true  (g-type-is-deep-derivable "GdkKeymap"))
  (assert-false (g-type-is-interface "GdkKeymap"))

  ;; Check the registered name
  (assert-eq 'gdk-keymap
             (registered-object-type-by-name "GdkKeymap"))
  
  (let ((class (g-type-class-ref (gtype "GdkKeymap"))))
    (assert-equal (gtype "GdkKeymap")  (g-type-from-class class))
    (assert-equal (gtype "GdkKeymap") (g-object-class-type class))
    (assert-equal "GdkKeymap" (g-object-class-name class))
    (assert-equal (gtype "GdkKeymap")
                  (g-type-from-class (g-type-class-peek "GdkKeymap")))
    (assert-equal (gtype "GdkKeymap")
                  (g-type-from-class (g-type-class-peek-static "GdkKeymap")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-keymap)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-keymap (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkKeymap" (gobject-class-g-type-name class))
    (assert-equal "GdkKeymap" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_keymap_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GdkKeymap"))
  (assert-eql 2 (g-type-depth "GdkKeymap"))
  (assert-eql   (gtype "GdkKeymap")
                (g-type-next-base "GdkKeymap" "GObject"))
  (assert-true  (g-type-is-a "GdkKeymap" "GObject"))
  (assert-false (g-type-is-a "GdkKeymap" "gboolean"))
  (assert-false (g-type-is-a "GdkKeymap" "GtkWindow"))
  (assert-equal '("GdkX11Keymap")
                (mapcar #'gtype-name (g-type-children "GdkKeymap")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkKeymap")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkKeymap" query)
    (assert-equal (gtype "GdkKeymap")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkKeymap"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 128 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  16 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties of "GdkDevice".
  (assert-equal
     '()
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkKeymap"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkKeymap" GDK-KEYMAP
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_keymap_get_type")
                               NIL)
     (get-g-type-definition (gtype "GdkKeymap")))

  ;; Check functions
  (let ((keymap (gdk-keymap-get-default)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11Keymap") (g-object-type keymap))
    (assert-equal "GdkX11Keymap" (g-object-type-name keymap))
    (assert-true (g-type-is-a "GdkX11Keymap"
                              (g-type-from-instance (pointer keymap))))

   ;; Call functions
   (assert-true  (gdk-keymap-get-direction keymap))
   (assert-false (gdk-keymap-have-bidi-layouts keymap))
   (assert-false (gdk-keymap-get-caps-lock-state keymap))
   (assert-false (gdk-keymap-get-num-lock-state keymap))
   (assert-false (gdk-keymap-get-modifier-state keymap))
   (assert-equal "A" (gdk-keyval-name 65))
   (assert-eql 65 (gdk-keyval-from-name "A"))
   (assert-equal (values 97 65) (gdk-keyval-convert-case 65))
   (assert-eql 65 (gdk-keyval-to-upper 97))
   (assert-eql 97 (gdk-keyval-to-lower 65)))
)

#+windows
(define-test gdk-keymap
  (assert-true  (g-type-is-object "GdkKeymap"))
  (assert-false (g-type-is-abstract "GdkKeymap"))
  (assert-true  (g-type-is-derived "GdkKeymap"))
  (assert-false (g-type-is-fundamental "GdkKeymap"))
  (assert-true  (g-type-is-value-type "GdkKeymap"))
  (assert-true  (g-type-has-value-table "GdkKeymap"))
  (assert-true  (g-type-is-classed "GdkKeymap"))
  (assert-true  (g-type-is-instantiatable "GdkKeymap"))
  (assert-true  (g-type-is-derivable "GdkKeymap"))
  (assert-true  (g-type-is-deep-derivable "GdkKeymap"))
  (assert-false (g-type-is-interface "GdkKeymap"))

  ;; Check the registered name
  (assert-eq 'gdk-keymap
             (registered-object-type-by-name "GdkKeymap"))
  
  (let ((class (g-type-class-ref (gtype "GdkKeymap"))))
    (assert-equal (gtype "GdkKeymap")  (g-type-from-class class))
    (assert-equal (gtype "GdkKeymap") (g-object-class-type class))
    (assert-equal "GdkKeymap" (g-object-class-name class))
    (assert-equal (gtype "GdkKeymap")
                  (g-type-from-class (g-type-class-peek "GdkKeymap")))
    (assert-equal (gtype "GdkKeymap")
                  (g-type-from-class (g-type-class-peek-static "GdkKeymap")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-keymap)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-keymap (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkKeymap" (gobject-class-g-type-name class))
    (assert-equal "GdkKeymap" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_keymap_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GdkKeymap"))
  (assert-eql 2 (g-type-depth "GdkKeymap"))
  (assert-eql   (gtype "GdkKeymap")
                (g-type-next-base "GdkKeymap" "GObject"))
  (assert-true  (g-type-is-a "GdkKeymap" "GObject"))
  (assert-false (g-type-is-a "GdkKeymap" "gboolean"))
  (assert-false (g-type-is-a "GdkKeymap" "GtkWindow"))
  (assert-equal '("GdkWin32Keymap")
                (mapcar #'gtype-name (g-type-children "GdkKeymap")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkKeymap")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkKeymap" query)
    (assert-equal (gtype "GdkKeymap")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkKeymap"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 128 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  16 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties of "GdkDevice".
  (assert-equal
     '()
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkKeymap"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkKeymap" GDK-KEYMAP
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_keymap_get_type")
                               NIL)
     (get-g-type-definition (gtype "GdkKeymap")))

  ;; Check functions
  (let ((keymap (gdk-keymap-get-default)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkWin32Keymap") (g-object-type keymap))
    (assert-equal "GdkWin32Keymap" (g-object-type-name keymap))
    (assert-true (g-type-is-a "GdkWin32Keymap"
                              (g-type-from-instance (pointer keymap))))

   ;; Call functions
   (assert-true  (gdk-keymap-get-direction keymap))
   (assert-false (gdk-keymap-have-bidi-layouts keymap))
   (assert-false (gdk-keymap-get-caps-lock-state keymap))
   (assert-true (gdk-keymap-get-num-lock-state keymap))
   (assert-false (gdk-keymap-get-modifier-state keymap))
   (assert-equal "A" (gdk-keyval-name 65))
   (assert-eql 65 (gdk-keyval-from-name "A"))
   (assert-equal (values 97 65) (gdk-keyval-convert-case 65))
   (assert-eql 65 (gdk-keyval-to-upper 97))
   (assert-eql 97 (gdk-keyval-to-lower 65)))
)

;;; --- End of file rtest-gdk-keymap.lisp --------------------------------------
