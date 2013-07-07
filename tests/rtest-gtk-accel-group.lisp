;;; ----------------------------------------------------------------------------
;;; rtest-gtk-accel-group.lisp
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

(in-package :gtk-tests)

(define-test gtk-accel-group
  ;; Type checks
  (assert-false (g-type-is-abstract "GtkAccelGroup"))
  (assert-true  (g-type-is-derived "GtkAccelGroup"))
  (assert-false (g-type-is-fundamental "GtkAccelGroup"))
  (assert-true  (g-type-is-value-type "GtkAccelGroup"))
  (assert-true  (g-type-has-value-table "GtkAccelGroup"))
  (assert-true  (g-type-is-classed "GtkAccelGroup"))
  (assert-true  (g-type-is-instantiatable "GtkAccelGroup"))
  (assert-true  (g-type-is-derivable "GtkAccelGroup"))
  (assert-true  (g-type-is-deep-derivable "GtkAccelGroup"))
  (assert-false (g-type-is-interface "GtkAccelGroup"))

  ;; Check the registered name
  (assert-eq 'gtk-accel-group
             (registered-object-type-by-name "GtkAccelGroup"))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkAccelGroup"))))
    (assert-equal (gtype "GtkAccelGroup") (g-type-from-class class))
    (assert-equal (gtype "GtkAccelGroup")
                  (g-type-from-class  (g-type-class-peek "GtkAccelGroup")))
    (assert-equal (gtype "GtkAccelGroup")
                  (g-type-from-class (g-type-class-peek-static "GtkAccelGroup")))
    (g-type-class-unref class))

  ;; Checks for Gobjects
  (let* ((object (make-instance 'gtk-accel-group))
         (class (g-object-get-class object)))
    (assert-true  (g-type-is-object "GtkAccelGroup"))
    (assert-true  (g-is-object object))
    (assert-true  (g-is-object-class class))
    (assert-equal (gtype "GtkAccelGroup")
                  (g-type-from-class (g-object-get-class object)))
    (assert-equal (gtype "GtkAccelGroup") (g-object-type object))
    (assert-equal "GtkAccelGroup" (g-object-type-name object))
    (assert-equal (gtype "GtkAccelGroup") (g-object-class-type class))
    (assert-equal "GtkAccelGroup" (g-object-class-name class)))
  
  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-accel-group)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-accel-group (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkAccelGroup" (gobject-class-g-type-name class))
    (assert-equal "GtkAccelGroup" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_accel_group_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  ;; Check some more GType information
  (assert-equal (gtype "GObject") (g-type-parent "GtkAccelGroup"))
  (assert-eql 2 (g-type-depth "GtkAccelGroup"))
  (assert-eql   (gtype "GtkAccelGroup")
                (g-type-next-base "GtkAccelGroup" "GObject"))
  (assert-true  (g-type-is-a "GtkAccelGroup" "GObject"))

  ;; Check the children
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkAccelGroup")))
  ;; Check the interfaces
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GtkAccelGroup")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkAccelGroup" query)
    (assert-equal (gtype "GtkAccelGroup")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkAccelGroup"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 88 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 16 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("is-locked" "modifier-mask")
     (mapcar #'param-spec-name
             (g-object-class-list-properties "GtkAccelGroup")))  

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GtkAccelGroup" GTK-ACCEL-GROUP
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_accel_group_get_type")
                               ((IS-LOCKED GTK-ACCEL-GROUP-IS-LOCKED
                                 "is-locked" "gboolean" T NIL)
                                (MODIFIER-MASK GTK-ACCEL-GROUP-MODIFIER-MASK
                                 "modifier-mask" "GdkModifierType" T NIL)))
     (get-g-type-definition "GtkAccelGroup"))

  ;; Check the Accessors of Properties
  (let ((group (gtk-accel-group-new)))
    ;; gtk-accel-group-is-locked
    (assert-eql nil (gtk-accel-group-is-locked group))
    (assert-eql nil (gtk-accel-group-get-is-locked group))
    ;; gtk-accel-group-modifier-mask
    (assert-equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK
                    :HYPER-MASK :META-MASK)
                  (gtk-accel-group-modifier-mask group))
    (assert-equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK
                    :HYPER-MASK :META-MASK)
                  (gtk-accel-group-get-modifier-mask group)))

)

;;; --- End of file rtest-gtk-accel-group.lisp ---------------------------------
