;;; ----------------------------------------------------------------------------
;;; rtest-gtk-adjustment.lisp
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

(in-package :gtk-tests)

(define-test gtk-adjustment
  (assert-false (g-type-is-abstract "GtkAdjustment"))
  (assert-true  (g-type-is-derived "GtkAdjustment"))
  (assert-false (g-type-is-fundamental "GtkAdjustment"))
  (assert-true  (g-type-is-value-type "GtkAdjustment"))
  (assert-true  (g-type-has-value-table "GtkAdjustment"))
  (assert-true  (g-type-is-classed "GtkAdjustment"))
  (assert-true  (g-type-is-instantiatable "GtkAdjustment"))
  (assert-true  (g-type-is-derivable "GtkAdjustment"))
  (assert-true  (g-type-is-deep-derivable "GtkAdjustment"))
  (assert-false (g-type-is-interface "GtkAdjustment"))
  
  (let ((class (g-type-class-ref (gtype "GtkAdjustment"))))
    (assert-equal (gtype "GtkAdjustment") (g-type-from-class class))
    (assert-equal (gtype "GtkAdjustment")
                  (g-type-from-class (g-type-class-peek "GtkAdjustment")))
    (assert-equal (gtype "GtkAdjustment")
                  (g-type-from-class (g-type-class-peek-static "GtkAdjustment")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-adjustment)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-adjustment (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkAdjustment" (gobject-class-g-type-name class))
    (assert-equal "GtkAdjustment" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_adjustment_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkObject") (g-type-parent "GtkAdjustment"))
  (assert-eql 4 (g-type-depth "GtkAdjustment"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkAdjustment" "GObject"))
  (assert-true  (g-type-is-a "GtkAdjustment" "GtkAdjustment"))
  (assert-true  (g-type-is-a "GtkAdjustment" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkAdjustment" "gboolean"))
  (assert-false (g-type-is-a "GtkAdjustment" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkAdjustment")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GtkAdjustment")))
  
  ;; Query infos about the class "GtkAdjustment"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkAdjustment" query)
    (assert-equal (gtype "GtkAdjustment")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkAdjustment"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 104 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  64 (foreign-slot-value query 'g-type-query :instance-size)))
    
    ;; Get the names of the class properties.
    (assert-equal
        '("user-data" "value" "lower" "upper" "step-increment" "page-increment"
         "page-size")
       (mapcar #'g-class-property-definition-name
               (g-object-class-list-properties (gtype "GtkAdjustment"))))
    
    ;; Get the names of the style properties.
    (assert-equal
        '()
        (mapcar #'g-class-property-definition-name
                (gtk-widget-class-list-style-properties (gtype "GtkAdjustment"))))
                
    (let ((adj (make-instance 'gtk-adjustment)))
      ;; Check the default values and the accessor functions
      (assert-eql 0.0d0 (gtk-adjustment-get-value adj))
      (assert-eql 0.0d0 (gtk-adjustment-get-lower adj))
      (assert-eql 0.0d0 (gtk-adjustment-get-upper adj))
      (assert-eql 0.0d0 (gtk-adjustment-get-step-increment adj))
      (assert-eql 0.0d0 (gtk-adjustment-get-page-increment adj))
      (assert-eql 0.0d0 (gtk-adjustment-get-page-size adj))
      
      ;; Set new values and read the values
      (gtk-adjustment-set-lower adj 1.0d0)
      (assert-eql 1.0d0 (gtk-adjustment-get-lower adj))
      (gtk-adjustment-set-upper adj 2.0d0)
      (assert-eql 2.0d0 (gtk-adjustment-get-upper adj))
      ;; value must be in (lower, upper)
      (gtk-adjustment-set-value adj 1.5d0)
      (assert-eql 1.5d0 (gtk-adjustment-get-value adj))
      (gtk-adjustment-set-step-increment adj 3.0d0)
      (assert-eql 3.0d0 (gtk-adjustment-get-step-increment adj))
      (gtk-adjustment-set-page-increment adj 4.0d0)
      (assert-eql 4.0d0 (gtk-adjustment-get-page-increment adj))
      (gtk-adjustment-set-page-size adj 5.0d0)
      (assert-eql 5.0d0 (gtk-adjustment-get-page-size adj))
      
      ;; Create an adjustment with gtk-adjustment-new
      (setq adj (gtk-adjustment-new 1.5d0 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))
      (assert-eql 1.0d0 (gtk-adjustment-get-lower adj))
      (assert-eql 2.0d0 (gtk-adjustment-get-upper adj))
      (assert-eql 1.5d0 (gtk-adjustment-get-value adj))
      (assert-eql 3.0d0 (gtk-adjustment-get-step-increment adj))
      (assert-eql 4.0d0 (gtk-adjustment-get-page-increment adj))
      (assert-eql 5.0d0 (gtk-adjustment-get-page-size adj))
      
      ;; Configure the adjustment
      (gtk-adjustment-configure adj 2.5d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0)
      (assert-eql 2.0d0 (gtk-adjustment-get-lower adj))
      (assert-eql 3.0d0 (gtk-adjustment-get-upper adj))
      ;; The value is 2.0d0 and not 2.5d0!
      (assert-eql 2.0d0 (gtk-adjustment-get-value adj))
      (assert-eql 4.0d0 (gtk-adjustment-get-step-increment adj))
      (assert-eql 5.0d0 (gtk-adjustment-get-page-increment adj))
      (assert-eql 6.0d0 (gtk-adjustment-get-page-size adj))
    )
  )


