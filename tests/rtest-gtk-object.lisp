;;; ----------------------------------------------------------------------------
;;; rtest-gtk-label.lisp
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

(in-package :gtk-tests)

(define-test gtk-object
  (assert-true (g-type-is-abstract "GtkObject"))
  (assert-true (g-type-is-abstract (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-abstract (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-is-derived "GtkObject"))
  (assert-true (g-type-is-derived (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-derived (gtype-id (gtype "GtkObject"))))
  
  (assert-false (g-type-is-fundamental "GtkObject"))
  (assert-false (g-type-is-fundamental (gtype-name (gtype "GtkObject"))))
  (assert-false (g-type-is-fundamental (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-is-value-type "GtkObject"))
  (assert-true (g-type-is-value-type (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-value-type (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-has-value-table "GtkObject"))
  (assert-true (g-type-has-value-table (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-has-value-table (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-is-classed "GtkObject"))
  (assert-true (g-type-is-classed (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-classed (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-is-instantiatable "GtkObject"))
  (assert-true (g-type-is-instantiatable (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-instantiatable (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-is-derivable "GtkObject"))
  (assert-true (g-type-is-derivable (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-derivable (gtype-id (gtype "GtkObject"))))
  
  (assert-true (g-type-is-deep-derivable "GtkObject"))
  (assert-true (g-type-is-deep-derivable (gtype-name (gtype "GtkObject"))))
  (assert-true (g-type-is-deep-derivable (gtype-id (gtype "GtkObject"))))
  
  (assert-false (g-type-is-interface "GtkObject"))
  (assert-false (g-type-is-interface (gtype-name (gtype "GtkObject"))))
  (assert-false (g-type-is-interface (gtype-id (gtype "GtkObject"))))
  
  (let ((class (g-type-class-ref (gtype "GtkObject"))))
    (assert-equal (gtype "GtkObject")  (g-type-from-class class))
    (assert-equal (gtype "GtkObject")
                  (g-type-from-class (g-type-class-peek "GtkObject")))
    (assert-equal (gtype "GtkObject")
                  (g-type-from-class (g-type-class-peek-static "GtkObject")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-object)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-object (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkObject" (gobject-class-g-type-name class))
    (assert-equal "GtkObject" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_object_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GInitiallyUnowned") (g-type-parent "GtkObject"))
  (assert-eql 3 (g-type-depth "GtkObject"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkObject" "GObject"))
  (assert-true  (g-type-is-a "GtkObject" "GObject"))
  (assert-true  (g-type-is-a "GtkObject" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkObject" "gboolean"))
  (assert-false (g-type-is-a "GtkObject" "GtkWindow"))
  (unordered-equal '("GtkWidget" "GtkTreeViewColumn" "GtkCellRenderer"
                     "GtkAdjustment" "GtkFileFilter" "GtkRecentFilter"
                     "GtkItemFactory")
                   (mapcar #'gtype-name (g-type-children "GtkObject")))
  (unordered-equal '()
                   (mapcar #'gtype-name (g-type-interfaces "GtkObject")))
  
  ;; Query infos about the class "GtkObject"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkObject" query)
    (assert-equal (gtype "GtkObject")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkObject"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 80 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 16 (foreign-slot-value query 'g-type-query :instance-size)))
    
    ;; Get the names of the class properties.
    (unordered-equal
        '("user-data")
       (mapcar #'param-spec-name
               (g-object-class-list-properties (gtype "GtkObject"))))
    
    ;; Get the names of the style properties.
    (unordered-equal
        '()
        (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkWidget"))))
  )

