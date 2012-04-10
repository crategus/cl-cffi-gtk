;;; ----------------------------------------------------------------------------
;;; rtest-gtk-cell-renderer-progress.lisp
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

(define-test gtk-cell-renderer-progress
  (assert-false (g-type-is-abstract "GtkCellRendererProgress"))  
  (assert-true  (g-type-is-derived "GtkCellRendererProgress"))  
  (assert-false (g-type-is-fundamental "GtkCellRendererProgress"))  
  (assert-true  (g-type-is-value-type "GtkCellRendererProgress"))
  (assert-true  (g-type-has-value-table "GtkCellRendererProgress"))
  (assert-true  (g-type-is-classed "GtkCellRendererProgress"))  
  (assert-true  (g-type-is-instantiatable "GtkCellRendererProgress"))
  (assert-true  (g-type-is-derivable "GtkCellRendererProgress"))
  (assert-true  (g-type-is-deep-derivable "GtkCellRendererProgress"))
  (assert-false (g-type-is-interface "GtkCellRendererProgress"))
  
  (let ((class (g-type-class-ref (gtype "GtkCellRendererProgress"))))
    (assert-equal (gtype "GtkCellRendererProgress")  (g-type-from-class class))
    (assert-equal (gtype "GtkCellRendererProgress")
                  (g-type-from-class (g-type-class-peek "GtkCellRendererProgress")))
    (assert-equal (gtype "GtkCellRendererProgress")
                  (g-type-from-class (g-type-class-peek-static "GtkCellRendererProgress")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-cell-renderer-progress)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-cell-renderer-progress (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkCellRendererProgress" (gobject-class-g-type-name class))
    (assert-equal "GtkCellRendererProgress" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_cell_renderer_progress_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkCellRenderer") (g-type-parent "GtkCellRendererProgress"))
  (assert-eql 4 (g-type-depth "GtkCellRendererProgress"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkCellRendererProgress" "GObject"))
  (assert-true  (g-type-is-a "GtkCellRendererProgress" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkCellRendererProgress" "gboolean"))
  (assert-false (g-type-is-a "GtkCellRendererProgress" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkCellRendererProgress")))
  (assert-equal '("GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkCellRendererProgress")))
  
  ;; Query infos about the class "GtkTable"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkCellRendererProgress" query)
    (assert-equal (gtype "GtkCellRendererProgress")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkCellRendererProgress"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 148 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  20 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "mode" "visible" "sensitive" "xalign" "yalign" "xpad"
         "ypad" "width" "height" "is-expander" "is-expanded" "cell-background"
         "cell-background-gdk" "cell-background-rgba" "cell-background-set"
         "editing" "value" "text" "pulse" "text-xalign" "text-yalign"
         "inverted")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkCellRendererProgress"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '()
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkCellRendererProgress"))))
  ;; Read default values of properties
  (let ((renderer   (gtk-cell-renderer-progress-new)))
    (assert-false   (gtk-cell-renderer-progress-inverted renderer))
    (assert-eql -1  (gtk-cell-renderer-progress-pulse renderer))
    (assert-false   (gtk-cell-renderer-progress-text renderer))
    (assert-eql 0.5 (gtk-cell-renderer-progress-text-xalign renderer))
    (assert-eql 0.5 (gtk-cell-renderer-progress-text-yalign renderer))
    (assert-eql 0   (gtk-cell-renderer-progress-value renderer)))
)

