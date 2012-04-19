;;; ----------------------------------------------------------------------------
;;; rtest-gtk-grid.lisp
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

(define-test gtk-grid
  (assert-false (g-type-is-abstract "GtkGrid"))  
  (assert-true  (g-type-is-derived "GtkGrid"))  
  (assert-false (g-type-is-fundamental "GtkGrid"))  
  (assert-true  (g-type-is-value-type "GtkGrid"))
  (assert-true  (g-type-has-value-table "GtkGrid"))
  (assert-true  (g-type-is-classed "GtkGrid"))  
  (assert-true  (g-type-is-instantiatable "GtkGrid"))
  (assert-true  (g-type-is-derivable "GtkGrid"))
  (assert-true  (g-type-is-deep-derivable "GtkGrid"))
  (assert-false (g-type-is-interface "GtkGrid"))
  
  (let ((class (g-type-class-ref (gtype "GtkGrid"))))
    (assert-equal (gtype "GtkGrid")  (g-type-from-class class))
    (assert-equal (gtype "GtkGrid")
                  (g-type-from-class (g-type-class-peek "GtkGrid")))
    (assert-equal (gtype "GtkGrid")
                  (g-type-from-class (g-type-class-peek-static "GtkGrid")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-grid)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-grid (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkGrid" (gobject-class-g-type-name class))
    (assert-equal "GtkGrid" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_grid_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkContainer") (g-type-parent "GtkGrid"))
  (assert-eql 5 (g-type-depth "GtkGrid"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkGrid" "GObject"))
  (assert-true  (g-type-is-a "GtkGrid" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkGrid" "gboolean"))
  (assert-false (g-type-is-a "GtkGrid" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkGrid")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkGrid")))
  
  ;; Query infos about the class "GtkTable"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkGrid" query)
    (assert-equal (gtype "GtkGrid")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkGrid"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 520 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  24 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "name" "parent" "width-request" "height-request"
         "visible" "sensitive" "app-paintable" "can-focus" "has-focus"
         "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "no-show-all" "has-tooltip"
         "tooltip-markup" "tooltip-text" "window" "double-buffered" "halign"
         "valign" "margin-left" "margin-right" "margin-top" "margin-bottom"
         "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
         "border-width" "resize-mode" "child" "row-spacing" "column-spacing"
         "row-homogeneous" "column-homogeneous")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkGrid"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkGrid")))))


