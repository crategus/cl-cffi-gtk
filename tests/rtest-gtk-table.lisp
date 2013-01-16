;;; ----------------------------------------------------------------------------
;;; rtest-gtk-table.lisp
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

(define-test gtk-table-class
  (assert-false (g-type-is-abstract "GtkTable"))  
  (assert-true  (g-type-is-derived "GtkTable"))  
  (assert-false (g-type-is-fundamental "GtkTable"))  
  (assert-true  (g-type-is-value-type "GtkTable"))
  (assert-true  (g-type-has-value-table "GtkTable"))
  (assert-true  (g-type-is-classed "GtkTable"))  
  (assert-true  (g-type-is-instantiatable "GtkTable"))
  (assert-true  (g-type-is-derivable "GtkTable"))
  (assert-true  (g-type-is-deep-derivable "GtkTable"))
  (assert-false (g-type-is-interface "GtkTable"))
  
  (let ((class (g-type-class-ref (gtype "GtkTable"))))
    (assert-equal (gtype "GtkTable")  (g-type-from-class class))
    (assert-equal (gtype "GtkTable")
                  (g-type-from-class (g-type-class-peek "GtkTable")))
    (assert-equal (gtype "GtkTable")
                  (g-type-from-class (g-type-class-peek-static "GtkTable")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-table)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-table (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkTable" (gobject-class-g-type-name class))
    (assert-equal "GtkTable" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_table_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkContainer") (g-type-parent "GtkTable"))
  (assert-eql 5 (g-type-depth "GtkTable"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkTable" "GObject"))
  (assert-true  (g-type-is-a "GtkTable" "GtkTable"))
  (assert-true  (g-type-is-a "GtkTable" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkTable" "gboolean"))
  (assert-false (g-type-is-a "GtkTable" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkTable")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable")
                (mapcar #'gtype-name (g-type-interfaces "GtkTable")))
  
  ;; Query infos about the class "GtkTable"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkTable" query)
    (assert-equal (gtype "GtkTable")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkTable"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 504 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  24 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("name" "parent" "width-request" "height-request" "visible" "sensitive"
         "app-paintable" "can-focus" "has-focus" "is-focus" "can-default"
         "has-default" "receives-default" "composite-child" "style" "events"
         "no-show-all" "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "halign" "valign" "margin-left" "margin-right"
         "margin-top" "margin-bottom" "margin" "hexpand" "vexpand"
         "hexpand-set" "vexpand-set" "expand" "border-width" "resize-mode"
         "child" "n-rows" "n-columns" "column-spacing" "row-spacing"
         "homogeneous")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkTable"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "text-handle-height"
         "text-handle-width" "visited-link-color" "wide-separators"
         "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkTable")))))

(define-test gtk-table-functions
  (let ((table (make-instance 'gtk-table)))
    (assert-equal (values 1 1) (gtk-table-get-size table))
    (assert-eql 0 (gtk-table-get-default-row-spacing table))
    (assert-eql 0 (gtk-table-get-default-col-spacing table))
    (assert-false (gtk-table-get-homogeneous table))
    
    (gtk-table-resize table 3 2)
    (assert-equal (values 3 2) (gtk-table-get-size table))
    
    (assert-eql 5 (gtk-table-set-row-spacings table 5))
    (assert-eql 5 (gtk-table-set-col-spacings table 5))
    
    (assert-eql 5 (gtk-table-get-row-spacing table 0))
    (assert-eql 5 (gtk-table-get-col-spacing table 0))
    
    (assert-true  (gtk-table-set-homogeneous table t))
    (assert-true  (gtk-table-get-homogeneous table))
    ))

