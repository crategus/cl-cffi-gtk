;;; ----------------------------------------------------------------------------
;;; rtest-gtk-range.lisp
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

(define-test gtk-range
  (assert-true  (g-type-is-abstract "GtkRange"))  
  (assert-true  (g-type-is-derived "GtkRange"))  
  (assert-false (g-type-is-fundamental "GtkRange"))  
  (assert-true  (g-type-is-value-type "GtkRange"))
  (assert-true  (g-type-has-value-table "GtkRange"))
  (assert-true  (g-type-is-classed "GtkRange"))  
  (assert-true  (g-type-is-instantiatable "GtkRange"))
  (assert-true  (g-type-is-derivable "GtkRange"))
  (assert-true  (g-type-is-deep-derivable "GtkRange"))
  (assert-false (g-type-is-interface "GtkRange"))
  
  (let ((class (g-type-class-ref (gtype "GtkRange"))))
    (assert-equal (gtype "GtkRange")  (g-type-from-class class))
    (assert-equal (gtype "GtkRange")
                  (g-type-from-class (g-type-class-peek "GtkRange")))
    (assert-equal (gtype "GtkRange")
                  (g-type-from-class (g-type-class-peek-static "GtkRange")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-range)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-range (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkRange" (gobject-class-g-type-name class))
    (assert-equal "GtkRange" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_range_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkWidget") (g-type-parent "GtkRange"))
  (assert-eql 4 (g-type-depth "GtkRange"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkRange" "GObject"))
  (assert-true  (g-type-is-a "GtkRange" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkRange" "gboolean"))
  (assert-false (g-type-is-a "GtkRange" "GtkWindow"))
  (assert-equal '("GtkScrollbar" "GtkScale")
                (mapcar #'gtype-name (g-type-children "GtkRange")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkRange")))
  
  ;; Query infos about the class "GtkRange"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkRange" query)
    (assert-equal (gtype "GtkRange")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkRange"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 456 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  20 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "name" "parent" "width-request" "height-request"
         "visible" "sensitive" "app-paintable" "can-focus" "has-focus"
         "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "no-show-all" "has-tooltip"
         "tooltip-markup" "tooltip-text" "window" "double-buffered" "halign"
         "valign" "margin-left" "margin-right" "margin-top" "margin-bottom"
         "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
         "adjustment" "inverted" "lower-stepper-sensitivity"
         "upper-stepper-sensitivity" "show-fill-level" "restrict-to-fill-level"
         "fill-level" "round-digits")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkRange"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "text-handle-height"
         "text-handle-width" "visited-link-color" "wide-separators"
         "window-dragging" "arrow-displacement-x" "arrow-displacement-y"
         "arrow-scaling" "slider-width" "stepper-size" "stepper-spacing"
         "trough-border" "trough-under-steppers")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkRange"))))
  
  ;; Because GtkRange is abstract, we create a GtkScrollbar
  (let* ((widget (gtk-scrollbar-new :horizontal
                                    (make-instance 'gtk-adjustment
                                                   :value 1
                                                   :lower 0
                                                   :upper 2)))
         (ptr (pointer widget)))
    ;; Access the properties
    (assert-eq 'gtk-adjustment (type-of (gtk-range-adjustment widget)))
    (assert-eq 'double-float (type-of (gtk-range-fill-level widget)))
    (assert-false (gtk-range-inverted widget))
    (assert-eq :auto (gtk-range-lower-stepper-sensitivity widget))
    (assert-true (gtk-range-restrict-to-fill-level widget))
    (assert-eq -1 (gtk-range-round-digits widget))
    (assert-false (gtk-range-show-fill-level widget))
    (assert-eq :auto (gtk-range-upper-stepper-sensitivity widget))

    ;; Get the values of style properties
    (assert-eql 0 (gtk-widget-style-get-property ptr "arrow-displacement-x"))
    (assert-eql 0 (gtk-widget-style-get-property ptr "arrow-displacement-y"))
    (assert-eql 0.5 (gtk-widget-style-get-property ptr "arrow-scaling"))
    (assert-eql 14 (gtk-widget-style-get-property ptr "slider-width"))
    (assert-eql 13 (gtk-widget-style-get-property ptr "stepper-size"))
    (assert-eql 0 (gtk-widget-style-get-property ptr "stepper-spacing"))
    (assert-eql 0 (gtk-widget-style-get-property ptr "trough-border"))
    (assert-true (gtk-widget-style-get-property ptr "trough-under-steppers"))
    
    ;; Call functions
    (assert-eq 'double-float (type-of (gtk-range-get-fill-level widget)))
    (assert-true (gtk-range-get-restrict-to-fill-level widget))
    (assert-false (gtk-range-get-show-fill-level widget))
    (assert-eql 1.0 (gtk-range-set-fill-level widget 1.0))
    (assert-true (gtk-range-set-restrict-to-fill-level widget t))
    (assert-false (gtk-range-set-show-fill-level widget nil))
    (assert-eq 'gtk-adjustment (type-of (gtk-range-get-adjustment widget)))
    ;; gtk_range_set_adjustment
    (assert-eq 'gtk-adjustment
               (type-of (gtk-range-set-adjustment widget
                                                  (make-instance 'gtk-adjustment
                                                                 :value 1
                                                                 :lower 0
                                                                 :upper 2))))
    (assert-false (gtk-range-get-inverted widget))
    (assert-false (gtk-range-set-inverted widget nil))
    (assert-eql 1.0d0 (gtk-range-get-value widget))
    (assert-eql 1 (gtk-range-set-value widget 1))
    (assert-eql 1 (gtk-range-set-increments widget 1 2))
    (assert-eql 2 (gtk-range-set-range widget 0 2))
    (assert-eql -1 (gtk-range-get-round-digits widget))
    (assert-eql 2 (gtk-range-set-round-digits widget 2))
    ;;
    (assert-eq :auto (gtk-range-set-lower-stepper-sensitivity widget :auto))
    (assert-eq :auto (gtk-range-get-lower-stepper-sensitivity widget))
    (assert-eq :auto (gtk-range-set-upper-stepper-sensitivity widget :auto))
    (assert-eq :auto (gtk-range-get-upper-stepper-sensitivity widget))
    (assert-false (gtk-range-get-flippable widget))
    (assert-false (gtk-range-set-flippable widget nil))
    (assert-eql 1 (gtk-range-get-min-slider-size widget))
    (assert-eq 'gdk-rectangle (type-of (gtk-range-get-range-rect widget)))
    (assert-equal (values 0 1) (gtk-range-get-slider-range widget))
    (assert-false (gtk-range-get-slider-size-fixed widget))
    (assert-false (gtk-range-set-min-slider-size widget 1))
    (assert-false (gtk-range-set-slider-size-fixed widget nil))
    )
)

;;; --- End of file rtest-gtk-range.lisp ---------------------------------------
