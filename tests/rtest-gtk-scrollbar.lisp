;;; ----------------------------------------------------------------------------
;;; rtest-gtk-scrollbar.lisp
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

(define-test gtk-scrollbar
  (assert-false (g-type-is-abstract "GtkScrollbar"))  
  (assert-true  (g-type-is-derived "GtkScrollbar"))  
  (assert-false (g-type-is-fundamental "GtkScrollbar"))  
  (assert-true  (g-type-is-value-type "GtkScrollbar"))
  (assert-true  (g-type-has-value-table "GtkScrollbar"))
  (assert-true  (g-type-is-classed "GtkScrollbar"))  
  (assert-true  (g-type-is-instantiatable "GtkScrollbar"))
  (assert-true  (g-type-is-derivable "GtkScrollbar"))
  (assert-true  (g-type-is-deep-derivable "GtkScrollbar"))
  (assert-false (g-type-is-interface "GtkScrollbar"))
  
  (let ((class (g-type-class-ref (gtype "GtkScrollbar"))))
    (assert-equal (gtype "GtkScrollbar")  (g-type-from-class class))
    (assert-equal (gtype "GtkScrollbar")
                  (g-type-from-class (g-type-class-peek "GtkScrollbar")))
    (assert-equal (gtype "GtkScrollbar")
                  (g-type-from-class (g-type-class-peek-static "GtkScrollbar")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-scrollbar)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-scrollbar (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkScrollbar" (gobject-class-g-type-name class))
    (assert-equal "GtkScrollbar" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_scrollbar_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkRange") (g-type-parent "GtkScrollbar"))
  (assert-eql 5 (g-type-depth "GtkScrollbar"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkScrollbar" "GObject"))
  (assert-true  (g-type-is-a "GtkScrollbar" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkScrollbar" "gboolean"))
  (assert-false (g-type-is-a "GtkScrollbar" "GtkWindow"))
  (assert-equal '("GtkHScrollbar" "GtkVScrollbar")
                (mapcar #'gtype-name (g-type-children "GtkScrollbar")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkScrollbar")))
  
  ;; Query infos about the class "GtkScrollbar"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkScrollbar" query)
    (assert-equal (gtype "GtkScrollbar")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkScrollbar"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 472 (foreign-slot-value query 'g-type-query :class-size))
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
             (g-object-class-list-properties (gtype "GtkScrollbar"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "text-handle-height"
         "text-handle-width" "visited-link-color" "wide-separators"
         "window-dragging" "arrow-displacement-x" "arrow-displacement-y"
         "arrow-scaling" "slider-width" "stepper-size" "stepper-spacing"
         "trough-border" "trough-under-steppers" "fixed-slider-length"
         "has-backward-stepper" "has-forward-stepper"
         "has-secondary-backward-stepper" "has-secondary-forward-stepper"
         "min-slider-length")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkScrollbar"))))
  
  (let* ((widget (gtk-scrollbar-new :horizontal
                                    (make-instance 'gtk-adjustment
                                                   :value 1
                                                   :lower 0
                                                   :upper 2)))
         (ptr (pointer widget)))
    ;; Access the properties
    ;; GtkScrollbar has no new properties
    
    ;; Get the values of style properties
    (assert-false  (gtk-widget-style-get-property ptr "fixed-slider-length"))
    (assert-false  (gtk-widget-style-get-property ptr "has-backward-stepper"))
    (assert-false  (gtk-widget-style-get-property ptr "has-forward-stepper"))
    (assert-false  (gtk-widget-style-get-property ptr "has-secondary-backward-stepper"))
    (assert-false  (gtk-widget-style-get-property ptr "has-secondary-forward-stepper"))
    (assert-eql 31 (gtk-widget-style-get-property ptr "min-slider-length"))
    
    ;; Call functions
    ;; GtkScrollbar has no own functions
    
    ;; Check a value of NIL for the argument adjustment
    (let ((scrollbar (gtk-scrollbar-new :horizontal nil)))
      (assert-eq 'gtk-adjustment
                 (type-of (gtk-range-get-adjustment scrollbar))))
        
    ;; Check constructor of GtkHScrollbar and GtkVScrollbar
    ;; Both classes are deprecated, but present in GTK+ 3.2. The functions
    ;; gtk-hscrollbar-new and gtk-vscrollbar-new create an instance of type
    ;; GtkScrollbar.
    (assert-equal 'gtk-scrollbar
                  (type-of (gtk-hscrollbar-new (make-instance 'gtk-adjustment))))
    (assert-equal 'gtk-scrollbar
                  (type-of (gtk-vscrollbar-new (make-instance 'gtk-adjustment))))
    ))

;;; --- End of file rtest-gtk-scrollbar.lisp -----------------------------------
