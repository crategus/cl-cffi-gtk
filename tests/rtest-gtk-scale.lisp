;;; ----------------------------------------------------------------------------
;;; rtest-gtk-scale.lisp
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

(define-test gtk-scale
  (assert-true  (g-type-is-abstract "GtkScale"))  
  (assert-true  (g-type-is-derived "GtkScale"))  
  (assert-false (g-type-is-fundamental "GtkScale"))  
  (assert-true  (g-type-is-value-type "GtkScale"))
  (assert-true  (g-type-has-value-table "GtkScale"))
  (assert-true  (g-type-is-classed "GtkScale"))  
  (assert-true  (g-type-is-instantiatable "GtkScale"))
  (assert-true  (g-type-is-derivable "GtkScale"))
  (assert-true  (g-type-is-deep-derivable "GtkScale"))
  (assert-false (g-type-is-interface "GtkScale"))
  
  (let ((class (g-type-class-ref (gtype "GtkScale"))))
    (assert-equal (gtype "GtkScale")  (g-type-from-class class))
    (assert-equal (gtype "GtkScale")
                  (g-type-from-class (g-type-class-peek "GtkScale")))
    (assert-equal (gtype "GtkScale")
                  (g-type-from-class (g-type-class-peek-static "GtkScale")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-scale)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-scale (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkScale" (gobject-class-g-type-name class))
    (assert-equal "GtkScale" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_scale_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkRange") (g-type-parent "GtkScale"))
  (assert-eql 6 (g-type-depth "GtkScale"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkScale" "GObject"))
  (assert-true  (g-type-is-a "GtkScale" "GtkScale"))
  (assert-true  (g-type-is-a "GtkScale" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkScale" "gboolean"))
  (assert-false (g-type-is-a "GtkScale" "GtkWindow"))
  (assert-equal '("GtkHScale" "GtkVScale")
                (mapcar #'gtype-name (g-type-children "GtkScale")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkScale")))
  
  ;; Query infos about the class "GtkScale"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkScale" query)
    (assert-equal (gtype "GtkScale")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkScale"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 428 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 144 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "user-data" "name" "parent" "width-request"
         "height-request" "visible" "sensitive" "app-paintable" "can-focus"
         "has-focus" "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "extension-events" "no-show-all"
         "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "update-policy" "adjustment" "inverted"
         "lower-stepper-sensitivity" "upper-stepper-sensitivity"
         "show-fill-level" "restrict-to-fill-level" "fill-level" "round-digits"
         "digits" "draw-value" "value-pos")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkScale"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "draw-border"
         "focus-line-pattern" "focus-line-width" "focus-padding"
         "interior-focus" "link-color" "new-tooltip-style"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "activate-slider" "arrow-displacement-x"
         "arrow-displacement-y" "arrow-scaling" "slider-width"
         "stepper-position-details" "stepper-size" "stepper-spacing"
         "trough-border" "trough-side-details" "trough-under-steppers"
         "slider-length" "value-spacing")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkScale")))
    
    ;; Check the defintion of the class gtk-window
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkScale" GTK-SCALE
                       (:SUPERCLASS GTK-RANGE :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scale_get_type")
                       ((DIGITS GTK-SCALE-DIGITS "digits" "gint" T T)
                        (DRAW-VALUE GTK-SCALE-DRAW-VALUE "draw-value"
                         "gboolean" T T)
                        (VALUE-POS GTK-SCALE-VALUE-POS "value-pos"
                         "GtkPositionType" T T)))
     (get-g-class-definition (gtype "GtkScale")))
    
    ;; Check the expansion of the class definition
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-SCALE
           (GTK-RANGE ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE GTK-ORIENTABLE)
           ((DIGITS :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gint"
             :ACCESSOR GTK-SCALE-DIGITS :INITARG :DIGITS :G-PROPERTY-NAME
             "digits")
            (DRAW-VALUE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gboolean" :ACCESSOR GTK-SCALE-DRAW-VALUE :INITARG :DRAW-VALUE
             :G-PROPERTY-NAME "draw-value")
            (VALUE-POS :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GtkPositionType" :ACCESSOR GTK-SCALE-VALUE-POS :INITARG
             :VALUE-POS :G-PROPERTY-NAME "value-pos"))
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkScale")
           (:G-TYPE-INITIALIZER . "gtk_scale_get_type"))
         (EXPORT 'GTK-SCALE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-SCALE-DIGITS (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-SCALE-DRAW-VALUE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-SCALE-VALUE-POS (FIND-PACKAGE "GTK")))
     ;; macroexpand the class definition
     (macroexpand-1 (get-g-class-definition (gtype "GtkScale"))))))

(define-test gtk-hscale
  (assert-false (g-type-is-abstract "GtkHScale"))  
  (assert-true  (g-type-is-derived "GtkHScale"))  
  (assert-false (g-type-is-fundamental "GtkHScale"))  
  (assert-true  (g-type-is-value-type "GtkHScale"))
  (assert-true  (g-type-has-value-table "GtkHScale"))
  (assert-true  (g-type-is-classed "GtkHScale"))  
  (assert-true  (g-type-is-instantiatable "GtkHScale"))
  (assert-true  (g-type-is-derivable "GtkHScale"))
  (assert-true  (g-type-is-deep-derivable "GtkHScale"))
  (assert-false (g-type-is-interface "GtkHScale"))
  
  (let ((class (g-type-class-ref (gtype "GtkHScale"))))
    (assert-equal (gtype "GtkHScale")  (g-type-from-class class))
    (assert-equal (gtype "GtkHScale")
                  (g-type-from-class (g-type-class-peek "GtkHScale")))
    (assert-equal (gtype "GtkHScale")
                  (g-type-from-class (g-type-class-peek-static "GtkHScale")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-h-scale)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-h-scale (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkHScale" (gobject-class-g-type-name class))
    (assert-equal "GtkHScale" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_hscale_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkScale") (g-type-parent "GtkHScale"))
  (assert-eql 7 (g-type-depth "GtkHScale"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkHScale" "GObject"))
  (assert-true  (g-type-is-a "GtkHScale" "GtkHScale"))
  (assert-true  (g-type-is-a "GtkHScale" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkHScale" "gboolean"))
  (assert-false (g-type-is-a "GtkHScale" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkHScale")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkHScale")))
  
  ;; Query infos about the class "GtkScale"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkHScale" query)
    (assert-equal (gtype "GtkHScale")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkHScale"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 428 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 144 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "user-data" "name" "parent" "width-request"
         "height-request" "visible" "sensitive" "app-paintable" "can-focus"
         "has-focus" "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "extension-events" "no-show-all"
         "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "update-policy" "adjustment" "inverted"
         "lower-stepper-sensitivity" "upper-stepper-sensitivity"
         "show-fill-level" "restrict-to-fill-level" "fill-level" "round-digits"
         "digits" "draw-value" "value-pos")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkHScale"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "draw-border"
         "focus-line-pattern" "focus-line-width" "focus-padding"
         "interior-focus" "link-color" "new-tooltip-style"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "activate-slider" "arrow-displacement-x"
         "arrow-displacement-y" "arrow-scaling" "slider-width"
         "stepper-position-details" "stepper-size" "stepper-spacing"
         "trough-border" "trough-side-details" "trough-under-steppers"
         "slider-length" "value-spacing")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkHScale")))
    
    ;; Check the defintion of the class gtk-window
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkHScale" GTK-H-SCALE
                               (:SUPERCLASS GTK-SCALE :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_hscale_get_type")
                               NIL)
     (get-g-class-definition (gtype "GtkHScale")))
    
    ;; Check the expansion of the class definition
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-H-SCALE
                   (GTK-SCALE ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                    GTK-ORIENTABLE)
                   NIL (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkHScale")
                   (:G-TYPE-INITIALIZER . "gtk_hscale_get_type"))
         (EXPORT 'GTK-H-SCALE (FIND-PACKAGE "GTK")))
     ;; macroexpand the class definition
     (macroexpand-1 (get-g-class-definition (gtype "GtkHScale"))))))

(define-test gtk-vscale
  (assert-false (g-type-is-abstract "GtkVScale"))  
  (assert-true  (g-type-is-derived "GtkVScale"))  
  (assert-false (g-type-is-fundamental "GtkVScale"))  
  (assert-true  (g-type-is-value-type "GtkVScale"))
  (assert-true  (g-type-has-value-table "GtkVScale"))
  (assert-true  (g-type-is-classed "GtkVScale"))  
  (assert-true  (g-type-is-instantiatable "GtkVScale"))
  (assert-true  (g-type-is-derivable "GtkVScale"))
  (assert-true  (g-type-is-deep-derivable "GtkVScale"))
  (assert-false (g-type-is-interface "GtkVScale"))
  
  (let ((class (g-type-class-ref (gtype "GtkVScale"))))
    (assert-equal (gtype "GtkVScale")  (g-type-from-class class))
    (assert-equal (gtype "GtkVScale")
                  (g-type-from-class (g-type-class-peek "GtkVScale")))
    (assert-equal (gtype "GtkVScale")
                  (g-type-from-class (g-type-class-peek-static "GtkVScale")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-v-scale)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-v-scale (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkVScale" (gobject-class-g-type-name class))
    (assert-equal "GtkVScale" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_vscale_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkScale") (g-type-parent "GtkVScale"))
  (assert-eql 7 (g-type-depth "GtkVScale"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkVScale" "GObject"))
  (assert-true  (g-type-is-a "GtkVScale" "GtkVScale"))
  (assert-true  (g-type-is-a "GtkVScale" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkVScale" "gboolean"))
  (assert-false (g-type-is-a "GtkVScale" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkVScale")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkVScale")))
  
  ;; Query infos about the class "GtkScale"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkVScale" query)
    (assert-equal (gtype "GtkVScale")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkVScale"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 428 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 144 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "user-data" "name" "parent" "width-request"
         "height-request" "visible" "sensitive" "app-paintable" "can-focus"
         "has-focus" "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "extension-events" "no-show-all"
         "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "update-policy" "adjustment" "inverted"
         "lower-stepper-sensitivity" "upper-stepper-sensitivity"
         "show-fill-level" "restrict-to-fill-level" "fill-level" "round-digits"
         "digits" "draw-value" "value-pos")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkVScale"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "draw-border"
         "focus-line-pattern" "focus-line-width" "focus-padding"
         "interior-focus" "link-color" "new-tooltip-style"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "activate-slider" "arrow-displacement-x"
         "arrow-displacement-y" "arrow-scaling" "slider-width"
         "stepper-position-details" "stepper-size" "stepper-spacing"
         "trough-border" "trough-side-details" "trough-under-steppers"
         "slider-length" "value-spacing")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkVScale")))
    
    ;; Check the defintion of the class gtk-window
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkVScale" GTK-V-SCALE
                               (:SUPERCLASS GTK-SCALE :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_vscale_get_type")
                               NIL)
     (get-g-class-definition (gtype "GtkVScale")))
    
    ;; Check the expansion of the class definition
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-V-SCALE
                   (GTK-SCALE ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                    GTK-ORIENTABLE)
                   NIL (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkVScale")
                   (:G-TYPE-INITIALIZER . "gtk_vscale_get_type"))
         (EXPORT 'GTK-V-SCALE (FIND-PACKAGE "GTK")))
     ;; macroexpand the class definition
     (macroexpand-1 (get-g-class-definition (gtype "GtkVScale"))))))

;;; --- End of file rtest-scale.lisp -------------------------------------------
