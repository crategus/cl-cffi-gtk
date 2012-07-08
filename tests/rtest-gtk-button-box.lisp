;;; ----------------------------------------------------------------------------
;;; rtest-gtk-button-box.lisp
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

(define-test gtk-button-box
  (assert-false (g-type-is-abstract "GtkButtonBox"))
  (assert-true  (g-type-is-derived "GtkButtonBox"))
  (assert-false (g-type-is-fundamental "GtkButtonBox"))
  (assert-true  (g-type-is-value-type "GtkButtonBox"))
  (assert-true  (g-type-has-value-table "GtkButtonBox"))
  (assert-true  (g-type-is-classed "GtkButtonBox"))
  (assert-true  (g-type-is-instantiatable "GtkButtonBox"))
  (assert-true  (g-type-is-derivable "GtkButtonBox"))
  (assert-true  (g-type-is-deep-derivable "GtkButtonBox"))
  (assert-false (g-type-is-interface "GtkButtonBox"))
  
  (let ((class (g-type-class-ref (gtype "GtkButtonBox"))))
    (assert-equal (gtype "GtkButtonBox")  (g-type-from-class class))
    (assert-equal (gtype "GtkButtonBox")
                  (g-type-from-class (g-type-class-peek "GtkButtonBox")))
    (assert-equal (gtype "GtkButtonBox")
                  (g-type-from-class (g-type-class-peek-static "GtkButtonBox")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-button-box)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-button-box (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkButtonBox" (gobject-class-g-type-name class))
    (assert-equal "GtkButtonBox" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_button_box_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkBox") (g-type-parent "GtkButtonBox"))
  (assert-eql 6 (g-type-depth "GtkButtonBox"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkButtonBox" "GObject"))
  (assert-true  (g-type-is-a "GtkButtonBox" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkButtonBox" "gboolean"))
  (assert-false (g-type-is-a "GtkButtonBox" "GtkWindow"))
  (assert-equal '("GtkHButtonBox" "GtkVButtonBox")
                (mapcar #'gtype-name (g-type-children "GtkButtonBox")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkButtonBox")))
  
  ;; Query infos about the class "GtkButton"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkButtonBox" query)
    (assert-equal (gtype "GtkButtonBox")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkButtonBox"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 520 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "name" "parent" "width-request" "height-request"
         "visible" "sensitive" "app-paintable" "can-focus" "has-focus"
         "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "no-show-all" "has-tooltip"
         "tooltip-markup" "tooltip-text" "window" "double-buffered" "halign"
         "valign" "margin-left" "margin-right" "margin-top" "margin-bottom"
         "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
         "border-width" "resize-mode" "child" "spacing" "homogeneous"
         "layout-style")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkButtonBox"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging" "child-internal-pad-x"
         "child-internal-pad-y" "child-min-height" "child-min-width")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkButtonBox"))))
  
  ;; Create a GtkButtonBox
  (let* ((widget (make-instance 'gtk-button-box))
         (ptr (pointer widget))
         (child (make-instance 'gtk-button)))
    (assert-eq 'gtk-button-box (type-of widget))
    (assert-equal (gtype "GtkButtonBox") (g-type-from-instance ptr))
    (gtk-container-add widget child)
    ;; Get the values of class properties
    (assert-eq :edge (gtk-button-box-layout-style widget))
    (assert-eq :edge (gtk-button-box-get-layout widget))
    ;; Get the values of child properties
    (assert-false (gtk-button-box-get-child-non-homogeneous widget child))
    (assert-false (gtk-button-box-get-child-secondary widget child))
    ;; Get the values of style properties
    (assert-eql  4 (gtk-widget-style-get-property ptr "child-internal-pad-x"))
    (assert-eql  0 (gtk-widget-style-get-property ptr "child-internal-pad-y"))
    (assert-eql 27 (gtk-widget-style-get-property ptr "child-min-height"))
    (assert-eql 85 (gtk-widget-style-get-property ptr "child-min-width")))
    
  ;; Check the definition of the class
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkButtonBox" GTK-BUTTON-BOX
                               (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_button_box_get_type")
                               ((LAYOUT-STYLE GTK-BUTTON-BOX-LAYOUT-STYLE
                                 "layout-style" "GtkButtonBoxStyle" T T)))
    (get-g-type-definition (gtype "GtkButtonBox")))
)

(define-test gtk-hbutton-box
  (assert-false (g-type-is-abstract "GtkHButtonBox"))
  (assert-true  (g-type-is-derived "GtkHButtonBox"))
  (assert-false (g-type-is-fundamental "GtkHButtonBox"))
  (assert-true  (g-type-is-value-type "GtkHButtonBox"))
  (assert-true  (g-type-has-value-table "GtkHButtonBox"))
  (assert-true  (g-type-is-classed "GtkHButtonBox"))
  (assert-true  (g-type-is-instantiatable "GtkHButtonBox"))
  (assert-true  (g-type-is-derivable "GtkHButtonBox"))
  (assert-true  (g-type-is-deep-derivable "GtkHButtonBox"))
  (assert-false (g-type-is-interface "GtkHButtonBox"))
  
  (let ((class (g-type-class-ref (gtype "GtkHButtonBox"))))
    (assert-equal (gtype "GtkHButtonBox")  (g-type-from-class class))
    (assert-equal (gtype "GtkHButtonBox")
                  (g-type-from-class (g-type-class-peek "GtkHButtonBox")))
    (assert-equal (gtype "GtkHButtonBox")
                  (g-type-from-class (g-type-class-peek-static "GtkHButtonBox")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-hbutton-box)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-hbutton-box (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkHButtonBox" (gobject-class-g-type-name class))
    (assert-equal "GtkHButtonBox" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_hbutton_box_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkButtonBox") (g-type-parent "GtkHButtonBox"))
  (assert-eql 7 (g-type-depth "GtkHButtonBox"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkHButtonBox" "GObject"))
  (assert-true  (g-type-is-a "GtkHButtonBox" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkHButtonBox" "gboolean"))
  (assert-false (g-type-is-a "GtkHButtonBox" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkHButtonBox")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkHButtonBox")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkHButtonBox" query)
    (assert-equal (gtype "GtkHButtonBox")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkHButtonBox"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 520 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "name" "parent" "width-request" "height-request"
         "visible" "sensitive" "app-paintable" "can-focus" "has-focus"
         "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "no-show-all" "has-tooltip"
         "tooltip-markup" "tooltip-text" "window" "double-buffered" "halign"
         "valign" "margin-left" "margin-right" "margin-top" "margin-bottom"
         "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
         "border-width" "resize-mode" "child" "spacing" "homogeneous"
         "layout-style")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkHButtonBox"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging" "child-internal-pad-x"
         "child-internal-pad-y" "child-min-height" "child-min-width")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkHButtonBox"))))
  
  ;; Create a GtkHButtonBox
  (let* ((widget (make-instance 'gtk-hbutton-box))
         (ptr (pointer widget))
         (child (make-instance 'gtk-button)))
    (assert-eq 'gtk-hbutton-box (type-of widget))
    (assert-equal (gtype "GtkHButtonBox") (g-type-from-instance ptr))
    (gtk-container-add widget child)
    ;; Get the values of class properties
    (assert-eq :edge (gtk-button-box-layout-style widget))
    (assert-eq :edge (gtk-button-box-get-layout widget))
    ;; Get the values of child properties
    (assert-false (gtk-button-box-get-child-non-homogeneous widget child))
    (assert-false (gtk-button-box-get-child-secondary widget child))
    ;; Get the values of style properties
    (assert-eql  4 (gtk-widget-style-get-property ptr "child-internal-pad-x"))
    (assert-eql  0 (gtk-widget-style-get-property ptr "child-internal-pad-y"))
    (assert-eql 27 (gtk-widget-style-get-property ptr "child-min-height"))
    (assert-eql 85 (gtk-widget-style-get-property ptr "child-min-width")))
    
  ;; Check the definition of the class
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkHButtonBox" GTK-H-BUTTON-BOX
                               (:SUPERCLASS GTK-BUTTON-BOX :EXPORT T
                                :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_hbutton_box_get_type")
                               NIL)
    (get-g-type-definition (gtype "GtkHButtonBox")))
)

(define-test gtk-vbutton-box
  (assert-false (g-type-is-abstract "GtkVButtonBox"))
  (assert-true  (g-type-is-derived "GtkVButtonBox"))
  (assert-false (g-type-is-fundamental "GtkVButtonBox"))
  (assert-true  (g-type-is-value-type "GtkVButtonBox"))
  (assert-true  (g-type-has-value-table "GtkVButtonBox"))
  (assert-true  (g-type-is-classed "GtkVButtonBox"))
  (assert-true  (g-type-is-instantiatable "GtkVButtonBox"))
  (assert-true  (g-type-is-derivable "GtkVButtonBox"))
  (assert-true  (g-type-is-deep-derivable "GtkVButtonBox"))
  (assert-false (g-type-is-interface "GtkVButtonBox"))
  
  (let ((class (g-type-class-ref (gtype "GtkVButtonBox"))))
    (assert-equal (gtype "GtkVButtonBox")  (g-type-from-class class))
    (assert-equal (gtype "GtkVButtonBox")
                  (g-type-from-class (g-type-class-peek "GtkVButtonBox")))
    (assert-equal (gtype "GtkVButtonBox")
                  (g-type-from-class (g-type-class-peek-static "GtkVButtonBox")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-vbutton-box)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-vbutton-box (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkVButtonBox" (gobject-class-g-type-name class))
    (assert-equal "GtkVButtonBox" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_vbutton_box_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkButtonBox") (g-type-parent "GtkVButtonBox"))
  (assert-eql 7 (g-type-depth "GtkVButtonBox"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkVButtonBox" "GObject"))
  (assert-true  (g-type-is-a "GtkVButtonBox" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkVButtonBox" "gboolean"))
  (assert-false (g-type-is-a "GtkVButtonBox" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkVButtonBox")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkVButtonBox")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkVButtonBox" query)
    (assert-equal (gtype "GtkVButtonBox")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkVButtonBox"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 520 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("orientation" "name" "parent" "width-request" "height-request"
         "visible" "sensitive" "app-paintable" "can-focus" "has-focus"
         "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "no-show-all" "has-tooltip"
         "tooltip-markup" "tooltip-text" "window" "double-buffered" "halign"
         "valign" "margin-left" "margin-right" "margin-top" "margin-bottom"
         "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
         "border-width" "resize-mode" "child" "spacing" "homogeneous"
         "layout-style")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkVButtonBox"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging" "child-internal-pad-x"
         "child-internal-pad-y" "child-min-height" "child-min-width")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkVButtonBox"))))
  
  ;; Create a GtkVButtonBox
  (let* ((widget (make-instance 'gtk-vbutton-box))
         (ptr (pointer widget))
         (child (make-instance 'gtk-button)))
    (assert-eq 'gtk-vbutton-box (type-of widget))
    (assert-equal (gtype "GtkVButtonBox") (g-type-from-instance ptr))
    (gtk-container-add widget child)
    ;; Get the values of class properties
    (assert-eq :edge (gtk-button-box-layout-style widget))
    (assert-eq :edge (gtk-button-box-get-layout widget))
    ;; Get the values of child properties
    (assert-false (gtk-button-box-get-child-non-homogeneous widget child))
    (assert-false (gtk-button-box-get-child-secondary widget child))
    ;; Get the values of style properties
    (assert-eql  4 (gtk-widget-style-get-property ptr "child-internal-pad-x"))
    (assert-eql  0 (gtk-widget-style-get-property ptr "child-internal-pad-y"))
    (assert-eql 27 (gtk-widget-style-get-property ptr "child-min-height"))
    (assert-eql 85 (gtk-widget-style-get-property ptr "child-min-width")))
    
  ;; Check the definition of the class
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkVButtonBox" GTK-V-BUTTON-BOX
                               (:SUPERCLASS GTK-BUTTON-BOX :EXPORT T
                                :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_vbutton_box_get_type")
                               NIL)
    (get-g-type-definition (gtype "GtkVButtonBox")))
)

;;; --- End of the file rtest-gtk-button-box.lisp ------------------------------
