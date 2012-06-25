;;; ----------------------------------------------------------------------------
;;; rtest-gtk-box.lisp
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

(define-test gtk-box
  (assert-false (g-type-is-abstract "GtkBox"))
  (assert-true  (g-type-is-derived "GtkBox"))
  (assert-false (g-type-is-fundamental "GtkBox"))
  (assert-true  (g-type-is-value-type "GtkBox"))
  (assert-true  (g-type-has-value-table "GtkBox"))
  (assert-true  (g-type-is-classed "GtkBox"))
  (assert-true  (g-type-is-instantiatable "GtkBox"))
  (assert-true  (g-type-is-derivable "GtkBox"))
  (assert-true  (g-type-is-deep-derivable "GtkBox"))
  (assert-false (g-type-is-interface "GtkBox"))
  
  (let ((class (g-type-class-ref (gtype "GtkBox"))))
    (assert-equal (gtype "GtkBox")  (g-type-from-class class))
    (assert-equal (gtype "GtkBox")
                  (g-type-from-class (g-type-class-peek "GtkBox")))
    (assert-equal (gtype "GtkBox")
                  (g-type-from-class (g-type-class-peek-static "GtkBox")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-box)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-box (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkBox" (gobject-class-g-type-name class))
    (assert-equal "GtkBox" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_box_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkContainer") (g-type-parent "GtkBox"))
  (assert-eql 5 (g-type-depth "GtkBox"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkBox" "GObject"))
  (assert-true  (g-type-is-a "GtkBox" "GtkBox"))
  (assert-true  (g-type-is-a "GtkBox" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkBox" "gboolean"))
  (assert-false (g-type-is-a "GtkBox" "GtkWindow"))
  (assert-equal '("GtkHBox" "GtkVBox" "GtkButtonBox" "GtkStatusbar" "GtkInfoBar"
         "GtkColorChooserWidget" "GtkColorSelection" "GtkFileChooserWidget"
         "GtkFileChooserButton" "GtkFontSelection" "GtkRecentChooserWidget")
                (mapcar #'gtype-name (g-type-children "GtkBox")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkBox")))
  
  ;; Query infos about the class "GtkBox"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkBox" query)
    (assert-equal (gtype "GtkBox")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkBox"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 504 (foreign-slot-value query 'g-type-query :class-size))
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
         "border-width" "resize-mode" "child" "spacing" "homogeneous")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkBox"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkBox"))))

  (assert-equal "GtkBox" (gtype-name (gtype "GtkBox")))
  (assert-eql 'gtk-box (registered-object-type-by-name "GtkBox"))
  (assert-equal "GtkContainer" (gtype-name (g-type-parent (gtype "GtkBox"))))
  (unordered-equal '("GtkButtonBox" "GtkHBox" "GtkVBox")
                   (mapcar #'gtype-name (g-type-children (gtype "GtkBox"))))
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkBox" GTK-BOX
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_box_get_type")
                       ((HOMOGENEOUS GTK-BOX-HOMOGENEOUS "homogeneous"
                         "gboolean" T T)
                        (SPACING GTK-BOX-SPACING "spacing" "gint" T T)))
   (gobject::get-g-class-definition (gtype "GtkBox")))
  (assert-equal
    '(PROGN
       (DEFCLASS GTK-BOX (GTK-CONTAINER ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                                        GTK-ORIENTABLE)
         ((HOMOGENEOUS :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
           "gboolean" :ACCESSOR GTK-BOX-HOMOGENEOUS :INITARG :HOMOGENEOUS
             :G-PROPERTY-NAME "homogeneous")
          (SPACING :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gint"
             :ACCESSOR GTK-BOX-SPACING :INITARG :SPACING :G-PROPERTY-NAME
             "spacing"))
          (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkBox")
         (:G-TYPE-INITIALIZER . "gtk_box_get_type"))
       (EXPORT 'GTK-BOX (FIND-PACKAGE "GTK"))
       (EXPORT 'GTK-BOX-HOMOGENEOUS (FIND-PACKAGE "GTK"))
       (EXPORT 'GTK-BOX-SPACING (FIND-PACKAGE "GTK")))
   (macroexpand-1 (gobject::get-g-class-definition (gtype "GtkBox"))))
   ;; In Gtk+ 2 we can not create an instance of GtkBox. We use a GtkHBox.
   (let ((hbox (make-instance 'gtk-hbox))
         (button1 (make-instance 'gtk-button))
         (button2 (make-instance 'gtk-button)))
     ;; Check default values of properties
     (assert-false (gtk-box-get-homogeneous hbox))
     (assert-eql 0 (gtk-box-get-spacing hbox))
     ;; Add two buttons to the hbox
     (gtk-box-pack-start hbox button1)
     (gtk-box-pack-end   hbox button2)
     ;; Check default values of child properties
     (assert-true (gtk-box-child-expand hbox button1))
     (assert-true (gtk-box-child-expand hbox button2))
     (assert-true (gtk-box-child-fill hbox button1))
     (assert-true (gtk-box-child-fill hbox button2))
     (assert-eql 0 (gtk-box-child-padding hbox button1))
     (assert-eql 0 (gtk-box-child-padding hbox button2))
     (assert-eql 0 (gtk-box-child-position hbox button1))
     (assert-eql 1 (gtk-box-child-position hbox button2))
     (assert-eq :start (gtk-box-child-pack-type hbox button1))
     (assert-eq :end   (gtk-box-child-pack-type hbox button2))
     ;; Reorder the buttons in the hbox
     (gtk-box-reorder-child hbox button1 1)
     (assert-eq 0 (gtk-box-child-position hbox button2))
     (assert-eq 1 (gtk-box-child-position hbox button1))
     ;; Set and query child packing
     (gtk-box-set-child-packing hbox button1 nil nil 10 :end)
     (assert-equal (values nil nil 10 :end)
                   (gtk-box-query-child-packing hbox button1))))

(define-test gtk-hbox
  (assert-false (g-type-is-abstract "GtkHBox"))
  (assert-true  (g-type-is-derived "GtkHBox"))
  (assert-false (g-type-is-fundamental "GtkHBox"))
  (assert-true  (g-type-is-value-type "GtkHBox"))
  (assert-true  (g-type-has-value-table "GtkHBox"))
  (assert-true  (g-type-is-classed "GtkHBox"))
  (assert-true  (g-type-is-instantiatable "GtkHBox"))
  (assert-true  (g-type-is-derivable "GtkHBox"))
  (assert-true  (g-type-is-deep-derivable "GtkHBox"))
  (assert-false (g-type-is-interface "GtkHBox"))
  
  (let ((class (g-type-class-ref (gtype "GtkHBox"))))
    (assert-equal (gtype "GtkHBox")  (g-type-from-class class))
    (assert-equal (gtype "GtkHBox")
                  (g-type-from-class (g-type-class-peek "GtkHBox")))
    (assert-equal (gtype "GtkHBox")
                  (g-type-from-class (g-type-class-peek-static "GtkHBox")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-hbox)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-hbox (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkHBox" (gobject-class-g-type-name class))
    (assert-equal "GtkHBox" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_hbox_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkBox") (g-type-parent "GtkHBox"))
  (assert-eql 6 (g-type-depth "GtkHBox"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkHBox" "GObject"))
  (assert-true  (g-type-is-a "GtkHBox" "GtkHBox"))
  (assert-true  (g-type-is-a "GtkHBox" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkHBox" "gboolean"))
  (assert-false (g-type-is-a "GtkHBox" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkHBox")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkHBox")))
  
  ;; Query infos about the class "GtkBox"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkHBox" query)
    (assert-equal (gtype "GtkHBox")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkHBox"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 504 (foreign-slot-value query 'g-type-query :class-size))
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
         "border-width" "resize-mode" "child" "spacing" "homogeneous")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkHBox"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkHBox"))))

  (let* ((hbox (make-instance 'gtk-hbox
                              :homogeneous nil
                              :spacing 0))
         (hbox-type (g-type-from-instance (pointer hbox))))
    (assert-equal "GtkHBox" (gtype-name hbox-type))
    (assert-eql 'gtk-hbox (registered-object-type-by-name "GtkHBox"))
    (assert-equal "GtkBox" (gtype-name (g-type-parent hbox-type)))
    (unordered-equal '("GtkFileChooserButton" "GtkStatusbar")
                     (mapcar #'gtype-name (g-type-children hbox-type)))
    (assert-false (gtk-box-homogeneous hbox))
    (assert-eql 0 (gtk-box-spacing hbox))
    (assert-true (gtk-box-set-homogeneous hbox t))
    (assert-true (gtk-box-get-homogeneous hbox))
    (assert-eql 10 (gtk-box-set-spacing hbox 10))
    (assert-eql 10 (gtk-box-get-spacing hbox))
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkHBox" GTK-HBOX
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_hbox_get_type")
         NIL)
     (gobject::get-g-class-definition hbox-type))
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-HBOX (GTK-BOX ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                                      GTK-ORIENTABLE) NIL
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkHBox")
           (:G-TYPE-INITIALIZER . "gtk_hbox_get_type"))
         (EXPORT 'GTK-HBOX (FIND-PACKAGE "GTK")))
     (macroexpand-1 (gobject::get-g-class-definition (gtype "GtkHBox"))))
   ;; Check properties of a GtkHBox
   (let ((hbox (make-instance 'gtk-hbox))
         (button1 (make-instance 'gtk-button))
         (button2 (make-instance 'gtk-button)))
     ;; Check default values of properties
     (assert-false (gtk-box-get-homogeneous hbox))
     (assert-eql 0 (gtk-box-get-spacing hbox))
     ;; Add two buttons to the hbox
     (gtk-box-pack-start hbox button1)
     (gtk-box-pack-end   hbox button2)
     ;; Check default values of child properties
     (assert-true (gtk-hbox-child-expand hbox button1))
     (assert-true (gtk-hbox-child-expand hbox button2))
     (assert-true (gtk-hbox-child-fill hbox button1))
     (assert-true (gtk-hbox-child-fill hbox button2))
     (assert-eql 0 (gtk-hbox-child-padding hbox button1))
     (assert-eql 0 (gtk-hbox-child-padding hbox button2))
     (assert-eql 0 (gtk-hbox-child-position hbox button1))
     (assert-eql 1 (gtk-hbox-child-position hbox button2))
     (assert-eq :start (gtk-hbox-child-pack-type hbox button1))
     (assert-eq :end   (gtk-hbox-child-pack-type hbox button2))
     ;; Reorder the buttons in the hbox
     (gtk-box-reorder-child hbox button1 1)
     (assert-eq 0 (gtk-hbox-child-position hbox button2))
     (assert-eq 1 (gtk-hbox-child-position hbox button1))
     ;; Set and query child packing
     (gtk-box-set-child-packing hbox button1 nil nil 10 :end)
     (assert-equal (values nil nil 10 :end)
                   (gtk-box-query-child-packing hbox button1)))))

(define-test gtk-vbox
  (assert-false (g-type-is-abstract "GtkVBox"))
  (assert-true  (g-type-is-derived "GtkVBox"))
  (assert-false (g-type-is-fundamental "GtkVBox"))
  (assert-true  (g-type-is-value-type "GtkVBox"))
  (assert-true  (g-type-has-value-table "GtkVBox"))
  (assert-true  (g-type-is-classed "GtkVBox"))
  (assert-true  (g-type-is-instantiatable "GtkVBox"))
  (assert-true  (g-type-is-derivable "GtkVBox"))
  (assert-true  (g-type-is-deep-derivable "GtkVBox"))
  (assert-false (g-type-is-interface "GtkVBox"))
  
  (let ((class (g-type-class-ref (gtype "GtkVBox"))))
    (assert-equal (gtype "GtkVBox")  (g-type-from-class class))
    (assert-equal (gtype "GtkVBox")
                  (g-type-from-class (g-type-class-peek "GtkVBox")))
    (assert-equal (gtype "GtkVBox")
                  (g-type-from-class (g-type-class-peek-static "GtkVBox")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-vbox)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-vbox (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkVBox" (gobject-class-g-type-name class))
    (assert-equal "GtkVBox" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_vbox_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkBox") (g-type-parent "GtkVBox"))
  (assert-eql 6 (g-type-depth "GtkVBox"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkVBox" "GObject"))
  (assert-true  (g-type-is-a "GtkVBox" "GtkVBox"))
  (assert-true  (g-type-is-a "GtkVBox" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkVBox" "gboolean"))
  (assert-false (g-type-is-a "GtkVBox" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkVBox")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkVBox")))
  
  ;; Query infos about the class "GtkBox"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkVBox" query)
    (assert-equal (gtype "GtkVBox")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkVBox"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 504 (foreign-slot-value query 'g-type-query :class-size))
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
         "border-width" "resize-mode" "child" "spacing" "homogeneous")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkVBox"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkVBox"))))

  (let* ((vbox (make-instance 'gtk-vbox
                              :homogeneous nil
                              :spacing 0))
         (vbox-type (g-type-from-instance (pointer vbox))))
    (assert-equal "GtkVBox" (gtype-name vbox-type))
    (assert-eql 'gtk-vbox (registered-object-type-by-name "GtkVBox"))
    (assert-equal "GtkBox" (gtype-name (g-type-parent vbox-type)))
    (unordered-equal '("GtkColorSelection"
                       "GtkFileChooserWidget"
                       "GtkFontSelection"
                       "GtkGammaCurve"
                       "GtkRecentChooserWidget")
                  (mapcar #'gtype-name (g-type-children vbox-type)))
    (assert-false (gtk-box-homogeneous vbox))
    (assert-eql 0 (gtk-box-spacing vbox))
    (assert-true (gtk-box-set-homogeneous vbox t))
    (assert-true (gtk-box-get-homogeneous vbox))
    (assert-eql 10 (gtk-box-set-spacing vbox 10))
    (assert-eql 10 (gtk-box-get-spacing vbox))
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkVBox" GTK-VBOX
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_vbox_get_type")
         NIL)
     (gobject::get-g-class-definition vbox-type))
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-VBOX (GTK-BOX ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                                      GTK-ORIENTABLE) NIL
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkVBox")
           (:G-TYPE-INITIALIZER . "gtk_vbox_get_type"))
         (EXPORT 'GTK-VBOX (FIND-PACKAGE "GTK")))
     (macroexpand-1 (gobject::get-g-class-definition (gtype "GtkVBox"))))
   ;; Check properties of a GtkVBox
   (let ((vbox (make-instance 'gtk-vbox))
         (button1 (make-instance 'gtk-button))
         (button2 (make-instance 'gtk-button)))
     ;; Check default values of properties
     (assert-false (gtk-box-get-homogeneous vbox))
     (assert-eql 0 (gtk-box-get-spacing vbox))
     ;; Add two buttons to the vboxv
     (gtk-box-pack-start vbox button1)
     (gtk-box-pack-end   vbox button2)
     ;; Check default values of child properties
     (assert-true (gtk-vbox-child-expand vbox button1))
     (assert-true (gtk-vbox-child-expand vbox button2))
     (assert-true (gtk-vbox-child-fill vbox button1))
     (assert-true (gtk-vbox-child-fill vbox button2))
     (assert-eql 0 (gtk-vbox-child-padding vbox button1))
     (assert-eql 0 (gtk-vbox-child-padding vbox button2))
     (assert-eql 0 (gtk-vbox-child-position vbox button1))
     (assert-eql 1 (gtk-vbox-child-position vbox button2))
     (assert-eq :start (gtk-vbox-child-pack-type vbox button1))
     (assert-eq :end   (gtk-vbox-child-pack-type vbox button2))
     ;; Reorder the buttons in the vbox
     (gtk-box-reorder-child vbox button1 1)
     (assert-eq 0 (gtk-vbox-child-position vbox button2))
     (assert-eq 1 (gtk-vbox-child-position vbox button1))
     ;; Set and query child packing
     (gtk-box-set-child-packing vbox button1 nil nil 10 :end)
     (assert-equal (values nil nil 10 :end)
                   (gtk-box-query-child-packing vbox button1)))))

;;; --- End of the file rtest-gtk-box.lisp -------------------------------------
