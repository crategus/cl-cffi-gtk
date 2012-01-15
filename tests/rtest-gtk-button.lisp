;;; ----------------------------------------------------------------------------
;;; rtest-gtk-box.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; ----------------------------------------------------------------------------
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

(asdf:operate 'asdf:load-op :cl-gtk-gtk)

(defpackage :gtk-tests
  (:use :gtk :gobject :glib :cffi :common-lisp :lisp-unit))

(in-package :gtk-tests)

(define-test gtk-button
  (let* ((button (gtk-button-new))
         (type (g-type-from-instance (pointer button))))
    (assert-equal "GtkButton" (gtype-name type))
    (assert-equal "GtkButton" (gtype-name (gtype "GtkButton")))
    (assert-eql 'gtk-button (registered-object-type-by-name "GtkButton"))
    (assert-equal "GtkBin" (gtype-name (g-type-parent type)))
    (assert-equal '("GtkColorButton" "GtkFontButton" "GtkLinkButton"
                    "GtkScaleButton" "GtkToggleButton")
                  (mapcar #'gtype-name (g-type-children type)))
    
    (assert-true       (gtk-button-focus-on-click button))
    (assert-false      (gtk-button-image button))
    (assert-eq :left   (gtk-button-image-position button))
    (assert-false      (gtk-button-label button))
    (assert-eq :normal (gtk-button-relief button))
    (assert-false      (gtk-button-use-stock button))
    (assert-false      (gtk-button-use-underline button))
    (assert-eql 0.5    (gtk-button-xalign button))
    (assert-eql 0.5    (gtk-button-yalign button))
    
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkButton" GTK-BUTTON
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_button_get_type")
                       ((FOCUS-ON-CLICK GTK-BUTTON-FOCUS-ON-CLICK
                         "focus-on-click" "gboolean" T T)
                        (IMAGE GTK-BUTTON-IMAGE "image" "GtkWidget" T T)
                        (IMAGE-POSITION GTK-BUTTON-IMAGE-POSITION
                         "image-position" "GtkPositionType" T T)
                        (LABEL GTK-BUTTON-LABEL "label" "gchararray" T T)
                        (RELIEF GTK-BUTTON-RELIEF "relief" "GtkReliefStyle" T
                         T)
                        (USE-STOCK GTK-BUTTON-USE-STOCK "use-stock" "gboolean"
                         T T)
                        (USE-UNDERLINE GTK-BUTTON-USE-UNDERLINE "use-underline"
                         "gboolean" T T)
                        (XALIGN GTK-BUTTON-XALIGN "xalign" "gfloat" T T)
                        (YALIGN GTK-BUTTON-YALIGN "yalign" "gfloat" T T)))
     (get-g-class-definition type))
    
    (assert-equal
     '(PROGN
        (DEFCLASS GTK-BUTTON
                  (GTK-BIN ATK-IMPLEMENTOR-IFACE ACTIVATABLE BUILDABLE)
           ((FOCUS-ON-CLICK :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gboolean" :ACCESSOR GTK-BUTTON-FOCUS-ON-CLICK :INITARG
             :FOCUS-ON-CLICK :G-PROPERTY-NAME "focus-on-click")
            (IMAGE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "GtkWidget"
             :ACCESSOR GTK-BUTTON-IMAGE :INITARG :IMAGE :G-PROPERTY-NAME
             "image")
            (IMAGE-POSITION :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GtkPositionType" :ACCESSOR GTK-BUTTON-IMAGE-POSITION :INITARG
             :IMAGE-POSITION :G-PROPERTY-NAME "image-position")
            (LABEL :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gchararray"
             :ACCESSOR GTK-BUTTON-LABEL :INITARG :LABEL :G-PROPERTY-NAME
             "label")
            (RELIEF :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GtkReliefStyle" :ACCESSOR GTK-BUTTON-RELIEF :INITARG :RELIEF
             :G-PROPERTY-NAME "relief")
            (USE-STOCK :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gboolean" :ACCESSOR GTK-BUTTON-USE-STOCK :INITARG :USE-STOCK
             :G-PROPERTY-NAME "use-stock")
            (USE-UNDERLINE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gboolean" :ACCESSOR GTK-BUTTON-USE-UNDERLINE :INITARG
             :USE-UNDERLINE :G-PROPERTY-NAME "use-underline")
            (XALIGN :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gfloat"
             :ACCESSOR GTK-BUTTON-XALIGN :INITARG :XALIGN :G-PROPERTY-NAME
             "xalign")
            (YALIGN :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gfloat"
             :ACCESSOR GTK-BUTTON-YALIGN :INITARG :YALIGN :G-PROPERTY-NAME
             "yalign"))
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkButton")
           (:G-TYPE-INITIALIZER . "gtk_button_get_type"))
        (EXPORT 'GTK-BUTTON (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-FOCUS-ON-CLICK (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-IMAGE (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-IMAGE-POSITION (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-LABEL (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-RELIEF (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-USE-STOCK (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-USE-UNDERLINE (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-XALIGN (FIND-PACKAGE "GTK"))
        (EXPORT 'GTK-BUTTON-YALIGN (FIND-PACKAGE "GTK")))
     (macroexpand-1 (get-g-class-definition (gtype "GtkButton"))))
    ))

(define-test gtk-toggle-button
  (let* ((button (make-instance 'gtk-toggle-button))
         (type (g-type-from-instance (pointer button))))
    (assert-equal "GtkToggleButton" (gtype-name type))
    (assert-equal "GtkToggleButton" (gtype-name (gtype "GtkToggleButton")))
    (assert-eql 'gtk-toggle-button
                (registered-object-type-by-name "GtkToggleButton"))
    (assert-equal "GtkButton" (gtype-name (g-type-parent type)))
    (assert-equal '("GtkCheckButton")
                  (mapcar #'gtype-name (g-type-children type)))
    
    (assert-true       (gtk-button-focus-on-click button))
    (assert-false      (gtk-button-image button))
    (assert-eq :left   (gtk-button-image-position button))
    (assert-false      (gtk-button-label button))
    (assert-eq :normal (gtk-button-relief button))
    (assert-false      (gtk-button-use-stock button))
    (assert-false      (gtk-button-use-underline button))
    (assert-eql 0.5    (gtk-button-xalign button))
    (assert-eql 0.5    (gtk-button-yalign button))
    
    (assert-false      (gtk-toggle-button-active button))
    (assert-false      (gtk-toggle-button-draw-indicator button))
    (assert-false      (gtk-toggle-button-inconsistent button))
    
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkToggleButton" GTK-TOGGLE-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_toggle_button_get_type")
                       ((ACTIVE GTK-TOGGLE-BUTTON-ACTIVE "active" "gboolean" T
                         T)
                        (DRAW-INDICATOR GTK-TOGGLE-BUTTON-DRAW-INDICATOR
                         "draw-indicator" "gboolean" T T)
                        (INCONSISTENT GTK-TOGGLE-BUTTON-INCONSISTENT
                         "inconsistent" "gboolean" T T)))
     (get-g-class-definition type))
    
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-TOGGLE-BUTTON
           (GTK-BUTTON ATK-IMPLEMENTOR-IFACE ACTIVATABLE BUILDABLE)
           ((ACTIVE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gboolean"
             :ACCESSOR GTK-TOGGLE-BUTTON-ACTIVE :INITARG :ACTIVE
             :G-PROPERTY-NAME "active")
            (DRAW-INDICATOR :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gboolean" :ACCESSOR GTK-TOGGLE-BUTTON-DRAW-INDICATOR :INITARG
             :DRAW-INDICATOR :G-PROPERTY-NAME "draw-indicator")
            (INCONSISTENT :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gboolean" :ACCESSOR GTK-TOGGLE-BUTTON-INCONSISTENT :INITARG
             :INCONSISTENT :G-PROPERTY-NAME "inconsistent"))
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkToggleButton")
           (:G-TYPE-INITIALIZER . "gtk_toggle_button_get_type"))
         (EXPORT 'GTK-TOGGLE-BUTTON (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-TOGGLE-BUTTON-ACTIVE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-TOGGLE-BUTTON-DRAW-INDICATOR (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-TOGGLE-BUTTON-INCONSISTENT (FIND-PACKAGE "GTK")))
     (macroexpand-1 (get-g-class-definition (gtype "GtkToggleButton"))))
    ))

(define-test gtk-radio-button
  (let* ((button (make-instance 'gtk-radio-button))
         (type (g-type-from-instance (pointer button))))
    (assert-equal "GtkRadioButton" (gtype-name type))
    (assert-equal "GtkRadioButton" (gtype-name (gtype "GtkRadioButton")))
    (assert-eql 'gtk-radio-button
                (registered-object-type-by-name "GtkRadioButton"))
    (assert-equal "GtkCheckButton" (gtype-name (g-type-parent type)))
    (assert-equal '() (mapcar #'gtype-name (g-type-children type)))
    
    (assert-true       (gtk-button-focus-on-click button))
    (assert-false      (gtk-button-image button))
    (assert-eq :left   (gtk-button-image-position button))
    (assert-false      (gtk-button-label button))
    (assert-eq :normal (gtk-button-relief button))
    (assert-false      (gtk-button-use-stock button))
    (assert-false      (gtk-button-use-underline button))
    (assert-eql 0.5    (gtk-button-xalign button))
    (assert-eql 0.5    (gtk-button-yalign button))
    
    (assert-true       (gtk-toggle-button-active button))
    (assert-true       (gtk-toggle-button-draw-indicator button))
    (assert-false      (gtk-toggle-button-inconsistent button))
    
    (let* ((group nil)
           (button2 (gtk-radio-button-new-from-widget button))
           (button3 (gtk-radio-button-new-from-widget button2)))
      (setq group (gtk-radio-button-get-group button))
      (assert-equal group (gtk-radio-button-get-group button))
      (assert-equal group (gtk-radio-button-get-group button2))
      (assert-equal group (gtk-radio-button-get-group button3)))
    
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkRadioButton" GTK-RADIO-BUTTON
                       (:SUPERCLASS GTK-CHECK-BUTTON :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_button_get_type")
                       ((GROUP GTK-RADIO-BUTTON-GROUP "group" "GtkRadioButton"
                         NIL T)))
     (get-g-class-definition type))
    
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-RADIO-BUTTON
           (GTK-CHECK-BUTTON ATK-IMPLEMENTOR-IFACE ACTIVATABLE BUILDABLE)
           ((GROUP :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GtkRadioButton" :ACCESSOR GTK-RADIO-BUTTON-GROUP :INITARG :GROUP
             :G-PROPERTY-NAME "group"))
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkRadioButton")
           (:G-TYPE-INITIALIZER . "gtk_radio_button_get_type"))
         (EXPORT 'GTK-RADIO-BUTTON (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-RADIO-BUTTON-GROUP (FIND-PACKAGE "GTK")))
     (macroexpand-1 (get-g-class-definition (gtype "GtkRadioButton"))))
  ))
