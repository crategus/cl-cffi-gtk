;;; ----------------------------------------------------------------------------
;;; rtest-gtk-button.lisp
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

(define-test gtk-button
  (assert-false (g-type-is-abstract "GtkButton"))
  (assert-true  (g-type-is-derived "GtkButton"))
  (assert-false (g-type-is-fundamental "GtkButton"))
  (assert-true  (g-type-is-value-type "GtkButton"))
  (assert-true  (g-type-has-value-table "GtkButton"))
  (assert-true  (g-type-is-classed "GtkButton"))
  (assert-true  (g-type-is-instantiatable "GtkButton"))
  (assert-true  (g-type-is-derivable "GtkButton"))
  (assert-true  (g-type-is-deep-derivable "GtkButton"))
  (assert-false (g-type-is-interface "GtkButton"))
  
  (let ((class (g-type-class-ref (gtype "GtkButton"))))
    (assert-equal (gtype "GtkButton")  (g-type-from-class class))
    (assert-equal (gtype "GtkButton")
                  (g-type-from-class (g-type-class-peek "GtkButton")))
    (assert-equal (gtype "GtkButton")
                  (g-type-from-class (g-type-class-peek-static "GtkButton")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-button)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-button (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkButton" (gobject-class-g-type-name class))
    (assert-equal "GtkButton" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_button_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkBin") (g-type-parent "GtkButton"))
  (assert-eql 6 (g-type-depth "GtkButton"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkButton" "GObject"))
  (assert-true  (g-type-is-a "GtkButton" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkButton" "gboolean"))
  (assert-false (g-type-is-a "GtkButton" "GtkWindow"))
  (assert-equal '("GtkToggleButton" "GtkLinkButton" "GtkScaleButton"
                  "GtkColorButton" "GtkFontButton")
                (mapcar #'gtype-name (g-type-children "GtkButton")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
                  "GtkActivatable")
                (mapcar #'gtype-name (g-type-interfaces "GtkButton")))
  
  ;; Query infos about the class "GtkButton"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkButton" query)
    (assert-equal (gtype "GtkButton")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkButton"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 544 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("action-name" "action-target" "related-action" "use-action-appearance"
         "name" "parent" "width-request" "height-request" "visible" "sensitive"
         "app-paintable" "can-focus" "has-focus" "is-focus" "can-default"
         "has-default" "receives-default" "composite-child" "style" "events"
         "no-show-all" "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "halign" "valign" "margin-left" "margin-right"
         "margin-top" "margin-bottom" "margin" "hexpand" "vexpand"
         "hexpand-set" "vexpand-set" "expand" "border-width" "resize-mode"
         "child" "label" "image" "relief" "use-underline" "use-stock"
         "focus-on-click" "xalign" "yalign" "image-position")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkButton"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging" "child-displacement-x"
         "child-displacement-y" "default-border" "default-outside-border"
         "displace-focus" "image-spacing" "inner-border")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkButton"))))
  
  ;; Create a GtkButton
  (let* ((widget (make-instance 'gtk-button))
         (ptr (pointer widget)))
    (assert-eq 'gtk-button (type-of widget))
    (assert-equal (gtype "GtkButton") (g-type-from-instance ptr))
    ;; Read the default values of the class properties
    (assert-true       (gtk-button-focus-on-click widget))
    (assert-false      (gtk-button-image widget))
    (assert-eq :left   (gtk-button-image-position widget))
    (assert-false      (gtk-button-label widget))
    (assert-eq :normal (gtk-button-relief widget))
    (assert-false      (gtk-button-use-stock widget))
    (assert-false      (gtk-button-use-underline widget))
    (assert-eql 0.5    (gtk-button-xalign widget))
    (assert-eql 0.5    (gtk-button-yalign widget))
    ;; Get the values of style properties
    (assert-eql 1 (gtk-widget-style-get-property ptr "child-displacement-x"))
    (assert-eql 1 (gtk-widget-style-get-property ptr "child-displacement-y"))
    (assert-eq 'gtk-border (type-of (gtk-widget-style-get-property ptr "default-border")))
    (assert-false (gtk-widget-style-get-property ptr "default-outside-border"))
    (assert-false (gtk-widget-style-get-property ptr "displace-focus"))
    (assert-eql 0 (gtk-widget-style-get-property ptr "image-spacing"))
    (assert-eq 'gtk-border (type-of (gtk-widget-style-get-property ptr "inner-border"))))
    
  ;; Check the definition of the class gtk-button
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkButton" GTK-BUTTON
                               (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkActionable"
                                 "GtkActivatable" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_button_get_type")
                               ((FOCUS-ON-CLICK GTK-BUTTON-FOCUS-ON-CLICK
                                 "focus-on-click" "gboolean" T T)
                                (IMAGE GTK-BUTTON-IMAGE "image" "GtkWidget" T
                                 T)
                                (IMAGE-POSITION GTK-BUTTON-IMAGE-POSITION
                                 "image-position" "GtkPositionType" T T)
                                (LABEL GTK-BUTTON-LABEL "label" "gchararray" T
                                 T)
                                (RELIEF GTK-BUTTON-RELIEF "relief"
                                 "GtkReliefStyle" T T)
                                (USE-STOCK GTK-BUTTON-USE-STOCK "use-stock"
                                 "gboolean" T T)
                                (USE-UNDERLINE GTK-BUTTON-USE-UNDERLINE
                                 "use-underline" "gboolean" T T)
                                (XALIGN GTK-BUTTON-XALIGN "xalign" "gfloat" T
                                 T)
                                (YALIGN GTK-BUTTON-YALIGN "yalign" "gfloat" T
                                 T)))
    (get-g-type-definition (gtype "GtkButton")))
)

;;; --- End of the file rtest-gtk-button.lisp ----------------------------------
