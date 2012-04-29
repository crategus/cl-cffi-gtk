;;; ----------------------------------------------------------------------------
;;; rtest-gtk-radio-button.lisp
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

(define-test gtk-radio-button
  (assert-false (g-type-is-abstract "GtkRadioButton"))
  (assert-true  (g-type-is-derived "GtkRadioButton"))
  (assert-false (g-type-is-fundamental "GtkRadioButton"))
  (assert-true  (g-type-is-value-type "GtkRadioButton"))
  (assert-true  (g-type-has-value-table "GtkRadioButton"))
  (assert-true  (g-type-is-classed "GtkRadioButton"))
  (assert-true  (g-type-is-instantiatable "GtkRadioButton"))
  (assert-true  (g-type-is-derivable "GtkRadioButton"))
  (assert-true  (g-type-is-deep-derivable "GtkRadioButton"))
  (assert-false (g-type-is-interface "GtkRadioButton"))
  
  (let ((class (g-type-class-ref (gtype "GtkRadioButton"))))
    (assert-equal (gtype "GtkRadioButton")  (g-type-from-class class))
    (assert-equal (gtype "GtkRadioButton")
                  (g-type-from-class (g-type-class-peek "GtkRadioButton")))
    (assert-equal (gtype "GtkRadioButton")
                  (g-type-from-class (g-type-class-peek-static "GtkRadioButton")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-radio-button)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-radio-button (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkRadioButton" (gobject-class-g-type-name class))
    (assert-equal "GtkRadioButton" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_radio_button_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkCheckButton") (g-type-parent "GtkRadioButton"))
  (assert-eql 9 (g-type-depth "GtkRadioButton"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkRadioButton" "GObject"))
  (assert-true  (g-type-is-a "GtkRadioButton" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkRadioButton" "gboolean"))
  (assert-false (g-type-is-a "GtkRadioButton" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkRadioButton")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
                  "GtkActivatable")
                (mapcar #'gtype-name (g-type-interfaces "GtkRadioButton")))
  
  ;; Query infos about the class "GtkLabel"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkRadioButton" query)
    (assert-equal (gtype "GtkRadioButton")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkRadioButton"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 604 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  36 (foreign-slot-value query 'g-type-query :instance-size)))
  
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
  
  ;; Create a GtkRadioButton
  (let* ((widget (make-instance 'gtk-radio-button))
         (ptr (pointer widget)))
    (assert-eq 'gtk-radio-button (type-of widget))
    (assert-equal (gtype "GtkRadioButton") (g-type-from-instance ptr))
    ;; Read the defalut values of the class properties
    (assert-error 'error (gtk-radio-button-group widget)) ; only write
    ;; Get the values of style properties
    )
)

