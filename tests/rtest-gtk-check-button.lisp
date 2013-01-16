;;; ----------------------------------------------------------------------------
;;; rtest-gtk-check-button.lisp
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

(define-test gtk-check-button
  (assert-false (g-type-is-abstract "GtkCheckButton"))
  (assert-true  (g-type-is-derived "GtkCheckButton"))
  (assert-false (g-type-is-fundamental "GtkCheckButton"))
  (assert-true  (g-type-is-value-type "GtkCheckButton"))
  (assert-true  (g-type-has-value-table "GtkCheckButton"))
  (assert-true  (g-type-is-classed "GtkCheckButton"))
  (assert-true  (g-type-is-instantiatable "GtkCheckButton"))
  (assert-true  (g-type-is-derivable "GtkCheckButton"))
  (assert-true  (g-type-is-deep-derivable "GtkCheckButton"))
  (assert-false (g-type-is-interface "GtkCheckButton"))
  
  (let ((class (g-type-class-ref (gtype "GtkCheckButton"))))
    (assert-equal (gtype "GtkCheckButton")  (g-type-from-class class))
    (assert-equal (gtype "GtkCheckButton")
                  (g-type-from-class (g-type-class-peek "GtkCheckButton")))
    (assert-equal (gtype "GtkCheckButton")
                  (g-type-from-class (g-type-class-peek-static "GtkCheckButton")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-check-button)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-check-button (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkCheckButton" (gobject-class-g-type-name class))
    (assert-equal "GtkCheckButton" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_check_button_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkToggleButton") (g-type-parent "GtkCheckButton"))
  (assert-eql 8 (g-type-depth "GtkCheckButton"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkCheckButton" "GObject"))
  (assert-true  (g-type-is-a "GtkCheckButton" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkCheckButton" "gboolean"))
  (assert-false (g-type-is-a "GtkCheckButton" "GtkWindow"))
  (assert-equal '("GtkRadioButton")
                (mapcar #'gtype-name (g-type-children "GtkCheckButton")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
                  "GtkActivatable")
                (mapcar #'gtype-name (g-type-interfaces "GtkCheckButton")))
  
  ;; Query infos about the class "GtkLabel"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkCheckButton" query)
    (assert-equal (gtype "GtkCheckButton")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkCheckButton"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 584 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  32 (foreign-slot-value query 'g-type-query :instance-size)))
  
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
         "focus-on-click" "xalign" "yalign" "image-position"
         "always-show-image" "active" "inconsistent" "draw-indicator")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkCheckButton"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "text-handle-height"
         "text-handle-width" "visited-link-color" "wide-separators"
         "window-dragging" "child-displacement-x" "child-displacement-y"
         "default-border" "default-outside-border" "displace-focus"
         "image-spacing" "inner-border" "indicator-size" "indicator-spacing")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkCheckButton"))))
  
  ;; Create a GtkCheckButton
  (let* ((widget (make-instance 'gtk-check-button))
         (ptr (pointer widget)))
    (assert-eq 'gtk-check-button (type-of widget))
    (assert-equal (gtype "GtkCheckButton") (g-type-from-instance ptr))
    ;; Read the defalut values of the class properties
    ;; Get the values of style properties
    )
)

;;; --- End of file rtest-gtk-check-button.lisp --------------------------------
