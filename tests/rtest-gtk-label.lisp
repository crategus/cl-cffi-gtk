;;; ----------------------------------------------------------------------------
;;; rtest-gtk-label.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(define-test gtk-label
  (let ((class nil)
        (label (make-instance 'gtk-label)))
    (setq class (find-class 'gtk-label))
    (assert-eq 'gtk-label (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Get the names of the class properties
    (assert-equal
     '("user-data" "name" "parent" "width-request" "height-request" "visible"
       "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
       "can-default" "has-default" "receives-default" "composite-child"
       "style" "events" "extension-events" "no-show-all" "has-tooltip"
       "tooltip-markup" "tooltip-text" "window" "double-buffered" "xalign"
       "yalign" "xpad" "ypad" "label" "attributes" "use-markup"
       "use-underline" "justify" "pattern" "wrap" "wrap-mode" "selectable"
       "mnemonic-keyval" "mnemonic-widget" "cursor-position"
       "selection-bound" "ellipsize" "width-chars" "single-line-mode" "angle"
       "max-width-chars" "track-visited-links")
      (mapcar #'g-class-property-definition-name
              (gobject::class-properties (gtype "GtkLabel"))))
    
    ;; Get the names of the style properties.
    (assert-equal
     '("link-color" "separator-height" "focus-line-width" "separator-width"
       "draw-border" "focus-line-pattern" "wide-separators" "visited-link-color"
       "cursor-aspect-ratio" "new-tooltip-style" "focus-padding"
       "secondary-cursor-color" "scroll-arrow-hlength" "cursor-color"
       "scroll-arrow-vlength" "interior-focus")
      (mapcar #'g-class-property-definition-name
              (gtk-widget-class-list-style-properties (gtype "GtkLabel"))))
    
    ;; Properties from gtk-object
    (assert-true (pointerp (gtk-object-user-data label)))
    
    ;; Properties from gtk-widget
    (assert-equal "" (gtk-widget-name label))
    (assert-false    (gtk-widget-parent label))
    (assert-eql -1   (gtk-widget-width-request label))
    (assert-eql -1   (gtk-widget-height-request label)) 
    (assert-false    (gtk-widget-visible label))
    (assert-true     (gtk-widget-sensitive label))
    (assert-false    (gtk-widget-app-paintable label))
    (assert-false    (gtk-widget-can-focus label))
    (assert-false    (gtk-widget-has-focus label))
    (assert-false    (gtk-widget-is-focus label))
    (assert-false    (gtk-widget-can-default label))
    (assert-false    (gtk-widget-has-default label))
    (assert-false    (gtk-widget-receives-default label))
    (assert-false    (gtk-widget-composite-child label))
    (assert-true     (gtk-widget-style label)) ; value is of type GtkStyle
    (assert-false    (gtk-widget-events label))
    (assert-eq :none (gtk-widget-extension-events label))
    (assert-false    (gtk-widget-no-show-all label))
    (assert-false    (gtk-widget-has-tooltip label))
    (assert-false    (gtk-widget-tooltip-markup label))
    (assert-false    (gtk-widget-tooltip-text label))
    (assert-false    (gtk-widget-window label))
    (assert-true     (gtk-widget-double-buffered label))
    
    ;; Properties from gtk-misc
    (assert-eql 0.5  (gtk-misc-xalign label))
    (assert-eql 0.5  (gtk-misc-yalign label))
    (assert-eql 0    (gtk-misc-xpad label))
    (assert-eql 0    (gtk-misc-ypad label))
    
    ;; Properties from gtk-label
    (assert-equal ""     (gtk-label-label label))
    (assert-error 'error (gtk-label-attributes label))
    (assert-false        (gtk-label-use-markup label))
    (assert-false        (gtk-label-use-underline label))
    (assert-eq :left     (gtk-label-justify label))
    (assert-error 'error (gtk-label-pattern label)) ; not readable
    (assert-false        (gtk-label-wrap label))
    (assert-eq :word     (gtk-label-wrap-mode label))
    (assert-false        (gtk-label-selectable label))
    (assert-eql 16777215 (gtk-label-mnemonic-keyval label))
    (assert-false        (gtk-label-mnemonic-widget label))
    (assert-eql 0        (gtk-label-cursor-position label))
    (assert-eql 0        (gtk-label-selection-bound label))
    (assert-eq :none     (gtk-label-ellipsize label))
    (assert-eql -1       (gtk-label-width-chars label))
    (assert-false        (gtk-label-single-line-mode label))
    (assert-eql 0.0d0    (gtk-label-angle label))
    (assert-eql -1       (gtk-label-max-width-chars label))
    (assert-true         (gtk-label-track-visited-links label))
    
    ))
