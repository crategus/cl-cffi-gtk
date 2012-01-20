;;; ----------------------------------------------------------------------------
;;; rtest-gobject.lisp
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

(define-test gtk-widget
  (let ((class (find-class 'gtk-widget)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-widget (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkWidget" (gobject-class-g-type-name class))
    (assert-equal "GtkWidget" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_widget_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class))
    
    (assert-equal "GtkWidget"
                  (g-object-class-name (g-type-class-ref (gtype "GtkWidget"))))
    
    ;; Get the names of the class properties
    (assert-equal
      '("user-data" "name" "parent" "width-request" "height-request" "visible"
        "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
        "can-default" "has-default" "receives-default" "composite-child" "style"
        "events" "extension-events" "no-show-all" "has-tooltip" "tooltip-markup"
        "tooltip-text" "window" "double-buffered")
     (mapcar #'g-class-property-definition-name
             (g-object-class-list-properties (gtype "GtkWidget"))))
    
    ;; Get the names of the style properties.
    (assert-equal
     '("link-color" "separator-height" "focus-line-width" "separator-width"
       "draw-border" "focus-line-pattern" "wide-separators" "visited-link-color"
       "cursor-aspect-ratio" "new-tooltip-style" "focus-padding"
       "secondary-cursor-color" "scroll-arrow-hlength" "cursor-color"
       "scroll-arrow-vlength" "interior-focus")     
      (mapcar #'g-class-property-definition-name
              (gtk-widget-class-list-style-properties (gtype "GtkWidget"))))
    ))
