;;; ----------------------------------------------------------------------------
;;; rtest-gtk-progress-bar.lisp
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

(define-test gtk-progress-bar
  (let* ((class (find-class 'gtk-progress-bar))
         (pbar (make-instance 'gtk-progress-bar))
         (type (g-type-from-instance (pointer pbar))))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-progress-bar (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Check the parent and the childs
    (assert-equal "GtkProgress" (gtype-name (g-type-parent type)))
    (assert-equal '()
                  (mapcar #'gtype-name (g-type-children type)))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkProgressBar" (gobject-class-g-type-name class))
    (assert-equal "GtkProgressBar" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_progress_bar_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class))
    
    (assert-equal "GtkProgressBar"
                  (g-object-class-name
                    (g-type-class-ref (gtype "GtkProgressBar"))))
    
    ;; Get the names of the class properties
    (assert-equal
     (sort
      (copy-list
        '("user-data" "name" "parent" "width-request" "height-request" "visible"
        "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
        "can-default" "has-default" "receives-default" "composite-child"
        "style" "events" "extension-events" "no-show-all" "has-tooltip"
        "tooltip-markup" "tooltip-text" "window" "double-buffered"
        "activity-mode" "show-text" "text-xalign" "text-yalign" "fraction"
        "pulse-step" "orientation" "text" "ellipsize" "adjustment" "bar-style"
          "activity-step" "activity-blocks" "discrete-blocks"))
      #'string<)
     (sort
      (mapcar #'g-class-property-definition-name
              (g-object-class-list-properties (gtype "GtkProgressBar")))
      #'string<))
    
    ;; Get the names of the style properties.
    (assert-equal
     (sort
      (copy-list
        '("draw-border" "focus-line-pattern" "secondary-cursor-color"
        "separator-width" "visited-link-color" "cursor-aspect-ratio"
        "scroll-arrow-hlength" "wide-separators" "scroll-arrow-vlength"
        "cursor-color" "interior-focus" "link-color" "separator-height"
        "focus-line-width" "focus-padding" "new-tooltip-style"
        "min-vertical-bar-width" "min-vertical-bar-height"
        "min-horizontal-bar-height" "xspacing" "yspacing"
          "min-horizontal-bar-width"))
      #'string<)
     (sort
      (mapcar #'g-class-property-definition-name
              (gtk-widget-class-list-style-properties
               (gtype "GtkProgressBar")))
      #'string<))
    
    ;; Properties from gtk-object
    (assert-true (pointerp (gtk-object-user-data pbar)))
    
    ;; Properties from gtk-widget
    (assert-equal "" (gtk-widget-name pbar))
    (assert-false    (gtk-widget-parent pbar))
    (assert-eql -1   (gtk-widget-width-request pbar))
    (assert-eql -1   (gtk-widget-height-request pbar)) 
    (assert-false    (gtk-widget-visible pbar))
    (assert-true     (gtk-widget-sensitive pbar))
    (assert-false    (gtk-widget-app-paintable pbar))
    (assert-false    (gtk-widget-can-focus pbar))
    (assert-false    (gtk-widget-has-focus pbar))
    (assert-false    (gtk-widget-is-focus pbar))
    (assert-false    (gtk-widget-can-default pbar))
    (assert-false    (gtk-widget-has-default pbar))
    (assert-false    (gtk-widget-receives-default pbar))
    (assert-false    (gtk-widget-composite-child pbar))
    (assert-true     (gtk-widget-style pbar)) ; value is of type GtkStyle
    (assert-false    (gtk-widget-events pbar))
    (assert-eq :none (gtk-widget-extension-events pbar))
    (assert-false    (gtk-widget-no-show-all pbar))
    (assert-false    (gtk-widget-has-tooltip pbar))
    (assert-false    (gtk-widget-tooltip-markup pbar))
    (assert-false    (gtk-widget-tooltip-text pbar))
    (assert-false    (gtk-widget-window pbar))
    (assert-true     (gtk-widget-double-buffered pbar))
    
    ;; Properties from gtk-progress
;    (assert-false    (gtk-progress-activity-mode pbar))
;    (assert-false    (gtk-progress-show-text pbar))
;    (assert-false    (gtk-progress-xalign pbar))
;    (assert-false    (gtk-progress-yalign pbar))
    
    ;; Properties from gtk-progress-bar
    (assert-eql 0.0d0 (gtk-progress-bar-fraction pbar))
    (assert-eql 0.1d0 (gtk-progress-bar-pulse-step pbar))
    (assert-eq :left-to-right (gtk-progress-bar-orientation pbar))
    (assert-false     (gtk-progress-bar-text pbar))
    (assert-eq :none  (gtk-progress-bar-ellipsize pbar))
    (assert-true      (gtk-progress-bar-adjustment pbar))
    (assert-eq :continuous (gtk-progress-bar-bar-style pbar))
    (assert-eql  3    (gtk-progress-bar-activity-step pbar))
    (assert-eql  5    (gtk-progress-bar-activity-blocks pbar))
    (assert-eql 10    (gtk-progress-bar-discrete-blocks pbar))
    ))

;;; --- End of file rtest-gtk-progress-bar.lisp --------------------------------
