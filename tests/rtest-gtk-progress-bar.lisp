;;; ----------------------------------------------------------------------------
;;; rtest-gtk-progress-bar.lisp
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

(define-test gtk-progress-bar
  (let* ((class (find-class 'gtk-progress-bar))
         (pbar (make-instance 'gtk-progress-bar))
         (type (g-type-from-instance (pointer pbar))))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-progress-bar (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Check the parent and the childs
    (assert-equal "GtkWidget" (gtype-name (g-type-parent type)))
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
        '("orientation" "name" "parent" "width-request" "height-request"
         "visible" "sensitive" "app-paintable" "can-focus" "has-focus"
         "is-focus" "can-default" "has-default" "receives-default"
         "composite-child" "style" "events" "no-show-all" "has-tooltip"
         "tooltip-markup" "tooltip-text" "window" "double-buffered" "halign"
         "valign" "margin-left" "margin-right" "margin-top" "margin-bottom"
         "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
         "fraction" "pulse-step" "inverted" "text" "show-text" "ellipsize")
      (mapcar #'param-spec-name
              (g-object-class-list-properties (gtype "GtkProgressBar"))))
    
    ;; Get the names of the style properties.
    (assert-equal
        '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging" "min-horizontal-bar-height"
         "min-horizontal-bar-width" "min-vertical-bar-height"
         "min-vertical-bar-width" "xspacing" "yspacing")
      (mapcar #'param-spec-name
              (gtk-widget-class-list-style-properties
               (gtype "GtkProgressBar"))))
        
    ;; Properties from gtk-progress-bar
    (assert-eq :none  (gtk-progress-bar-get-ellipsize pbar))
    (assert-eql 0.0d0 (gtk-progress-bar-get-fraction pbar))
    (assert-false     (gtk-progress-bar-get-inverted pbar))
    (assert-eql 0.1d0 (gtk-progress-bar-get-pulse-step pbar))
    (assert-false     (gtk-progress-bar-get-show-text pbar))
    (assert-false     (gtk-progress-bar-get-text pbar))
    ))

;;; --- End of file rtest-gtk-progress-bar.lisp --------------------------------
