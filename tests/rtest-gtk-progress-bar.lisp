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
  (assert-false (g-type-is-abstract "GtkProgressBar"))
  (assert-true  (g-type-is-derived "GtkProgressBar"))
  (assert-false (g-type-is-fundamental "GtkProgressBar"))
  (assert-true  (g-type-is-value-type "GtkProgressBar"))
  (assert-true  (g-type-has-value-table "GtkProgressBar"))
  (assert-true  (g-type-is-classed "GtkProgressBar"))
  (assert-true  (g-type-is-instantiatable "GtkProgressBar"))
  (assert-true  (g-type-is-derivable "GtkProgressBar"))
  (assert-true  (g-type-is-deep-derivable "GtkProgressBar"))
  (assert-false (g-type-is-interface "GtkProgressBar"))
  
  (let ((class (g-type-class-ref (gtype "GtkProgressBar"))))
    (assert-equal (gtype "GtkProgressBar")  (g-type-from-class class))
    (assert-equal (gtype "GtkProgressBar")
                  (g-type-from-class (g-type-class-peek "GtkProgressBar")))
    (assert-equal (gtype "GtkProgressBar")
                  (g-type-from-class (g-type-class-peek-static "GtkProgressBar")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-progress-bar)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-progress-bar (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkProgressBar" (gobject-class-g-type-name class))
    (assert-equal "GtkProgressBar" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_progress_bar_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkWidget") (g-type-parent "GtkProgressBar"))
  (assert-eql 4 (g-type-depth "GtkProgressBar"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkProgressBar" "GObject"))
  (assert-true  (g-type-is-a "GtkProgressBar" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkProgressBar" "gboolean"))
  (assert-false (g-type-is-a "GtkProgressBar" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkProgressBar")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkProgressBar")))
  
  ;; Query infos about the class "GtkProgressBar"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkProgressBar" query)
    (assert-equal (gtype "GtkProgressBar")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkProgressBar"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 428 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  20 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
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
                (gtk-widget-class-list-style-properties (gtype "GtkProgressBar"))))
  
  (let* ((widget (make-instance 'gtk-progress-bar))
         (ptr (pointer widget)))
    ;; Read the default values of the class properties
    (assert-eq :none  (gtk-progress-bar-ellipsize widget))
    (assert-eql 0.0d0 (gtk-progress-bar-fraction widget))
    (assert-false     (gtk-progress-bar-inverted widget))
    (assert-eql 0.1d0 (gtk-progress-bar-pulse-step widget))
    (assert-false     (gtk-progress-bar-show-text widget))
    (assert-false     (gtk-progress-bar-text widget))
    ;; Get the values of style properties    
    (assert-eql  14 (gtk-widget-style-get-property ptr "min-horizontal-bar-height"))
    (assert-eql 150 (gtk-widget-style-get-property ptr "min-horizontal-bar-width"))
    (assert-eql  80 (gtk-widget-style-get-property ptr "min-vertical-bar-height"))
    (assert-eql  14 (gtk-widget-style-get-property ptr "min-vertical-bar-width"))
    (assert-eql   7 (gtk-widget-style-get-property ptr "xspacing"))
    (assert-eql   7 (gtk-widget-style-get-property ptr "yspacing")))
    
  ;; Get the definition of the class GtkProgressBar
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkProgressBar" GTK-PROGRESS-BAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_progress_bar_get_type")
                               ((ELLIPSIZE GTK-PROGRESS-BAR-ELLIPSIZE
                                 "ellipsize" "PangoEllipsizeMode" T T)
                                (FRACTION GTK-PROGRESS-BAR-FRACTION "fraction"
                                 "gdouble" T T)
                                (INVERTED GTK-PROGRESS-BAR-INVERTED "inverted"
                                 "gboolean" T T)
                                (PULSE-STEP GTK-PROGRESS-BAR-PULSE-STEP
                                 "pulse-step" "gdouble" T T)
                                (SHOW-TEXT GTK-PROGRESS-BAR-SHOW-TEXT
                                 "show-text" "gboolean" T T)
                                (TEXT GTK-PROGRESS-BAR-TEXT "text" "gchararray"
                                 T T)))
    (get-g-class-definition (gtype "GtkProgressBar")))
)

;;; --- End of file rtest-gtk-progress-bar.lisp --------------------------------
