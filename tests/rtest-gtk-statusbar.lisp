;;; ----------------------------------------------------------------------------
;;; rtest-gtk-statusbar.lisp
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

(define-test gtk-statusbar
  (assert-false (g-type-is-abstract "GtkStatusbar"))
  (assert-true  (g-type-is-derived "GtkStatusbar"))
  (assert-false (g-type-is-fundamental "GtkStatusbar"))
  (assert-true  (g-type-is-value-type "GtkStatusbar"))
  (assert-true  (g-type-has-value-table "GtkStatusbar"))
  (assert-true  (g-type-is-classed "GtkStatusbar"))
  (assert-true  (g-type-is-instantiatable "GtkStatusbar"))
  (assert-true  (g-type-is-derivable "GtkStatusbar"))
  (assert-true  (g-type-is-deep-derivable "GtkStatusbar"))
  (assert-false (g-type-is-interface "GtkStatusbar"))
  
  (let ((class (g-type-class-ref (gtype "GtkStatusbar"))))
    (assert-equal (gtype "GtkStatusbar")  (g-type-from-class class))
    (assert-equal (gtype "GtkStatusbar")
                  (g-type-from-class (g-type-class-peek "GtkStatusbar")))
    (assert-equal (gtype "GtkStatusbar")
                  (g-type-from-class (g-type-class-peek-static "GtkStatusbar")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-statusbar)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-statusbar (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkStatusbar" (gobject-class-g-type-name class))
    (assert-equal "GtkStatusbar" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_statusbar_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkBox") (g-type-parent "GtkStatusbar"))
  (assert-eql 6 (g-type-depth "GtkStatusbar"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkStatusbar" "GObject"))
  (assert-true  (g-type-is-a "GtkStatusbar" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkStatusbar" "gboolean"))
  (assert-false (g-type-is-a "GtkStatusbar" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkStatusbar")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                (mapcar #'gtype-name (g-type-interfaces "GtkStatusbar")))
  
  ;; Query infos about the class "GtkStatusbar"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkStatusbar" query)
    (assert-equal (gtype "GtkStatusbar")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkStatusbar"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 532 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
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
             (g-object-class-list-properties (gtype "GtkStatusbar"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging" "shadow-type")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkStatusbar"))))
  
  (let* ((widget (make-instance 'gtk-statusbar))
         (ptr (pointer widget)))
    ;; Read the default values of the class properties
    
    ;; Get the values of style properties
    (assert-eq :none (gtk-widget-style-get-property ptr "shadow-type")))
         
  ;; Get the definition of the class GtkStatusbar
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkStatusbar" GTK-STATUSBAR
                               (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_statusbar_get_type")
                               NIL)
    (get-g-type-definition (gtype "GtkStatusbar")))
)

;;; --- End of file rtest-gtk-label.lisp ---------------------------------------
