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
  (assert-false (g-type-is-abstract "GtkLabel"))  
  (assert-true  (g-type-is-derived "GtkLabel"))  
  (assert-false (g-type-is-fundamental "GtkLabel"))  
  (assert-true  (g-type-is-value-type "GtkLabel"))
  (assert-true  (g-type-has-value-table "GtkLabel"))
  (assert-true  (g-type-is-classed "GtkLabel"))  
  (assert-true  (g-type-is-instantiatable "GtkLabel"))
  (assert-true  (g-type-is-derivable "GtkLabel"))
  (assert-true  (g-type-is-deep-derivable "GtkLabel"))
  (assert-false (g-type-is-interface "GtkLabel"))
  
  (let ((class (g-type-class-ref (gtype "GtkLabel"))))
    (assert-equal (gtype "GtkLabel")  (g-type-from-class class))
    (assert-equal (gtype "GtkLabel")
                  (g-type-from-class (g-type-class-peek "GtkLabel")))
    (assert-equal (gtype "GtkLabel")
                  (g-type-from-class (g-type-class-peek-static "GtkLabel")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-label)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-label (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkLabel" (gobject-class-g-type-name class))
    (assert-equal "GtkLabel" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_label_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GtkMisc") (g-type-parent "GtkLabel"))
  (assert-eql 6 (g-type-depth "GtkLabel"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkLabel" "GObject"))
  (assert-true  (g-type-is-a "GtkLabel" "GtkLabel"))
  (assert-true  (g-type-is-a "GtkLabel" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkLabel" "gboolean"))
  (assert-false (g-type-is-a "GtkLabel" "GtkWindow"))
  (assert-equal '("GtkAccelLabel")
                (mapcar #'gtype-name (g-type-children "GtkLabel")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable")
                (mapcar #'gtype-name (g-type-interfaces "GtkLabel")))
  
  ;; Query infos about the class "GtkLabel"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkLabel" query)
    (assert-equal (gtype "GtkLabel")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkLabel"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 392 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 112 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
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
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkLabel"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "draw-border"
        "focus-line-pattern" "focus-line-width" "focus-padding"
        "interior-focus" "link-color" "new-tooltip-style"
        "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
        "separator-height" "separator-width" "visited-link-color"
        "wide-separators")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkLabel"))))
  
  (let ((label (make-instance 'gtk-label)))    
    ;; Properties from gtk-object
    (assert-true         (pointerp (gtk-object-user-data label)))
    ;; Properties from gtk-widget
    (assert-equal ""     (gtk-widget-name label))
    (assert-false        (gtk-widget-parent label))
    (assert-eql -1       (gtk-widget-width-request label))
    (assert-eql -1       (gtk-widget-height-request label)) 
    (assert-false        (gtk-widget-visible label))
    (assert-true         (gtk-widget-sensitive label))
    (assert-false        (gtk-widget-app-paintable label))
    (assert-false        (gtk-widget-can-focus label))
    (assert-false        (gtk-widget-has-focus label))
    (assert-false        (gtk-widget-is-focus label))
    (assert-false        (gtk-widget-can-default label))
    (assert-false        (gtk-widget-has-default label))
    (assert-false        (gtk-widget-receives-default label))
    (assert-false        (gtk-widget-composite-child label))
    (assert-true         (gtk-widget-style label)) ; value is of type GtkStyle
    (assert-false        (gtk-widget-events label))
    (assert-eq :none     (gtk-widget-extension-events label))
    (assert-false        (gtk-widget-no-show-all label))
    (assert-false        (gtk-widget-has-tooltip label))
    (assert-false        (gtk-widget-tooltip-markup label))
    (assert-false        (gtk-widget-tooltip-text label))
    (assert-false        (gtk-widget-window label))
    (assert-true         (gtk-widget-double-buffered label))
    ;; Properties from gtk-misc
    (assert-eql 0.5      (gtk-misc-xalign label))
    (assert-eql 0.5      (gtk-misc-yalign label))
    (assert-eql 0        (gtk-misc-xpad label))
    (assert-eql 0        (gtk-misc-ypad label))
    ;; Properties from gtk-label
    (assert-equal ""     (gtk-label-label label))
    (assert-true         (gtk-label-attributes label)) ; Returns PangoAttrList
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
    (assert-true         (gtk-label-track-visited-links label)))
    
  ;; Check the defintion of the class gtk-window
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkLabel" GTK-LABEL
                               (:SUPERCLASS GTK-MISC :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_label_get_type")
                               ((ANGLE GTK-LABEL-ANGLE "angle" "gdouble" T T)
                                (ATTRIBUTES GTK-LABEL-ATTRIBUTES "attributes"
                                 "PangoAttrList" T T)
                                (CURSOR-POSITION GTK-LABEL-CURSOR-POSITION
                                 "cursor-position" "gint" T NIL)
                                (ELLIPSIZE GTK-LABEL-ELLIPSIZE "ellipsize"
                                 "PangoEllipsizeMode" T T)
                                (JUSTIFY GTK-LABEL-JUSTIFY "justify"
                                 "GtkJustification" T T)
                                (LABEL GTK-LABEL-LABEL "label" "gchararray" T
                                 T)
                                (MAX-WIDTH-CHARS GTK-LABEL-MAX-WIDTH-CHARS
                                 "max-width-chars" "gint" T T)
                                (MNEMONIC-KEYVAL GTK-LABEL-MNEMONIC-KEYVAL
                                 "mnemonic-keyval" "guint" T NIL)
                                (MNEMONIC-WIDGET GTK-LABEL-MNEMONIC-WIDGET
                                 "mnemonic-widget" "GtkWidget" T T)
                                (PATTERN GTK-LABEL-PATTERN "pattern"
                                 "gchararray" NIL T)
                                (SELECTABLE GTK-LABEL-SELECTABLE "selectable"
                                 "gboolean" T T)
                                (SELECTION-BOUND GTK-LABEL-SELECTION-BOUND
                                 "selection-bound" "gint" T NIL)
                                (SINGLE-LINE-MODE GTK-LABEL-SINGLE-LINE-MODE
                                 "single-line-mode" "gboolean" T T)
                                (TRACK-VISITED-LINKS
                                 GTK-LABEL-TRACK-VISITED-LINKS
                                 "track-visited-links" "gboolean" T T)
                                (USE-MARKUP GTK-LABEL-USE-MARKUP "use-markup"
                                 "gboolean" T T)
                                (USE-UNDERLINE GTK-LABEL-USE-UNDERLINE
                                 "use-underline" "gboolean" T T)
                                (WIDTH-CHARS GTK-LABEL-WIDTH-CHARS
                                 "width-chars" "gint" T T)
                                (WRAP GTK-LABEL-WRAP "wrap" "gboolean" T T)
                                (WRAP-MODE GTK-LABEL-WRAP-MODE "wrap-mode"
                                 "PangoWrapMode" T T)))
    (get-g-class-definition (gtype "GtkLabel")))
    
  ;; Check the expansion of the class definition
  (assert-equal
    '(PROGN
         (DEFCLASS GTK-LABEL (GTK-MISC ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE)
                   ((ANGLE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gdouble" :ACCESSOR GTK-LABEL-ANGLE :INITARG :ANGLE
                     :G-PROPERTY-NAME "angle")
                    (ATTRIBUTES :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "PangoAttrList" :ACCESSOR GTK-LABEL-ATTRIBUTES :INITARG
                     :ATTRIBUTES :G-PROPERTY-NAME "attributes")
                    (CURSOR-POSITION :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gint" :ACCESSOR
                     GTK-LABEL-CURSOR-POSITION :INITARG :CURSOR-POSITION
                     :G-PROPERTY-NAME "cursor-position")
                    (ELLIPSIZE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "PangoEllipsizeMode" :ACCESSOR GTK-LABEL-ELLIPSIZE
                     :INITARG :ELLIPSIZE :G-PROPERTY-NAME "ellipsize")
                    (JUSTIFY :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GtkJustification" :ACCESSOR GTK-LABEL-JUSTIFY :INITARG
                     :JUSTIFY :G-PROPERTY-NAME "justify")
                    (LABEL :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gchararray" :ACCESSOR GTK-LABEL-LABEL :INITARG :LABEL
                     :G-PROPERTY-NAME "label")
                    (MAX-WIDTH-CHARS :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gint" :ACCESSOR
                     GTK-LABEL-MAX-WIDTH-CHARS :INITARG :MAX-WIDTH-CHARS
                     :G-PROPERTY-NAME "max-width-chars")
                    (MNEMONIC-KEYVAL :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "guint" :ACCESSOR
                     GTK-LABEL-MNEMONIC-KEYVAL :INITARG :MNEMONIC-KEYVAL
                     :G-PROPERTY-NAME "mnemonic-keyval")
                    (MNEMONIC-WIDGET :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "GtkWidget" :ACCESSOR
                     GTK-LABEL-MNEMONIC-WIDGET :INITARG :MNEMONIC-WIDGET
                     :G-PROPERTY-NAME "mnemonic-widget")
                    (PATTERN :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gchararray" :ACCESSOR GTK-LABEL-PATTERN :INITARG :PATTERN
                     :G-PROPERTY-NAME "pattern")
                    (SELECTABLE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-LABEL-SELECTABLE :INITARG
                     :SELECTABLE :G-PROPERTY-NAME "selectable")
                    (SELECTION-BOUND :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gint" :ACCESSOR
                     GTK-LABEL-SELECTION-BOUND :INITARG :SELECTION-BOUND
                     :G-PROPERTY-NAME "selection-bound")
                    (SINGLE-LINE-MODE :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-LABEL-SINGLE-LINE-MODE :INITARG :SINGLE-LINE-MODE
                     :G-PROPERTY-NAME "single-line-mode")
                    (TRACK-VISITED-LINKS :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-LABEL-TRACK-VISITED-LINKS :INITARG
                     :TRACK-VISITED-LINKS :G-PROPERTY-NAME
                     "track-visited-links")
                    (USE-MARKUP :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-LABEL-USE-MARKUP :INITARG
                     :USE-MARKUP :G-PROPERTY-NAME "use-markup")
                    (USE-UNDERLINE :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-LABEL-USE-UNDERLINE :INITARG :USE-UNDERLINE
                     :G-PROPERTY-NAME "use-underline")
                    (WIDTH-CHARS :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gint" :ACCESSOR GTK-LABEL-WIDTH-CHARS :INITARG
                     :WIDTH-CHARS :G-PROPERTY-NAME "width-chars")
                    (WRAP :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-LABEL-WRAP :INITARG :WRAP
                     :G-PROPERTY-NAME "wrap")
                    (WRAP-MODE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "PangoWrapMode" :ACCESSOR GTK-LABEL-WRAP-MODE :INITARG
                     :WRAP-MODE :G-PROPERTY-NAME "wrap-mode"))
                   (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkLabel")
                   (:G-TYPE-INITIALIZER . "gtk_label_get_type"))
         (EXPORT 'GTK-LABEL (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-ANGLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-ATTRIBUTES (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-CURSOR-POSITION (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-ELLIPSIZE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-JUSTIFY (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-LABEL (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-MAX-WIDTH-CHARS (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-MNEMONIC-KEYVAL (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-MNEMONIC-WIDGET (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-PATTERN (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-SELECTABLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-SELECTION-BOUND (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-SINGLE-LINE-MODE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-TRACK-VISITED-LINKS (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-USE-MARKUP (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-USE-UNDERLINE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-WIDTH-CHARS (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-WRAP (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-LABEL-WRAP-MODE (FIND-PACKAGE "GTK")))
    ;; macroexpand the class definition
    (macroexpand-1 (get-g-class-definition (gtype "GtkLabel"))))
)

;;; --- End of file rtest-gtk-label.lisp ---------------------------------------
