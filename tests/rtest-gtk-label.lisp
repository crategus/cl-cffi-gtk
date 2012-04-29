;;; ----------------------------------------------------------------------------
;;; rtest-gtk-label.lisp
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
  (assert-eql 5 (g-type-depth "GtkLabel"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkLabel" "GObject"))
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
    (assert-eql 476 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  24 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("name" "parent" "width-request" "height-request" "visible" "sensitive"
         "app-paintable" "can-focus" "has-focus" "is-focus" "can-default"
         "has-default" "receives-default" "composite-child" "style" "events"
         "no-show-all" "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "halign" "valign" "margin-left" "margin-right"
         "margin-top" "margin-bottom" "margin" "hexpand" "vexpand"
         "hexpand-set" "vexpand-set" "expand" "xalign" "yalign" "xpad" "ypad"
         "label" "attributes" "use-markup" "use-underline" "justify" "pattern"
         "wrap" "wrap-mode" "selectable" "mnemonic-keyval" "mnemonic-widget"
         "cursor-position" "selection-bound" "ellipsize" "width-chars"
         "single-line-mode" "angle" "max-width-chars" "track-visited-links")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkLabel"))))
  
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkLabel"))))
  
  ;; Read the default values of the class properties
  (let* ((widget (make-instance 'gtk-label))
         (ptr (pointer widget)))
    (assert-eql 0.0d0    (gtk-label-angle widget))
    (assert-eq 'pango-attr-list (type-of (gtk-label-attributes widget)))
    (assert-eql 0        (gtk-label-cursor-position widget))
    (assert-eq :none     (gtk-label-ellipsize widget))
    (assert-eq :left     (gtk-label-justify widget))
    (assert-equal ""     (gtk-label-label widget))
    (assert-eql -1       (gtk-label-max-width-chars widget))
    (assert-eql 16777215 (gtk-label-mnemonic-keyval widget))
    (assert-false        (gtk-label-mnemonic-widget widget))
    (assert-error 'error (gtk-label-pattern widget)) ; not readable
    (assert-false        (gtk-label-selectable widget))
    (assert-eql 0        (gtk-label-selection-bound widget))
    (assert-false        (gtk-label-single-line-mode widget))
    (assert-true         (gtk-label-track-visited-links widget))
    (assert-false        (gtk-label-use-markup widget))
    (assert-false        (gtk-label-use-underline widget))
    (assert-eql -1       (gtk-label-width-chars widget))
    (assert-false        (gtk-label-wrap widget))
    (assert-eq :word     (gtk-label-wrap-mode widget))
    ;; Get the values of style properties
    (assert-eql 0.04 (gtk-widget-style-get-property ptr "cursor-aspect-ratio"))
    (assert-false (gtk-widget-style-get-property ptr "cursor-color"))
    (assert-equal "" (gtk-widget-style-get-property ptr "focus-line-pattern"))
    (assert-eql 1 (gtk-widget-style-get-property ptr "focus-line-width"))
    (assert-eql 0 (gtk-widget-style-get-property ptr "focus-padding"))
    (assert-true (gtk-widget-style-get-property ptr "interior-focus"))
    (assert-eq 'gdk-color (type-of (gtk-widget-style-get-property ptr "link-color")))
    (assert-eql 16 (gtk-widget-style-get-property ptr "scroll-arrow-hlength"))
    (assert-eql 16 (gtk-widget-style-get-property ptr "scroll-arrow-vlength"))
    (assert-false (gtk-widget-style-get-property ptr "secondary-cursor-color"))
    (assert-eql 2 (gtk-widget-style-get-property ptr "separator-height"))
    (assert-eql 2 (gtk-widget-style-get-property ptr "separator-width"))
    (assert-eq 'gdk-color (type-of (gtk-widget-style-get-property ptr "visited-link-color")))
    (assert-true  (gtk-widget-style-get-property ptr "wide-separators"))
    (assert-false (gtk-widget-style-get-property ptr "window-dragging")))
         
  ;; Check the definition of the class gtk-label
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
)

;;; --- End of file rtest-gtk-label.lisp ---------------------------------------
