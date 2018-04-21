(in-package :gtk-testsuite)

(def-suite gtk-frame :in gtk-suite)
(in-suite gtk-frame)

;;;   GtkFrame

(test gtk-frame-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkFrame"))
  (is-false (g-type-is-abstract "GtkFrame"))
  (is-true  (g-type-is-derived "GtkFrame"))
  (is-false (g-type-is-fundamental "GtkFrame"))
  (is-true  (g-type-is-value-type "GtkFrame"))
  (is-true  (g-type-has-value-table "GtkFrame"))
  (is-true  (g-type-is-classed "GtkFrame"))
  (is-true  (g-type-is-instantiatable "GtkFrame"))
  (is-true  (g-type-is-derivable "GtkFrame"))
  (is-true  (g-type-is-deep-derivable "GtkFrame"))
  (is-false (g-type-is-interface "GtkFrame"))

  ;; Check the registered name
  (is (eq 'gtk-frame
          (registered-object-type-by-name "GtkFrame")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkFrame"))))
    (is (equal (gtype "GtkFrame") (g-type-from-class class)))
    (is (equal (gtype "GtkFrame") (g-object-class-type class)))
    (is (equal "GtkFrame" (g-object-class-name class)))
    (is (equal (gtype "GtkFrame") (g-type-from-class  (g-type-class-peek "GtkFrame"))))
    (is (equal (gtype "GtkFrame") (g-type-from-class  (g-type-class-peek-static "GtkFrame"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-frame)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-frame (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkFrame" (gobject-class-g-type-name class)))
    (is (equal "GtkFrame" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_frame_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GtkBin") (g-type-parent "GtkFrame")))
  (is (= 6 (g-type-depth "GtkFrame")))
  (is (equal (gtype "GtkContainer")
             (g-type-next-base "GtkFrame" "GtkWidget")))
  (is-true  (g-type-is-a "GtkFrame" "GtkWidget"))
  (is-true  (g-type-is-a "GtkFrame" "GtkContainer"))
  (is-false (g-type-is-a "GtkFrame" "gboolean"))
  (is-false (g-type-is-a "GtkFrame" "GtkWindow"))

  ;; Check the children
  (is (equal '("GtkAspectFrame")
             (mapcar #'gtype-name (g-type-children "GtkFrame"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkFrame"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkFrame" query)
    (is (equal (gtype "GtkFrame")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkFrame"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 1048 
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 56
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (subsetp '("name" "parent" "width-request" "height-request" "visible"
                 "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
                 "can-default" "has-default" "receives-default" "composite-child"
                 "style" "events" "no-show-all" "has-tooltip" "tooltip-markup"
                 "tooltip-text" "window" "opacity" "double-buffered" "halign"
                 "valign" "margin-left" "margin-right" "margin-top"
                 "margin-bottom" "margin" "hexpand" "vexpand" "hexpand-set"
                 "vexpand-set" "expand" "scale-factor" "border-width"
                 "resize-mode" "child" "label" "label-xalign" "label-yalign"
                 "shadow-type" "label-widget")
               (mapcar #'param-spec-name
                       (g-object-class-list-properties "GtkFrame"))
               :test #'string=))
             
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'param-spec-name (gtk-widget-class-list-style-properties "GtkFrame"))))

  ;; Get the names to the child properties
  (is (equal '()
             (mapcar #'param-spec-name (gtk-container-class-list-child-properties "GtkFrame"))))

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFrame" GTK-FRAME
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_frame_get_type")
                       ((LABEL GTK-FRAME-LABEL "label" "gchararray" T T)
                        (LABEL-WIDGET GTK-FRAME-LABEL-WIDGET "label-widget"
                         "GtkWidget" T T)
                        (LABEL-XALIGN GTK-FRAME-LABEL-XALIGN "label-xalign"
                         "gfloat" T T)
                        (LABEL-YALIGN GTK-FRAME-LABEL-YALIGN "label-yalign"
                         "gfloat" T T)
                        (SHADOW-TYPE GTK-FRAME-SHADOW-TYPE "shadow-type"
                         "GtkShadowType" T T)))
             (get-g-type-definition "GtkFrame")))
)

(test gtk-frame-properties
  (let ((widget (make-instance 'gtk-frame)))
    (is-false (gtk-frame-label widget))
    (is-false (gtk-frame-label-widget widget))
    (is (= 0.0 (gtk-frame-label-xalign widget)))
    (is (= 0.5 (gtk-frame-label-yalign widget)))
    (is (eq :etched-in (gtk-frame-shadow-type widget)))
  ))

(test gtk-frame-style-properties
  (let ((widget (make-instance 'gtk-frame)))
    (is (= 0.04 (gtk-widget-style-get-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-get-property widget "cursor-color"))
    (is (equal "" (gtk-widget-style-get-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-get-property widget "focus-line-width")))
    (is-true (integerp (gtk-widget-style-get-property widget "focus-padding")))
    (is-true (gtk-widget-style-get-property widget "interior-focus"))
    #-windows
    (is (eq 'gdk-color (type-of (gtk-widget-style-get-property widget "link-color"))))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-get-property widget "secondary-cursor-color"))
    (is-true (integerp (gtk-widget-style-get-property widget "separator-height")))
    (is-true (integerp (gtk-widget-style-get-property widget "separator-width")))
    (is (= 20 (gtk-widget-style-get-property widget "text-handle-height")))
    (is (= 16 (gtk-widget-style-get-property widget "text-handle-width")))
    #-windows
    (is (eq 'gdk-color (type-of (gtk-widget-style-get-property widget "visited-link-color"))))
    (is-false  (gtk-widget-style-get-property widget "wide-separators"))
    (is-false (gtk-widget-style-get-property widget "window-dragging"))
  ))

;;;     gtk_frame_new
;;;     gtk_frame_set_label
;;;     gtk_frame_set_label_widget
;;;     gtk_frame_set_label_align
;;;     gtk_frame_set_shadow_type
;;;     gtk_frame_get_label
;;;     gtk_frame_get_label_align
;;;     gtk_frame_get_label_widget
;;;     gtk_frame_get_shadow_type

