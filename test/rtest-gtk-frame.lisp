(def-suite gtk-frame :in gtk-suite)
(in-suite gtk-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFrame

(test gtk-frame-class
  ;; Type check
  (is (g-type-is-object "GtkFrame"))
  ;; Check the registered name
  (is (eq 'gtk-frame
          (registered-object-type-by-name "GtkFrame")))
  ;; Check the type initializer
  (is (eq (gtype "GtkFrame")
          (gtype (foreign-funcall "gtk_frame_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBin") (g-type-parent "GtkFrame")))
  ;; Check the children
  (is (equal '("GtkAspectFrame")
             (mapcar #'g-type-name (g-type-children "GtkFrame"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkFrame"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand" "focus-on-click"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request"
               "hexpand" "hexpand-set" "is-focus" "label" "label-widget"
               "label-xalign" "label-yalign" "margin" "margin-bottom" "margin-end"
               "margin-left" "margin-right" "margin-start" "margin-top" "name"
               "no-show-all" "opacity" "parent" "receives-default" "resize-mode"
               "scale-factor" "sensitive" "shadow-type" "style" "tooltip-markup"
               "tooltip-text" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkFrame"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
               "separator-height" "separator-width" "text-handle-height"
               "text-handle-width" "visited-link-color" "wide-separators"
               "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkFrame"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkFrame"))))
  ;; Check the class definition
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
             (get-g-type-definition "GtkFrame"))))


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
    (is (= 0.04 (gtk-widget-style-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-property widget "cursor-color"))
    (is (equal "" (gtk-widget-style-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-property widget "focus-line-width")))
    (is-true (integerp (gtk-widget-style-property widget "focus-padding")))
    (is-true (gtk-widget-style-property widget "interior-focus"))
    #-windows
    (is-false (gtk-widget-style-property widget "link-color"))
    (is (= 16 (gtk-widget-style-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-property widget "secondary-cursor-color"))
    (is-true (integerp (gtk-widget-style-property widget "separator-height")))
    (is-true (integerp (gtk-widget-style-property widget "separator-width")))
    (is (= 24 (gtk-widget-style-property widget "text-handle-height")))
    (is (= 20 (gtk-widget-style-property widget "text-handle-width")))
    #-windows
    (is-false (gtk-widget-style-property widget "visited-link-color"))
    (is-false (gtk-widget-style-property widget "wide-separators"))
    (is-false (gtk-widget-style-property widget "window-dragging"))
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

