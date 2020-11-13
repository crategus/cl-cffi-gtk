(def-suite gtk-accel-label :in gtk-suite)
(in-suite gtk-accel-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccelLabel

(test gtk-accel-label-class
  ;; Type check
  (is (g-type-is-object "GtkAccelLabel"))
  ;; Check the registered name
  (is (eq 'gtk-accel-label
          (registered-object-type-by-name "GtkAccelLabel")))
  ;; Check the type initializer
  (is (eq (gtype "GtkAccelLabel")
          (gtype (foreign-funcall "gtk_accel_label_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkLabel") (g-type-parent "GtkAccelLabel")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkAccelLabel"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkAccelLabel"))))
  ;; Check the class properties
  (is (equal '("accel-closure" "accel-widget" "angle" "app-paintable" "attributes"
               "can-default" "can-focus" "composite-child" "cursor-position"
               "double-buffered" "ellipsize" "events" "expand" "focus-on-click" "halign"
               "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "is-focus" "justify" "label" "lines" "margin" "margin-bottom"
               "margin-end" "margin-left" "margin-right" "margin-start" "margin-top"
               "max-width-chars" "mnemonic-keyval" "mnemonic-widget" "name" "no-show-all"
               "opacity" "parent" "pattern" "receives-default" "scale-factor" "selectable"
               "selection-bound" "sensitive" "single-line-mode" "style" "tooltip-markup"
               "tooltip-text" "track-visited-links" "use-markup" "use-underline" "valign"
               "vexpand" "vexpand-set" "visible" "width-chars" "width-request" "window"
               "wrap" "wrap-mode" "xalign" "xpad" "yalign" "ypad")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkAccelLabel"))
                          #'string-lessp)))
  ;; Check the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkAccelLabel"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAccelLabel" GTK-ACCEL-LABEL
                       (:SUPERCLASS GTK-LABEL :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_accel_label_get_type")
                       ((ACCEL-CLOSURE GTK-ACCEL-LABEL-ACCEL-CLOSURE
                         "accel-closure" "GClosure" T T)
                        (ACCEL-WIDGET GTK-ACCEL-LABEL-ACCEL-WIDGET
                         "accel-widget" "GtkWidget" T T)))
             (get-g-type-definition "GtkAccelLabel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-accel-label-properties
  (let ((accel-label (make-instance 'gtk-accel-label :label "text")))
    ;; TODO: GClosure is in C implemented as a boxed type, but not in Lisp
    ;; therefore we get an error with the accessor
;    (is-false (gtk-accel-label-accel-closure accel-label))
    (is-false (gtk-accel-label-accel-widget accel-label))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accel_label_new

(test gtk-accel-label-new
  (is (eq 'gtk-accel-label (gtk-accel-label-new "text"))))

;;;     gtk_accel_label_get_accel_width

(test gtk-accel-label-new
  (let ((accel-label (gtk-accel-label-new "text")))
    (is (= 0 (gtk-accel-label-accel-width accel-label)))))

;;;     gtk_accel_label_set_accel
;;;     gtk_accel_label_get_accel

(test gtk-accel-label-accel
  (let ((accel-label (gtk-accel-label-new "text")))
    (is-false (gtk-accel-label-set-accel accel-label
                                         (gdk-keyval-from-name "p")
                                         :control-mask))
    (multiple-value-bind (key mods)
        (gtk-accel-label-get-accel accel-label)
      (is (= 112 key))
      (is (equal '(:control-mask) mods)))))

;;;     gtk_accel_label_refetch

