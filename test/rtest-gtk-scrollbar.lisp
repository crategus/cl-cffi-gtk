(def-suite gtk-scrollbar :in gtk-suite)
(in-suite gtk-scrollbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollbar

(test gtk-scrollbar-class
  ;; Type check
  (is-true  (g-type-is-object "GtkScrollbar"))
  ;; Check the registered name
  (is (eq 'gtk-scrollbar
          (registered-object-type-by-name "GtkScrollbar")))
  ;; Check the type initializer
  (is (string= "GtkScrollbar"
               (g-type-name (gtype (foreign-funcall "gtk_scrollbar_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkRange") (g-type-parent "GtkScrollbar")))
  ;; Check the children
  (is (equal '("GtkHScrollbar" "GtkVScrollbar")
             (mapcar #'gtype-name (g-type-children "GtkScrollbar"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'gtype-name (g-type-interfaces "GtkScrollbar"))))
  ;; Check the class properties
  (is (equal '("adjustment" "app-paintable" "can-default" "can-focus" "composite-child"
               "double-buffered" "events" "expand" "fill-level" "focus-on-click" "halign"
               "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "inverted" "is-focus" "lower-stepper-sensitivity" "margin"
               "margin-bottom" "margin-end" "margin-left" "margin-right" "margin-start"
               "margin-top" "name" "no-show-all" "opacity" "orientation" "parent"
               "receives-default" "restrict-to-fill-level" "round-digits" "scale-factor"
               "sensitive" "show-fill-level" "style" "tooltip-markup" "tooltip-text"
               "upper-stepper-sensitivity" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkScrollbar"))
                          #'string-lessp)))
  ;; Check the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging"
               "arrow-displacement-x" "arrow-displacement-y" "arrow-scaling" "slider-width"
               "stepper-size" "stepper-spacing" "trough-border" "trough-under-steppers"
               "fixed-slider-length" "has-backward-stepper" "has-forward-stepper"
               "has-secondary-backward-stepper" "has-secondary-forward-stepper"
               "min-slider-length")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkScrollbar"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkScrollbar" GTK-SCROLLBAR
                       (:SUPERCLASS GTK-RANGE :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scrollbar_get_type")
                       NIL)
             (get-g-type-definition "GtkScrollbar"))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-scrollbar-properties
  (let ((window (make-instance 'gtk-scrollbar)))
    (is-false (gtk-widget-style-get-property window "fixed-slider-length"))
    (is-false (gtk-widget-style-get-property window "has-backward-stepper"))
    (is-false (gtk-widget-style-get-property window "has-forward-stepper"))
    (is-false (gtk-widget-style-get-property window "has-secondary-backward-stepper"))
    (is-false (gtk-widget-style-get-property window "has-secondary-forward-stepper"))
    (is (= 21 (gtk-widget-style-get-property window "min-slider-length")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollbar_new

(test gtk-scrollbar-new
  (is (eq 'gtk-scrollbar (type-of (gtk-scrollbar-new :horizontal))))
  (is (eq 'gtk-scrollbar (type-of (gtk-scrollbar-new :vertical (make-instance 'gtk-adjustment))))))

