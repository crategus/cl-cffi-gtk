(def-suite gtk-progress-bar :in gtk-suite)
(in-suite gtk-progress-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkProgressBar

(test gtk-progress-bar-class
  ;; Type check
  (is-true  (g-type-is-object "GtkProgressBar"))
  ;; Check the registered name
  (is (eq 'gtk-progress-bar
          (registered-object-type-by-name "GtkProgressBar")))
  ;; Check the type initializer
  (is (string= "GtkProgressBar"
               (g-type-name (gtype (foreign-funcall "gtk_progress_bar_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkWidget") (g-type-parent "GtkProgressBar")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkProgressBar"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'gtype-name (g-type-interfaces "GtkProgressBar"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "can-default" "can-focus" "composite-child" "double-buffered"
               "ellipsize" "events" "expand" "focus-on-click" "fraction" "halign"
               "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "inverted" "is-focus" "margin" "margin-bottom" "margin-end"
               "margin-left" "margin-right" "margin-start" "margin-top" "name" "no-show-all"
               "opacity" "orientation" "parent" "pulse-step" "receives-default"
               "scale-factor" "sensitive" "show-text" "style" "text" "tooltip-markup"
               "tooltip-text" "valign" "vexpand" "vexpand-set" "visible" "width-request"
               "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkProgressBar"))
                          #'string-lessp)))
  ;; Check the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging"
               "min-horizontal-bar-height" "min-horizontal-bar-width"
               "min-vertical-bar-height" "min-vertical-bar-width" "xspacing" "yspacing")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkProgressBar"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkProgressBar" GTK-PROGRESS-BAR
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_progress_bar_get_type")
                       ((ELLIPSIZE GTK-PROGRESS-BAR-ELLIPSIZE "ellipsize"
                         "PangoEllipsizeMode" T T)
                        (FRACTION GTK-PROGRESS-BAR-FRACTION "fraction"
                         "gdouble" T T)
                        (INVERTED GTK-PROGRESS-BAR-INVERTED "inverted"
                         "gboolean" T T)
                        (PULSE-STEP GTK-PROGRESS-BAR-PULSE-STEP "pulse-step"
                         "gdouble" T T)
                        (SHOW-TEXT GTK-PROGRESS-BAR-SHOW-TEXT "show-text"
                         "gboolean" T T)
                        (TEXT GTK-PROGRESS-BAR-TEXT "text" "gchararray" T T)))
             (get-g-type-definition "GtkProgressBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-progress-bar-propertiers
  (let ((progress-bar (make-instance 'gtk-progress-bar)))
    ;; ellipsize
    (is (eq :none (gtk-progress-bar-ellipsize progress-bar)))
    (is (eq :start (setf (gtk-progress-bar-ellipsize progress-bar) :start)))
    (is (eq :start (gtk-progress-bar-ellipsize progress-bar)))
    ;; fraction
    (is (= 0.0d0 (gtk-progress-bar-fraction progress-bar)))
    (is (= 0.5d0 (setf (gtk-progress-bar-fraction progress-bar) 0.5)))
    (is (= 0.5d0 (gtk-progress-bar-fraction progress-bar)))
    ;; inverted
    (is-false (gtk-progress-bar-inverted progress-bar))
    (is-true (setf (gtk-progress-bar-inverted progress-bar) t))
    (is-true (gtk-progress-bar-inverted progress-bar))
    ;; pulse-step
    (is (= 0.1d0 (gtk-progress-bar-pulse-step progress-bar)))
    (is (= 0.3d0 (setf (gtk-progress-bar-pulse-step progress-bar) 0.3d0)))
    (is (= 0.3d0 (gtk-progress-bar-pulse-step progress-bar)))
    ;; show-text
    (is-false (gtk-progress-bar-show-text progress-bar))
    (is-true (setf (gtk-progress-bar-show-text progress-bar) t))
    (is-true (gtk-progress-bar-show-text progress-bar))
    ;; text
    (is-false (gtk-progress-bar-text progress-bar))
    (is (string= "text" (setf (gtk-progress-bar-text progress-bar) "text")))
    (is (string= "text" (gtk-progress-bar-text progress-bar)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-progress-bar-style-properties
  (let ((progress-bar (make-instance 'gtk-progress-bar)))
    (is (=   6 (gtk-widget-style-property progress-bar "min-horizontal-bar-height")))
    (is (= 150 (gtk-widget-style-property progress-bar "min-horizontal-bar-width")))
    (is (=  80 (gtk-widget-style-property progress-bar "min-vertical-bar-height")))
    (is (=   7 (gtk-widget-style-property progress-bar "min-vertical-bar-width")))
    (is (=   2 (gtk-widget-style-property progress-bar "xspacing")))
    (is (=   2 (gtk-widget-style-property progress-bar "yspacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_progress_bar_new

(test gtk-progress-bar-new
  (is (eq 'gtk-progress-bar (type-of (gtk-progress-bar-new)))))

;;;     gtk_progress_bar_pulse

