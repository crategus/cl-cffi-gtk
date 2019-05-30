(def-suite gtk-box :in gtk-suite)
(in-suite gtk-box)

;;; --- GtkBox -----------------------------------------------------------------

(test gtk-box-class
  ;; Type check
  (is-true  (g-type-is-object "GtkBox"))
  ;; Check the registered name
  (is (eq 'gtk-box
          (registered-object-type-by-name "GtkBox")))
  ;; Check the parent
  (is (equal (gtype "GtkContainer") (g-type-parent "GtkBox")))
  ;; Check the children
  (is (equal '("GtkHBox" "GtkVBox" "GtkStackSwitcher" "GtkButtonBox" "GtkStatusbar"
 "GtkInfoBar" "GtkColorChooserWidget" "GtkColorSelection"
 "GtkFileChooserWidget" "GtkFileChooserButton" "GtkFontChooserWidget"
 "GtkFontSelection" "GtkRecentChooserWidget" "GtkAppChooserWidget"
 "GtkShortcutsSection" "GtkShortcutsGroup" "GtkShortcutsShortcut")
             (mapcar #'gtype-name (g-type-children "GtkBox"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'gtype-name (g-type-interfaces "GtkBox"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "baseline-position" "border-width" "can-default" "can-focus"
 "child" "composite-child" "double-buffered" "events" "expand" "focus-on-click"
 "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
 "hexpand-set" "homogeneous" "is-focus" "margin" "margin-bottom" "margin-end"
 "margin-left" "margin-right" "margin-start" "margin-top" "name" "no-show-all"
 "opacity" "orientation" "parent" "receives-default" "resize-mode"
 "scale-factor" "sensitive" "spacing" "style" "tooltip-markup" "tooltip-text"
 "valign" "vexpand" "vexpand-set" "visible" "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkBox"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
 "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
 "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
 "separator-width" "text-handle-height" "text-handle-width"
 "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkBox"))))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "padding" "pack-type" "position")
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkBox"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkBox" GTK-BOX
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_box_get_type")
                       ((BASELINE-POSITION GTK-BOX-BASELINE-POSITION
                         "baseline-position" "GtkBaselinePosition" T T)
                        (HOMOGENEOUS GTK-BOX-HOMOGENEOUS "homogeneous"
                         "gboolean" T T)
                        (SPACING GTK-BOX-SPACING "spacing" "gint" T T)))
             (get-g-type-definition "GtkBox"))))

;;; --- gtk-box-properties -----------------------------------------------------

(test gtk-box-properties
  (let ((box (make-instance 'gtk-box :orientation :vertical :spacing 12)))
    (is (eq 'gtk-box (type-of box)))
    (is (eq :vertical (gtk-orientable-orientation box)))
    (is (eq :center (gtk-box-baseline-position box)))
    (is-false (gtk-box-homogeneous box))
    (is (= 12 (gtk-box-spacing box)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-box-child-properties
  (let* ((box (make-instance 'gtk-box :orientation :vertical))
         (button (make-instance 'gtk-button)))
    (gtk-container-add box button)
    (is-false (gtk-box-child-expand box button))
    (is-true (gtk-box-child-fill box button))
    (is (eq :start (gtk-box-child-pack-type box button)))
    (is (= 0 (gtk-box-child-padding box button)))
    (is (= 0 (gtk-box-child-position box button)))))

;;; --- gtk-box-new ------------------------------------------------------------

(test gtk-box-new
  (let ((box (gtk-box-new :vertical 12)))
    (is (eq :vertical (gtk-orientable-orientation box)))
    (is (eq :center (gtk-box-baseline-position box)))
    (is-false (gtk-box-homogeneous box))
    (is (= 12 (gtk-box-spacing box))))
  ;; Use make-instance with default values
  (let ((box (make-instance 'gtk-box)))
    (is (eq :horizontal (gtk-orientable-orientation box)))
    (is (eq :center (gtk-box-baseline-position box)))
    (is-false (gtk-box-homogeneous box))
    (is (= 0 (gtk-box-spacing box))))
  ;; Use make-instance and set some properties
  (let ((box (make-instance 'gtk-box
                            :orientation :vertical
                            :baseline-position :top
                            :homogeneous t
                            :spacing 12)))
    (is (eq :vertical (gtk-orientable-orientation box)))
    (is (eq :top (gtk-box-baseline-position box)))
    (is-true (gtk-box-homogeneous box))
    (is (= 12 (gtk-box-spacing box)))))

;;; --- gtk-box-pack-start -----------------------------------------------------

(test gtk-box-pack-start
  (let* ((box (make-instance 'gtk-box :orientation :vertical))
         (button1 (make-instance 'gtk-button))
         (button2 (make-instance 'gtk-button))
         (button3 (make-instance 'gtk-button)))
    (gtk-box-pack-start box button1)
    (is (= 0 (gtk-box-child-position box button1)))
    (gtk-box-pack-start box button2)
    (is (= 0 (gtk-box-child-position box button1)))
    (is (= 1 (gtk-box-child-position box button2)))
    (gtk-box-pack-start box button3)
    (is (= 0 (gtk-box-child-position box button1)))
    (is (= 1 (gtk-box-child-position box button2)))
    (is (= 2 (gtk-box-child-position box button3)))))

;;; --- gtk-box-pack-end -------------------------------------------------------

(test gtk-box-pack-end
  (let* ((box (make-instance 'gtk-box :orientation :vertical))
         (button1 (make-instance 'gtk-button))
         (button2 (make-instance 'gtk-button))
         (button3 (make-instance 'gtk-button)))
    (gtk-box-pack-end box button1)
    (is (= 0 (gtk-box-child-position box button1)))
    (is (eq :end (gtk-box-child-pack-type box button1)))
    (gtk-box-pack-end box button2)
    (is (= 0 (gtk-box-child-position box button1)))
    (is (eq :end (gtk-box-child-pack-type box button1)))
    (is (= 1 (gtk-box-child-position box button2)))
    (is (eq :end (gtk-box-child-pack-type box button2)))
    (gtk-box-pack-end box button3)
    (is (= 0 (gtk-box-child-position box button1)))
    (is (eq :end (gtk-box-child-pack-type box button1)))
    (is (= 1 (gtk-box-child-position box button2)))
    (is (eq :end (gtk-box-child-pack-type box button2)))
    (is (= 2 (gtk-box-child-position box button3)))
    (is (eq :end (gtk-box-child-pack-type box button3)))))

;;;     gtk_box_reorder_child

;;; --- gtk-box-query-child-packing --------------------------------------------

(test gtk-box-query-child-packing
  (let ((box (make-instance 'gtk-box))
        (button (make-instance 'gtk-button)))
    (gtk-container-add box button)
    (multiple-value-bind (expand fill padding pack-type)
        (gtk-box-query-child-packing box button)
      (is-false expand)
      (is-true fill)
      (is (= 0 padding))
      (is (eq :start pack-type)))))

;;;     gtk_box_set_child_packing
;;;     gtk_box_get_center_widget
;;;     gtk_box_set_center_widget

