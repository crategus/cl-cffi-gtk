(def-suite gtk-button-box :in gtk-suite)
(in-suite gtk-button-box)

;;; Types and Values

;;;     GtkButtonBoxStyle

(test gtk-button-box-style
  ;; Check the type
  (is (g-type-is-enum "GtkButtonBoxStyle"))
  ;; Check the type initializer
  (is (eq (gtype "GtkButtonBoxStyle")
          (gtype (foreign-funcall "gtk_button_box_style_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-button-box-style
          (registered-enum-type "GtkButtonBoxStyle")))
  ;; Check the names
  (is (equal '("GTK_BUTTONBOX_SPREAD" "GTK_BUTTONBOX_EDGE" "GTK_BUTTONBOX_START"
               "GTK_BUTTONBOX_END" "GTK_BUTTONBOX_CENTER" "GTK_BUTTONBOX_EXPAND")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkButtonBoxStyle"))))
  ;; Check the values
  (is (equal '(1 2 3 4 5 6)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkButtonBoxStyle"))))
  ;; Check the nick names
  (is (equal '("spread" "edge" "start" "end" "center" "expand")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkButtonBoxStyle"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkButtonBoxStyle"
                             GTK-BUTTON-BOX-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_button_box_style_get_type")
                             (:SPREAD 1)
                             (:EDGE 2)
                             (:START 3)
                             (:END 4)
                             (:CENTER 5)
                             (:EXPAND 6))
             (get-g-type-definition "GtkButtonBoxStyle"))))

;;;     GtkButtonBox

(foreign-funcall "gtk_hbutton_box_get_type" g-size)
(foreign-funcall "gtk_vbutton_box_get_type" g-size)

(test gtk-button-box-class
  ;; Type check
  (is (g-type-is-object "GtkButtonBox"))
  ;; Check the registered name
  (is (eq 'gtk-button-box
          (registered-object-type-by-name "GtkButtonBox")))
  ;; Check the type initializer
  (is (eq (gtype "GtkButtonBox")
          (gtype (foreign-funcall "gtk_button_box_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBox") (g-type-parent "GtkButtonBox")))
  ;; Check the children
  (is (equal '("GtkHButtonBox" "GtkVButtonBox")
             (mapcar #'g-type-name (g-type-children "GtkButtonBox"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'g-type-name (g-type-interfaces "GtkButtonBox"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "baseline-position" "border-width" "can-default"
               "can-focus" "child" "composite-child" "double-buffered" "events"
               "expand" "focus-on-click" "halign" "has-default" "has-focus"
               "has-tooltip" "height-request" "hexpand" "hexpand-set"
               "homogeneous" "is-focus" "layout-style" "margin" "margin-bottom"
               "margin-end" "margin-left" "margin-right" "margin-start"
               "margin-top" "name" "no-show-all" "opacity" "orientation"
               "parent" "receives-default" "resize-mode" "scale-factor"
               "sensitive" "spacing" "style" "tooltip-markup" "tooltip-text"
               "valign" "vexpand" "vexpand-set" "visible" "width-request"
               "window")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GtkButtonBox"))
                   #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging" "child-internal-pad-x"
               "child-internal-pad-y" "child-min-height" "child-min-width")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkButtonBox"))))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "padding" "pack-type" "position" "secondary"
               "non-homogeneous")
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkButtonBox"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkButtonBox" GTK-BUTTON-BOX
                       (:SUPERCLASS GTK-BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_button_box_get_type")
                       ((LAYOUT-STYLE GTK-BUTTON-BOX-LAYOUT-STYLE
                         "layout-style" "GtkButtonBoxStyle" T T)))
             (get-g-type-definition "GtkButtonBox"))))

;;; Functions

;;;     gtk_button_box_new
;;;     gtk_button_box_get_layout
;;;     gtk_button_box_get_child_secondary
;;;     gtk_button_box_get_child_non_homogeneous
;;;     gtk_button_box_set_layout
;;;     gtk_button_box_set_child_secondary
;;;     gtk_button_box_set_child_non_homogeneous

;;; Properties

;;;     GtkButtonBoxStyle  layout-style          Read / Write

;;; Child Properties

;;;              gboolean  non-homogeneous       Read / Write
;;;              gboolean  secondary             Read / Write

;;; Style Properties

;;;                  gint  child-internal-pad-x  Read
;;;                  gint  child-internal-pad-y  Read
;;;                  gint  child-min-height      Read
;;;                  gint  child-min-width       Read

;;; 2021-8-2
