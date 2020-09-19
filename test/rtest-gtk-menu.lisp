(def-suite gtk-menu :in gtk-suite)
(in-suite gtk-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkArrowPlacement

(test gtk-arrow-placement
  ;; Check the type
  (is-true (g-type-is-enum "GtkArrowPlacement"))
  ;; Check the type initializer
  (is (string= "GtkArrowPlacement"
               (g-type-name (gtype (foreign-funcall "gtk_arrow_placement_get_type" :int)))))
  ;; Check the registered name
  (is (eq 'gtk-arrow-placement (gobject::registered-enum-type "GtkArrowPlacement")))
  ;; Check the names
  (is (equal '("GTK_ARROWS_BOTH" "GTK_ARROWS_START" "GTK_ARROWS_END")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkArrowPlacement"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkArrowPlacement"))))
  ;; Check the nick names
  (is (equal '("both" "start" "end")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkArrowPlacement"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkArrowPlacement"
                             GTK-ARROW-PLACEMENT
                             (:EXPORT T :TYPE-INITIALIZER "gtk_arrow_placement_get_type")
                             (:BOTH 0)
                             (:START 1)
                             (:END 2))
             (gobject::get-g-type-definition "GtkArrowPlacement"))))

;;;     GtkMenu

(test gtk-menu-class
  ;; Type check
  (is-true  (g-type-is-object "GtkMenu"))
  ;; Check the registered name
  (is (eq 'gtk-menu
          (registered-object-type-by-name "GtkMenu")))
  ;; Check the type initializer
  (is (string= "GtkMenu"
               (g-type-name (gtype (foreign-funcall "gtk_menu_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkMenuShell") (g-type-parent "GtkMenu")))
  ;; Check the children
  (is (equal '("GtkRecentChooserMenu")
             (mapcar #'gtype-name (g-type-children "GtkMenu"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkMenu"))))
  ;; Check the class properties
  (is (equal '("accel-group" "accel-path" "active" "anchor-hints" "app-paintable"
               "attach-widget" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand" "focus-on-click"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "is-focus" "margin" "margin-bottom" "margin-end" "margin-left"
               "margin-right" "margin-start" "margin-top" "menu-type-hint" "monitor" "name"
               "no-show-all" "opacity" "parent" "receives-default" "rect-anchor-dx"
               "rect-anchor-dy" "reserve-toggle-size" "resize-mode" "scale-factor"
               "sensitive" "style" "take-focus" "tearoff-state" "tearoff-title"
               "tooltip-markup" "tooltip-text" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkMenu"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "arrow-placement"
               "arrow-scaling" "double-arrows" "horizontal-offset" "horizontal-padding"
               "vertical-offset" "vertical-padding")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkMenu"))))
  ;; Get the names of the child properties
  (is (equal '("left-attach" "right-attach" "top-attach" "bottom-attach")
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkMenu"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMenu" GTK-MENU
                       (:SUPERCLASS GTK-MENU-SHELL :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_menu_get_type")
                       ((ACCEL-GROUP GTK-MENU-ACCEL-GROUP "accel-group"
                         "GtkAccelGroup" T T)
                        (ACCEL-PATH GTK-MENU-ACCEL-PATH "accel-path"
                         "gchararray" T T)
                        (ACTIVE GTK-MENU-ACTIVE "active" "gint" T T)
                        (ANCHOR-HINTS GTK-MENU-ANCHOR-HINTS "anchor-hints"
                         "GdkAnchorHints" T T)
                        (ATTACH-WIDGET GTK-MENU-ATTACH-WIDGET "attach-widget"
                         "GtkWidget" T T)
                        (MENU-TYPE-HINT GTK-MENU-MENU-TYPE-HINT
                         "menu-type-hint" "GdkWindowTypeHint" T T)
                        (MONITOR GTK-MENU-MONITOR "monitor" "gint" T T)
                        (RECT-ANCHOR-DX GTK-MENU-RECT-ANCHOR-DX
                         "rect-anchor-dx" "gint" T T)
                        (RECT-ANCHOR-DY GTK-MENU-RECT-ANCHOR-DY
                         "rect-anchor-dy" "gint" T T)
                        (RESERVE-TOGGLE-SIZE GTK-MENU-RESERVE-TOGGLE-SIZE
                         "reserve-toggle-size" "gboolean" T T)
                        (TEAROFF-STATE GTK-MENU-TEAROFF-STATE "tearoff-state"
                         "gboolean" T T)
                        (TEAROFF-TITLE GTK-MENU-TEAROFF-TITLE "tearoff-title"
                         "gchararray" T T)))
             (get-g-type-definition "GtkMenu"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-menu-properties
  (let ((menu (make-instance 'gtk-menu)))
    (is-false (gtk-menu-accel-group menu))
    (is-false (gtk-menu-accel-path menu))
    (is (= -1 (gtk-menu-active menu)))
    (is (equal '(:FLIP-X :FLIP-Y :SLIDE-X :SLIDE-Y :RESIZE-X :RESIZE-Y)
               (gtk-menu-anchor-hints menu)))
    (is-false (gtk-menu-attach-widget menu))
    (is (eq :POPUP-MENU (gtk-menu-menu-type-hint menu)))
    (is (= -1 (gtk-menu-monitor menu)))
    (is (= 0 (gtk-menu-rect-anchor-dx menu)))
    (is (= 0 (gtk-menu-rect-anchor-dy menu)))
    (is-true (gtk-menu-reserve-toggle-size menu))
    (is-false (gtk-menu-tearoff-state menu))
    (is-false (gtk-menu-tearoff-title menu))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-menu-child-properties
  (let ((menu (make-instance 'gtk-menu))
        (child (make-instance 'gtk-menu-item)))
    (is-false (gtk-container-add menu child))
    (is (= -1 (gtk-menu-child-bottom-attach menu child)))
    (is (= -1 (gtk-menu-child-left-attach menu child)))
    (is (= -1 (gtk-menu-child-right-attach menu child)))
    (is (= -1 (gtk-menu-child-top-attach menu child)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-menu-style-properties
  (let ((menu (make-instance 'gtk-menu)))
    (is (eq :both (gtk-widget-style-property menu "arrow-placement")))
    (is (= 0.7 (gtk-widget-style-property menu "arrow-scaling")))
    (is-true (gtk-widget-style-property menu "double-arrows"))
    (is-true (gtk-widget-style-property menu "horizontal-offset"))
    (is (= 0 (gtk-widget-style-property menu "horizontal-padding")))
    (is-true (gtk-widget-style-property menu "vertical-offset"))
    (is (= 1 (gtk-widget-style-property menu "vertical-padding")))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkMenuPositionFunc
;;;     GtkMenuDetachFunc

;;;     gtk_menu_new

(test gtk-menu-new
  (is (eq 'gtk-menu (type-of (gtk-menu-new)))))

;;;     gtk_menu_new_from_model

(test gtk-menu-new-from-model
  (is (eq 'gtk-menu
          (type-of (gtk-menu-new-from-model (make-instance 'g-menu))))))

;;;     gtk_menu_set_screen

(test gtk-menu-set-screen
  (let ((menu (make-instance 'gtk-menu)))
    (is-false (gtk-menu-set-screen menu nil))
    (is-false (gtk-menu-set-screen menu (gdk-display-default-screen (gdk-display-default))))))

;;;     gtk_menu_reorder_child

(test gtk-menu-reorder-child
  (let ((menu (make-instance 'gtk-menu))
        (item-open (gtk-menu-item-new-with-label "Open"))
        (item-save (gtk-menu-item-new-with-label "Save"))
        (item-quit (gtk-menu-item-new-with-label "Quit")))

    (is-false (gtk-container-add menu item-open))
    (is-false (gtk-container-add menu item-save))
    (is-false (gtk-container-add menu item-quit))

    (is (equal item-open (first (gtk-container-children menu))))
    (is (equal item-save (second (gtk-container-children menu))))
    (is (equal item-quit (third (gtk-container-children menu))))

    (is-false (gtk-menu-reorder-child menu item-quit 0))

    (is (equal item-quit (first (gtk-container-children menu))))
    (is (equal item-open (second (gtk-container-children menu))))
    (is (equal item-save (third (gtk-container-children menu))))))

;;;     gtk_menu_attach

(test gtk-menu-reorder-child
  (let ((menu (make-instance 'gtk-menu))
        (item-open (gtk-menu-item-new-with-label "Open"))
        (item-save (gtk-menu-item-new-with-label "Save"))
        (item-quit (gtk-menu-item-new-with-label "Quit")))

    (is-false (gtk-menu-attach menu item-open 0 2 0 1))
    (is-false (gtk-menu-attach menu item-save 1 3 1 2))
    (is-false (gtk-menu-attach menu item-quit 0 2 2 3))

    (is (= 0 (gtk-menu-child-left-attach menu item-open)))
    (is (= 2 (gtk-menu-child-right-attach menu item-open)))
    (is (= 0 (gtk-menu-child-top-attach menu item-open)))
    (is (= 1 (gtk-menu-child-bottom-attach menu item-open)))))

;;;     gtk_menu_popup_at_rect
;;;     gtk_menu_popup_at_widget
;;;     gtk_menu_popup_at_pointer

;;;     gtk_menu_popup_for_device
;;;     gtk_menu_popup

;;;     gtk_menu_place_on_monitor

;;;     gtk_menu_popdown
;;;     gtk_menu_reposition

;;;     gtk_menu_attach_to_widget
;;;     gtk_menu_detach

;;;     gtk_menu_get_for_attach_widget

