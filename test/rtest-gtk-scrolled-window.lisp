(def-suite gtk-scrolled-window :in gtk-suite)
(in-suite gtk-scrolled-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPolicyType

(test gtk-policy-type
  ;; Check the type
  (is-true (g-type-is-enum "GtkPolicyType"))
  ;; Check the type initializer
  (is (string= "GtkPolicyType"
               (g-type-name (gtype (foreign-funcall "gtk_policy_type_get_type" :int)))))
  ;; Check the registered name
  (is (eq 'gtk-policy-type (gobject::registered-enum-type "GtkPolicyType")))
  ;; Check the names
  (is (equal '("GTK_POLICY_ALWAYS" "GTK_POLICY_AUTOMATIC" "GTK_POLICY_NEVER"
               "GTK_POLICY_EXTERNAL")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkPolicyType"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkPolicyType"))))
  ;; Check the nick names
  (is (equal '("always" "automatic" "never" "external")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkPolicyType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkPolicyType"
                             GTK-POLICY-TYPE
                             (:EXPORT T :TYPE-INITIALIZER "gtk_policy_type_get_type")
                             (:ALWAYS 0)
                             (:AUTOMATIC 1)
                             (:NEVER 2)
                             (:EXTERNAL 3))
             (gobject::get-g-type-definition "GtkPolicyType"))))

;;;     GtkCornerType

(test gtk-corner-type
  ;; Check the type
  (is-true (g-type-is-enum "GtkCornerType"))
  ;; Check the type initializer
  (is (string= "GtkCornerType"
               (g-type-name (gtype (foreign-funcall "gtk_corner_type_get_type" :int)))))
  ;; Check the registered name
  (is (eq 'gtk-policy-type (gobject::registered-enum-type "GtkPolicyType")))
  ;; Check the names
  (is (equal '("GTK_CORNER_TOP_LEFT" "GTK_CORNER_BOTTOM_LEFT" "GTK_CORNER_TOP_RIGHT"
               "GTK_CORNER_BOTTOM_RIGHT")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkCornerType"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkCornerType"))))
  ;; Check the nick names
  (is (equal '("top-left" "bottom-left" "top-right" "bottom-right")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkCornerType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkCornerType"
                             GTK-CORNER-TYPE
                             (:EXPORT T :TYPE-INITIALIZER "gtk_corner_type_get_type")
                             (:TOP-LEFT 0)
                             (:BOTTOM-LEFT 1)
                             (:TOP-RIGHT 2)
                             (:BOTTOM-RIGHT 3))
             (gobject::get-g-type-definition "GtkCornerType"))))

;;;     GtkScrolledWindow

(test gtk-scrolled-window-class
  ;; Type check
  (is-true  (g-type-is-object "GtkScrolledWindow"))
  ;; Check the registered name
  (is (eq 'gtk-scrolled-window
          (registered-object-type-by-name "GtkScrolledWindow")))
  ;; Check the type initializer
  (is (string= "GtkScrolledWindow"
               (g-type-name (gtype (foreign-funcall "gtk_scrolled_window_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkBin") (g-type-parent "GtkScrolledWindow")))
  ;; Check the children
  (is (equal '("GtkPlacesSidebar")
             (mapcar #'gtype-name (g-type-children "GtkScrolledWindow"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkScrolledWindow"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand" "focus-on-click"
               "hadjustment" "halign" "has-default" "has-focus" "has-tooltip"
               "height-request" "hexpand" "hexpand-set" "hscrollbar-policy" "is-focus"
               "kinetic-scrolling" "margin" "margin-bottom" "margin-end" "margin-left"
               "margin-right" "margin-start" "margin-top" "max-content-height"
               "max-content-width" "min-content-height" "min-content-width" "name"
               "no-show-all" "opacity" "overlay-scrolling" "parent"
               "propagate-natural-height" "propagate-natural-width" "receives-default"
               "resize-mode" "scale-factor" "sensitive" "shadow-type" "style"
               "tooltip-markup" "tooltip-text" "vadjustment" "valign" "vexpand" "vexpand-set"
               "visible" "vscrollbar-policy" "width-request" "window" "window-placement"
               "window-placement-set")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkScrolledWindow"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "scrollbar-spacing"
               "scrollbars-within-bevel")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkScrolledWindow"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkScrolledWindow"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkScrolledWindow" GTK-SCROLLED-WINDOW
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_scrolled_window_get_type")
                       ((HADJUSTMENT GTK-SCROLLED-WINDOW-HADJUSTMENT
                         "hadjustment" "GtkAdjustment" T T)
                        (HSCROLLBAR-POLICY
                         GTK-SCROLLED-WINDOW-HSCROLLBAR-POLICY
                         "hscrollbar-policy" "GtkPolicyType" T T)
                        (KINETIC-SCROLLING
                         GTK-SCROLLED-WINDOW-KINETIC-SCROLLING
                         "kinetic-scrolling" "gboolean" T T)
                        (MAX-CONTENT-HEIGHT
                         GTK-SCROLLED-WINDOW-MAX-CONTENT-HEIGHT
                         "max-content-height" "gint" T T)
                        (MAX-CONTENT-WIDTH
                         GTK-SCROLLED-WINDOW-MAX-CONTENT-WIDTH
                         "max-content-width" "gint" T T)
                        (MIN-CONTENT-HEIGHT
                         GTK-SCROLLED-WINDOW-MIN-CONTENT-HEIGHT
                         "min-content-height" "gint" T T)
                        (MIN-CONTENT-WIDTH
                         GTK-SCROLLED-WINDOW-MIN-CONTENT-WIDTH
                         "min-content-width" "gint" T T)
                        (OVERLAY-SCROLLING
                         GTK-SCROLLED-WINDOW-OVERLAY-SCROLLING
                         "overlay-scrolling" "gboolean" T T)
                        (PROPAGATE-NATURAL-HEIGHT
                         GTK-SCROLLED-WINDOW-PROPAGATE-NATURAL-HEIGHT
                         "propagate-natural-height" "gboolean" T T)
                        (PROPAGATE-NATURAL-WIDTH
                         GTK-SCROLLED-WINDOW-PROPAGATE-NATURAL-WIDTH
                         "propagate-natural-width" "gboolean" T T)
                        (SHADOW-TYPE GTK-SCROLLED-WINDOW-SHADOW-TYPE
                         "shadow-type" "GtkShadowType" T T)
                        (VADJUSTMENT GTK-SCROLLED-WINDOW-VADJUSTMENT
                         "vadjustment" "GtkAdjustment" T T)
                        (VSCROLLBAR-POLICY
                         GTK-SCROLLED-WINDOW-VSCROLLBAR-POLICY
                         "vscrollbar-policy" "GtkPolicyType" T T)
                        (WINDOW-PLACEMENT GTK-SCROLLED-WINDOW-WINDOW-PLACEMENT
                         "window-placement" "GtkCornerType" T T)
                        (WINDOW-PLACEMENT-SET
                         GTK-SCROLLED-WINDOW-WINDOW-PLACEMENT-SET
                         "window-placement-set" "gboolean" T T)))
             (get-g-type-definition "GtkScrolledWindow"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scrolled-window-properties
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is (eq 'gtk-adjustment (type-of (gtk-scrolled-window-hadjustment window))))
    (is (eq :automatic (gtk-scrolled-window-hscrollbar-policy window)))
    (is-true (gtk-scrolled-window-kinetic-scrolling window))
    (is (= -1 (gtk-scrolled-window-max-content-height window)))
    (is (= -1 (gtk-scrolled-window-max-content-width window)))
    (is (= -1 (gtk-scrolled-window-min-content-height window)))
    (is (= -1 (gtk-scrolled-window-min-content-width window)))
    (is-true (gtk-scrolled-window-overlay-scrolling window))
    (is-false (gtk-scrolled-window-propagate-natural-height window))
    (is-false (gtk-scrolled-window-propagate-natural-width window))
    (is (eq :none (gtk-scrolled-window-shadow-type window)))
    (is (eq 'gtk-adjustment (type-of (gtk-scrolled-window-vadjustment window))))
    (is (eq :automatic (gtk-scrolled-window-vscrollbar-policy window)))
    (is (eq :top-left (gtk-scrolled-window-window-placement window)))
    (is-true (gtk-scrolled-window-window-placement-set window))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-scrolled-window-style-properties
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is (= 0 (gtk-widget-style-get-property window "scrollbar-spacing")))
    (is-false (gtk-widget-style-get-property window "scrollbar-within-bevel"))))

;;; --- Signals ----------------------------------------------------------------

;;;              void    edge-overshot               Run Last
;;;              void    edge-reached                Run Last
;;;              void    move-focus-out              Action
;;;          gboolean    scroll-child                Action

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrolled_window_new

(test gtk-scrolled-window-new
  (is (eq 'gtk-scrolled-window (type-of (gtk-scrolled-window-new))))
  (is (eq 'gtk-scrolled-window
          (type-of (gtk-scrolled-window-new (make-instance 'gtk-adjustment)))))
  (is (eq 'gtk-scrolled-window
          (type-of (gtk-scrolled-window-new nil
                                            (make-instance 'gtk-adjustment)))))
  (is (eq 'gtk-scrolled-window
          (type-of (gtk-scrolled-window-new (make-instance 'gtk-adjustment)
                                            (make-instance 'gtk-adjustment))))))

;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar

(test gtk-scrolled-window-scrollbar
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is (eq 'gtk-scrollbar (type-of (gtk-scrolled-window-hscrollbar window))))
    (is (eq 'gtk-scrollbar (type-of (gtk-scrolled-window-vscrollbar window))))))

;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_set_policy

(test gtk-scrolled-window-policy
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is (equal (list :automatic :automatic)
               (multiple-value-list (gtk-scrolled-window-policy window))))
    (is (equal (list :never :external)
               (multiple-value-list
                   (setf (gtk-scrolled-window-policy window) '(:never :external)))))
    (is (equal (list :never :external)
               (multiple-value-list (gtk-scrolled-window-policy window))))))

;;;     gtk_scrolled_window_add_with_viewport

(test gtk-scrolled-window-add-with-viewport
  (let ((window (make-instance 'gtk-scrolled-window))
        (button (make-instance 'gtk-button)))
    ;; Use the deprecated function gtk-scrolled-window-add-with-viewport
    (is-false (gtk-scrolled-window-add-with-viewport window button))
    (is (eq 'gtk-viewport (type-of (gtk-bin-child window))))
    (is (eq 'gtk-button (type-of (gtk-bin-child (gtk-bin-child window))))))
  (let ((window (make-instance 'gtk-scrolled-window))
        (button (make-instance 'gtk-button)))
    ;; Use the function gtk-container-add
    (is-false (gtk-container-add window button))
    (is (eq 'gtk-viewport (type-of (gtk-bin-child window))))
    (is (eq 'gtk-button (type-of (gtk-bin-child (gtk-bin-child window)))))))

;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_set_placement

(test gtk-scrolled-window-placement
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is (eq :top-left (gtk-scrolled-window-placement window)))
    (is (eq :top-right (setf (gtk-scrolled-window-placement window) :top-right)))
    (is (eq :top-right (gtk-scrolled-window-placement window)))))

;;;     gtk_scrolled_window_unset_placement

(test gtk-scrolled-window-unset-placement
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is (eq :top-right (setf (gtk-scrolled-window-placement window) :top-right)))
    (is (eq :top-right (gtk-scrolled-window-placement window)))
    (is-false (gtk-scrolled-window-unset-placement window))
    (is (eq :top-left (gtk-scrolled-window-placement window)))))

;;;     gtk_scrolled_window_get_capture_button_press
;;;     gtk_scrolled_window_set_capture_button_press

(test gtk-scrolled-window-capture-button-press
  (let ((window (make-instance 'gtk-scrolled-window)))
    (is-true (gtk-scrolled-window-capture-button-press window))
    (is-false (setf (gtk-scrolled-window-capture-button-press window) nil))
    (is-false (gtk-scrolled-window-capture-button-press window))))

