(def-suite gtk-info-bar :in gtk-suite)
(in-suite gtk-info-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInfoBar

(test gtk-info-bar-class
  ;; Type check
  (is-true  (g-type-is-object "GtkInfoBar"))
  ;; Check the registered name
  (is (eq 'gtk-info-bar
          (registered-object-type-by-name "GtkInfoBar")))
  ;; Check the type initializer
  (is (string= "GtkInfoBar"
               (g-type-name (gtype (foreign-funcall "gtk_info_bar_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkBox") (g-type-parent "GtkInfoBar")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkInfoBar"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'gtype-name (g-type-interfaces "GtkInfoBar"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "baseline-position" "border-width" "can-default" "can-focus"
               "child" "composite-child" "double-buffered" "events" "expand" "focus-on-click"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "homogeneous" "is-focus" "margin" "margin-bottom" "margin-end"
               "margin-left" "margin-right" "margin-start" "margin-top" "message-type" "name"
               "no-show-all" "opacity" "orientation" "parent" "receives-default"
               "resize-mode" "revealed" "scale-factor" "sensitive" "show-close-button"
               "spacing" "style" "tooltip-markup" "tooltip-text" "valign" "vexpand"
               "vexpand-set" "visible" "width-request" "window")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkInfoBar"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "action-area-border"
               "button-spacing" "content-area-border" "content-area-spacing")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkInfoBar"))))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "padding" "pack-type" "position")
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkInfoBar"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkInfoBar" GTK-INFO-BAR
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_info_bar_get_type")
                       ((MESSAGE-TYPE GTK-INFO-BAR-MESSAGE-TYPE "message-type"
                         "GtkMessageType" T T)
                        (REVEALED GTK-INFO-BAR-REVEALED "revealed" "gboolean" T
                         T)
                        (SHOW-CLOSE-BUTTON GTK-INFO-BAR-SHOW-CLOSE-BUTTON
                         "show-close-button" "gboolean" T T)))
             (get-g-type-definition "GtkInfoBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-info-bar-properties
  (let ((info-bar (make-instance 'gtk-info-bar)))
    ;; message-type
    (is (eq :info (gtk-info-bar-message-type info-bar)))
    (is (eq :error (setf (gtk-info-bar-message-type info-bar) :error)))
    (is (eq :error (gtk-info-bar-message-type info-bar)))
    ;; revealed
    (is-true (gtk-info-bar-revealed info-bar))
    (is-false (setf (gtk-info-bar-revealed info-bar) nil))
    (is-false (gtk-info-bar-revealed info-bar))
    ;; show-close-button
    (is-false (gtk-info-bar-show-close-button info-bar))
    (is-true (setf (gtk-info-bar-show-close-button info-bar) t))
    (is-true (gtk-info-bar-show-close-button info-bar))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-info-bar-style-properties
  (let ((info-bar (make-instance 'gtk-info-bar)))
    (is (=  5 (gtk-widget-style-property info-bar "action-area-border")))
    (is (=  6 (gtk-widget-style-property info-bar "button-spacing")))
    (is (=  8 (gtk-widget-style-property info-bar "content-area-border")))
    (is (= 16 (gtk-widget-style-property info-bar "content-area-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_info_bar_new

(test gtk-info-bar-new
  (is (eq 'gtk-info-bar (type-of (gtk-info-bar-new)))))

;;;     gtk_info_bar_new_with_buttons

(test gtk-info-bar-new-with-buttons
  (let ((info-bar nil))
    (is (eq 'gtk-info-bar
            (type-of (setf info-bar
                           (gtk-info-bar-new-with-buttons "gtk-ok" 1)))))
    (is (= 1 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is (eq 'gtk-info-bar
            (type-of (setf info-bar
                           (gtk-info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2)))))
    (is (= 2 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_add_action_widget

(test gtk-info-bar-add-action-widget
  (let ((info-bar (make-instance 'gtk-info-bar)))
    (is (= 0 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is-false (gtk-info-bar-add-action-widget info-bar (make-instance 'gtk-button) 1))
    (is (= 1 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is-false (gtk-info-bar-add-action-widget info-bar (make-instance 'gtk-button) 2))
    (is (= 2 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_add_button

(test gtk-info-bar-add-button
  (let ((info-bar (make-instance 'gtk-info-bar)))
    (is (= 0 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is (eq 'gtk-button (type-of (gtk-info-bar-add-button info-bar "gtk-ok" 1))))
    (is (= 1 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is (eq 'gtk-button (type-of (gtk-info-bar-add-button info-bar "gtk-cancel" 2))))
    (is (= 2 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_add_buttons

(test gtk-info-bar-add-buttons
  (let ((info-bar (make-instance 'gtk-info-bar)))
    (is (= 0 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is-false (gtk-info-bar-add-buttons info-bar "gtk-ok" 1))
    (is (= 1 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))
    (is-false (gtk-info-bar-add-buttons info-bar "gtk-cancel" 2 "gtk-no" 3))
    (is (= 3 (length (gtk-container-children (gtk-info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_set_response_sensitive

(test gtk-info-bar-set-response-sensitive
  (let ((info-bar (gtk-info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2 "gtk-no" 3)))
    (is-false (gtk-info-bar-set-response-sensitive info-bar 1 nil))))

;;;     gtk_info_bar_set_default_response

(test gtk-info-bar-set-default-response
  (let ((window (make-instance 'gtk-window))
        (info-bar (gtk-info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2 "gtk-no" 3)))
    ;; The info bar must be within a GtkWindow
    (is-false (gtk-container-add window info-bar))
    (is-false (gtk-info-bar-set-default-response info-bar 1))))

;;;     gtk_info_bar_response

;;;     gtk_info_bar_get_action_area

(test gtk-info-bar-action-area
  (let ((info-bar (make-instance 'gtk-info-bar)))
    (is (eq 'gtk-button-box (type-of (gtk-info-bar-action-area info-bar))))))

;;;     gtk_info_bar_get_content_area

(test gtk-info-bar-content-area
  (let ((info-bar (make-instance 'gtk-info-bar)))
    (is (eq 'gtk-box (type-of (gtk-info-bar-content-area info-bar))))))

