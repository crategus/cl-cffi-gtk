(def-suite gtk-radio-button :in gtk-suite)
(in-suite gtk-radio-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioButton

(test gtk-radio-button-class
  ;; Type check
  (is-true  (g-type-is-object "GtkRadioButton"))
  ;; Check the registered name
  (is (eq 'gtk-radio-button
          (registered-object-type-by-name "GtkRadioButton")))
  ;; Check the type initializer
  (is (string= "GtkRadioButton"
               (g-type-name (gtype (foreign-funcall "gtk_radio_button_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkCheckButton") (g-type-parent "GtkRadioButton")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkRadioButton"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable" "GtkActivatable")
             (mapcar #'gtype-name (g-type-interfaces "GtkRadioButton"))))
  ;; Check the class properties
  (is (equal '("action-name" "action-target" "active" "always-show-image" "app-paintable"
               "border-width" "can-default" "can-focus" "child" "composite-child"
               "double-buffered" "draw-indicator" "events" "expand" "focus-on-click" "group"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "image" "image-position" "inconsistent" "is-focus" "label"
               "margin" "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "name" "no-show-all" "opacity" "parent"
               "receives-default" "related-action" "relief" "resize-mode" "scale-factor"
               "sensitive" "style" "tooltip-markup" "tooltip-text" "use-action-appearance"
               "use-stock" "use-underline" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window" "xalign" "yalign")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkRadioButton"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging"
               "child-displacement-x" "child-displacement-y" "default-border"
               "default-outside-border" "displace-focus" "image-spacing" "inner-border"
               "indicator-size" "indicator-spacing")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkRadioButton"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkRadioButton"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkRadioButton" GTK-RADIO-BUTTON
                       (:SUPERCLASS GTK-CHECK-BUTTON :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_button_get_type")
                       ((GROUP GTK-RADIO-BUTTON-GROUP "group" "GtkRadioButton"
                         NIL T)))
             (get-g-type-definition "GtkRadioButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-radio-button-properties
  (let ((button (make-instance 'gtk-radio-button)))
    ;; group is not readable
    (signals (error) (gtk-radio-button-group button))
    ;; group is writable
    (is-false (setf (gtk-radio-button-group button) nil))))

;;; --- Signals ----------------------------------------------------------------

;;;     void    group-changed    Run First

#+nil
(test gtk-radio-button-signals
  (let ((result nil)
        (newbutton nil)
        (button (gtk-radio-button-new nil)))
    (g-signal-connect button "group-changed"
                      (lambda (button)
                        (setf result (cons "group-changed" result))
                        (is (eq 'gtk-radio-button (type-of button)))))
    (setf (gtk-radio-button-group button) nil)
    (setf newbutton (gtk-radio-button-new (gtk-radio-button-get-group button)))
    ;; Check if we called the signal handler two times
    (is (equal '("group-changed" "group-changed") result))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_button_new

(test gtk-radio-button-new
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new nil)))))
  ;; Second radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new (gtk-radio-button-get-group button))))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-button-get-group button))))
  (is (equal button (first (gtk-radio-button-get-group button))))
  ;; No bin child
  (is-false (gtk-bin-child button))))

;;;     gtk_radio_button_new_from_widget

(test gtk-radio-button-new-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new-from-widget nil)))))
  ;; Second radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new-from-widget button)))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-button-get-group button))))
  (is (equal button (first (gtk-radio-button-get-group button))))
  ;; No bin child
  (is-false (gtk-bin-child button))))

;;;     gtk_radio_button_new_with_label

(test gtk-radio-button-new-with-label
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new-with-label nil "First Button")))))
  ;; Second radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button
                         (gtk-radio-button-new-with-label (gtk-radio-button-get-group button)
                                                          "Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-button-get-group button))))
  (is (equal button (first (gtk-radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk-label (type-of (gtk-bin-child button))))
  (is (string= "Second Button" (gtk-label-label (gtk-bin-child button))))))

;;;     gtk_radio_button_new_with_label_from_widget

(test gtk-radio-button-new-with-label-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new-with-label-from-widget nil "First Button")))))
  ;; Second radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button
                         (gtk-radio-button-new-with-label-from-widget button "Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-button-get-group button))))
  (is (equal button (first (gtk-radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk-label (type-of (gtk-bin-child button))))
  (is (string= "Second Button" (gtk-label-label (gtk-bin-child button))))))

;;;     gtk_radio_button_new_with_mnemonic

(test gtk-radio-button-new-with-mnemonic
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new-with-mnemonic nil "_First Button")))))
  ;; Second radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button
                         (gtk-radio-button-new-with-mnemonic (gtk-radio-button-get-group button)
                                                             "_Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-button-get-group button))))
  (is (equal button (first (gtk-radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk-label (type-of (gtk-bin-child button))))
  (is (string= "_Second Button" (gtk-label-label (gtk-bin-child button))))))

;;;     gtk_radio_button_new_with_mnemonic_from_widget

(test gtk-radio-button-new-with-mnemonic-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button (gtk-radio-button-new-with-mnemonic-from-widget nil "_First Button")))))
  ;; Second radio button
  (is (eq 'gtk-radio-button
          (type-of (setf button
                         (gtk-radio-button-new-with-mnemonic-from-widget button
                                                                         "_Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-button-get-group button))))
  (is (equal button (first (gtk-radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk-label (type-of (gtk-bin-child button))))
  (is (string= "_Second Button" (gtk-label-label (gtk-bin-child button))))))

;;;     gtk_radio_button_set_group
;;;     gtk_radio_button_get_group

(test gtk-radio-button-group
  (let (button)
    ;; First radio button
    (is (eq 'gtk-radio-button (type-of (setf button (gtk-radio-button-new nil)))))
    (is (listp (gtk-radio-button-get-group button)))
    (is (= 1 (length (gtk-radio-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-button-get-group button)))))
    (is (equal button (first (gtk-radio-button-get-group button))))
    ;; Second radio button
    (is (eq 'gtk-radio-button
            (type-of (setf button (gtk-radio-button-new (gtk-radio-button-get-group button))))))
    (is (listp (gtk-radio-button-get-group button)))
    (is (= 2 (length (gtk-radio-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-button-get-group button)))))
    (is (equal button (first (gtk-radio-button-get-group button))))
    ;; Third radio button
    (is (eq 'gtk-radio-button
            (type-of (setf button (gtk-radio-button-new (gtk-radio-button-get-group button))))))
    (is (listp (gtk-radio-button-get-group button)))
    (is (= 3 (length (gtk-radio-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-button-get-group button)))))
    (is (equal button (first (gtk-radio-button-get-group button))))))

;;;     gtk_radio_button_join_group

(test gtk-radio-button-join-group.1
  (let (button lastbutton)
    ;; Add three buttons to a group
    (dotimes (i 3)
      (is (eq 'gtk-radio-button (type-of (setf button (gtk-radio-button-new nil)))))
      (is-false (gtk-radio-button-join-group button lastbutton))
      (is (eq 'gtk-radio-button (type-of (setf lastbutton button)))))
    ;; Check radio button group
    (is (listp (gtk-radio-button-get-group button)))
    (is (= 3 (length (gtk-radio-button-get-group button))))
    (is (= 3 (length (gtk-radio-button-get-group lastbutton))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-button-get-group button)))))
    ;; Remove the secion radio button from group
    (is-false (gtk-radio-button-join-group (second (gtk-radio-button-get-group button)) nil))
    (is (= 2 (length (gtk-radio-button-get-group button))))))

(test gtk-radio-button-join-group.2
  (let (button lastbutton)
    ;; Add three buttons to a group
    (dolist (label '("First Button" "Second Button" "Third Button"))
      (is (eq 'gtk-radio-button (type-of (setf button (gtk-radio-button-new-with-label nil label)))))
      (is-false (gtk-radio-button-join-group button lastbutton))
      (is (eq 'gtk-radio-button (type-of (setf lastbutton button)))))
    ;; Check radio button group
    (is (listp (gtk-radio-button-get-group button)))
    (is (= 3 (length (gtk-radio-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-button-get-group button)))))
    ;; Check the bin child
    (is (string= "Third Button"
                 (gtk-label-label (gtk-bin-child (first (gtk-radio-button-get-group button))))))
    (is (string= "Second Button"
                 (gtk-label-label (gtk-bin-child (second (gtk-radio-button-get-group button))))))
    (is (string= "First Button"
                 (gtk-label-label (gtk-bin-child (third (gtk-radio-button-get-group button))))))
    ;; Remove the secion radio button from group
    (is-false (gtk-radio-button-join-group (second (gtk-radio-button-get-group button)) nil))
    (is (= 2 (length (gtk-radio-button-get-group button))))
    (is (string= "Third Button"
                 (gtk-label-label (gtk-bin-child (first (gtk-radio-button-get-group button))))))
    (is (string= "First Button"
                 (gtk-label-label (gtk-bin-child (second (gtk-radio-button-get-group button))))))))

