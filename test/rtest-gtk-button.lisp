(def-suite gtk-button :in gtk-suite)
(in-suite gtk-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButton

(test gtk-button-class
  ;; Type check
  (is (g-type-is-object "GtkButton"))
  ;; Check the registered name
  (is (eq 'gtk-button
          (registered-object-type-by-name "GtkButton")))
  ;; Check the type initializer
  (is (eq (gtype "GtkButton")
          (gtype (foreign-funcall "gtk_button_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBin") (g-type-parent "GtkButton")))
  ;; Check the children
  (is (equal '("GtkToggleButton" "GtkLinkButton" "GtkScaleButton"
               "GtkModelButton" "GtkColorButton" "GtkFontButton")
             (mapcar #'g-type-name (g-type-children "GtkButton"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (mapcar #'g-type-name (g-type-interfaces "GtkButton"))))
  ;; Check the class properties
  (is (equal '("action-name" "action-target" "always-show-image" "image"
               "image-position" "label" "related-action" "relief"  
               "use-action-appearance" "use-stock" "use-underline" "xalign"
               "yalign")
             (list-class-property-names "GtkButton")))
  ;; Get the names of the style properties
  (is (equal '("child-displacement-x" "child-displacement-y" "default-border"
               "default-outside-border" "displace-focus" "image-spacing" 
               "inner-border")
             (list-class-style-property-names "GtkButton")))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkButton"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkButton" GTK-BUTTON
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_button_get_type")
                       ((ALWAYS-SHOW-IMAGE GTK-BUTTON-ALWAYS-SHOW-IMAGE
                         "always-show-image" "gboolean" T T)
                        (IMAGE GTK-BUTTON-IMAGE "image" "GtkWidget" T T)
                        (IMAGE-POSITION GTK-BUTTON-IMAGE-POSITION
                         "image-position" "GtkPositionType" T T)
                        (LABEL GTK-BUTTON-LABEL "label" "gchararray" T T)
                        (RELIEF GTK-BUTTON-RELIEF "relief" "GtkReliefStyle" T
                         T)
                        (USE-STOCK GTK-BUTTON-USE-STOCK "use-stock" "gboolean"
                         T T)
                        (USE-UNDERLINE GTK-BUTTON-USE-UNDERLINE "use-underline"
                         "gboolean" T T)
                        (XALIGN GTK-BUTTON-XALIGN "xalign" "gfloat" T T)
                        (YALIGN GTK-BUTTON-YALIGN "yalign" "gfloat" T T)))
             (get-g-type-definition "GtkButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-button-properties
  (let ((button (make-instance 'gtk-button)))
    (is-false (gtk-button-always-show-image button))
    (is-false (gtk-button-image button))
    (is (eq :left (gtk-button-image-position button)))
    (is-false (gtk-button-label button))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-false (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-button-style-properties
  (let ((button (make-instance 'gtk-button)))
    (is (= 0 (gtk-widget-style-property button "child-displacement-x")))
    (is (= 0 (gtk-widget-style-property button "child-displacement-y")))
    (is-false (gtk-widget-style-property button "default-border"))
    (is-false (gtk-widget-style-property button "default-outside-border"))
    (is-false (gtk-widget-style-property button "displace-focus"))
    (is (= 2 (gtk-widget-style-property button "image-spacing")))
    (is-false (gtk-widget-style-property button "inner-border"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk-button-new

(test gtk-button-new
  (let ((button (gtk-button-new)))
    (is-false (gtk-button-always-show-image button))
    (is-false (gtk-button-image button))
    (is (eq :left (gtk-button-image-position button)))
    (is-false (gtk-button-label button))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-false (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;;     gtk-button-new-with-label

(test gtk-button-new-with-label
  (let ((button (gtk-button-new-with-label "Label")))
    (is-false (gtk-button-always-show-image button))
    (is-false (gtk-button-image button))
    (is (eq :left (gtk-button-image-position button)))
    (is (string= "Label" (gtk-button-label button)))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-false (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;;     gtk-button-new-with-mnemonic

(test gtk-button-new-with-mnemonic
  (let ((button (gtk-button-new-with-mnemonic "_Mnemonic")))
    (is-false (gtk-button-always-show-image button))
    (is-false (gtk-button-image button))
    (is (eq :left (gtk-button-image-position button)))
    (is (string= "_Mnemonic" (gtk-button-label button)))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-true (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;;     gtk-button-new-from-icon-name

(test gtk-button-new-from-icon-name
  (let ((button (gtk-button-new-from-icon-name "edit-copy" :button)))
    (is-false (gtk-button-always-show-image button))
    (is (typep (gtk-button-image button) 'gtk-image))
    (is (eq :left (gtk-button-image-position button)))
    (is-false (gtk-button-label button))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-false (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;;     gtk-button-new-from-stock

(test gtk-button-new-from-stock
  (let ((button (gtk-button-new-from-stock "gtk-close")))
    (is-false (gtk-button-always-show-image button))
    (is (typep (gtk-button-image button) 'gtk-image))
    (is (eq :left (gtk-button-image-position button)))
    (is (string= "gtk-close" (gtk-button-label button)))
    (is (eq :normal (gtk-button-relief button)))
    (is-true (gtk-button-use-stock button))
    (is-true (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;;     gtk-button-clicked

#+nil
(test gtk-button-clicked
  (let* ((message nil)
         (button (make-instance 'gtk-button))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (setf message "Signal clicked")
                         (is (typep widget 'gtk-button))
                         t))))
    (declare (ignore handler-id))
    ;; Emit the signal
    (is-false (gtk-button-clicked button))
    (is (string= "Signal clicked" message))))

;;;     gtk-button-alignment                               deprecated

(test gtk-button-alignment
  (let ((button (make-instance 'gtk-button))
        (label (make-instance 'gtk-label)))
    (is-false (gtk-container-add button label))
    (is (equal '(0.5 0.5) (gtk-button-alignment button)))
    (is (equal '(0.1 0.9) (setf (gtk-button-alignment button) '(0.1 0.9))))
    (is (equal '(0.1 0.9) (gtk-button-alignment button)))))

;;;     gtk-button-event-window

(test gtk-button-event-window
  (let ((window (make-instance 'gtk-window :type :toplevel))
        (button (gtk-button-new-from-icon-name "gtk-close" :button)))
    (is-false (gtk-container-add window button))
    (is-false (gtk-widget-realize window))
    (is-false (gtk-widget-realize button))
    (is (typep (gtk-button-event-window button) 'gdk-window))))

;;; 2021-10-19
