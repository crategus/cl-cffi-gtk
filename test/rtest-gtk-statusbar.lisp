(def-suite gtk-statusbar :in gtk-suite)
(in-suite gtk-statusbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStatusbar

(test gtk-statusbar-class
  ;; Type check
  (is-true  (g-type-is-object "GtkStatusbar"))
  ;; Check the registered name
  (is (eq 'gtk-statusbar
          (registered-object-type-by-name "GtkStatusbar")))
  ;; Check the type initializer
  (is (string= "GtkStatusbar"
               (g-type-name (gtype (foreign-funcall "gtk_statusbar_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkBox") (g-type-parent "GtkStatusbar")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkStatusbar"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'gtype-name (g-type-interfaces "GtkStatusbar"))))
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
                                  (g-object-class-list-properties "GtkStatusbar"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "shadow-type")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkStatusbar"))))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "padding" "pack-type" "position")
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkStatusbar"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkStatusbar" GTK-STATUSBAR
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_statusbar_get_type")
                       NIL)
             (get-g-type-definition "GtkStatusbar"))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-statusbar-style-properties
  (let ((statusbar (make-instance 'gtk-statusbar)))
    (is (eq :in (gtk-widget-style-get-property statusbar "shadow-type")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_statusbar_new

(test gtk-statusbar-new
  (is (eq 'gtk-statusbar (type-of (gtk-statusbar-new)))))

;;;     gtk_statusbar_get_context_id

(test gtk-statusbar-context-id
  (let ((statusbar (gtk-statusbar-new)))
    ;; Get the context IDs
    (is (= 1 (gtk-statusbar-context-id statusbar "context1")))
    (is (= 2 (gtk-statusbar-context-id statusbar "context2")))
    (is (= 3 (gtk-statusbar-context-id statusbar "context3")))
    ;; Read the context IDs
    (is (= 1 (gtk-statusbar-context-id statusbar "context1")))
    (is (= 2 (gtk-statusbar-context-id statusbar "context2")))
    (is (= 3 (gtk-statusbar-context-id statusbar "context3")))))

;;;     gtk_statusbar_push
;;;     gtk_statusbar_pop
;;;     gtk_statusbar_remove
;;;     gtk_statusbar_remove_all

(test gtk-statusbar-push
  (let* ((statusbar (gtk-statusbar-new))
         (context-id-1 (gtk-statusbar-context-id statusbar "context1"))
         (context-id-2 (gtk-statusbar-context-id statusbar "context2"))
         message-id-1
         message-id-2)

    (is (eq 'gtk-statusbar (type-of statusbar)))
    (is (= 1 context-id-1))
    (is (= 2 context-id-2))
    ;; Set some message IDs on the contexts
    (is (= 1 (setf message-id-1 (gtk-statusbar-push statusbar context-id-1 "message1"))))
    (is (= 2 (setf message-id-2 (gtk-statusbar-push statusbar context-id-1 "message2"))))
    (is (= 3 (setf message-id-1 (gtk-statusbar-push statusbar context-id-2 "message1"))))
    (is (= 4 (setf message-id-2 (gtk-statusbar-push statusbar context-id-2 "message2"))))
    ;; Remove some messages
    (is-false (gtk-statusbar-remove statusbar "context1" 1))
    (is-false (gtk-statusbar-remove-all statusbar "context2"))))

;;;     gtk_statusbar_get_message_area

(test gtk-statusbar-message-area
  (let ((statusbar (gtk-statusbar-new)))
    (is (eq 'gtk-box (type-of (gtk-statusbar-message-area statusbar))))))

