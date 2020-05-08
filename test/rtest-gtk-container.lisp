(def-suite gtk-container :in gtk-suite)
(in-suite gtk-container)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkResizeMode

(test gtk-resize-mode
  ;; Check the type
  (is-true (g-type-is-enum "GtkResizeMode"))
  ;; Check the type initializer
  (is (string= "GtkResizeMode"
               (g-type-name (gtype (foreign-funcall "gtk_resize_mode_get_type" :int)))))
  ;; Check the registered name
  (is (eq 'gtk-resize-mode (gobject::registered-enum-type "GtkResizeMode")))
  ;; Check the names
  (is (equal '("GTK_RESIZE_PARENT" "GTK_RESIZE_QUEUE" "GTK_RESIZE_IMMEDIATE")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkResizeMode"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkResizeMode"))))
  ;; Check the nick names
  (is (equal '("parent" "queue" "immediate")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkResizeMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkResizeMode"
                             GTK-RESIZE-MODE
                             (:EXPORT T :TYPE-INITIALIZER "gtk_resize_mode_get_type")
                             (:PARENT 0)
                             (:QUEUE 1)
                             (:IMMEDIATE 2))
             (gobject::get-g-type-definition "GtkResizeMode"))))

;;;     GtkContainer

(test gtk-container-class
  ;; Type check
  (is-true  (g-type-is-object "GtkContainer"))
  ;; Check the registered name
  (is (eq 'gtk-container
          (registered-object-type-by-name "GtkContainer")))
  ;; Check the type initializer
  (is (string= "GtkContainer"
               (g-type-name (gtype (foreign-funcall "gtk_container_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkWidget") (g-type-parent "GtkContainer")))
  ;; Check the children
  (is (equal '("GtkBin" "GtkMenuShell" "GtkBox" "GtkGrid" "GtkListBox" "GtkFlowBox"
               "GtkStack" "GtkHeaderBar" "GtkPaned" "GtkLayout" "GtkNotebook" "GtkFixed"
               "GtkTextView" "GtkTreeView" "GtkIconView" "GtkToolItemGroup" "GtkToolbar"
               "GtkToolPalette" "GtkSocket" "GtkTable")
             (mapcar #'gtype-name (g-type-children "GtkContainer"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkContainer"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand" "focus-on-click"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "is-focus" "margin" "margin-bottom" "margin-end" "margin-left"
               "margin-right" "margin-start" "margin-top" "name" "no-show-all" "opacity"
               "parent" "receives-default" "resize-mode" "scale-factor" "sensitive" "style"
               "tooltip-markup" "tooltip-text" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkContainer"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkContainer"))))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "padding" "pack-type" "position")
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkBox"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkContainer" GTK-CONTAINER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_container_get_type")
                       ((BORDER-WIDTH GTK-CONTAINER-BORDER-WIDTH "border-width"
                         "guint" T T)
                        (CHILD GTK-CONTAINER-CHILD "child" "GtkWidget" NIL T)
                        (RESIZE-MODE GTK-CONTAINER-RESIZE-MODE "resize-mode"
                         "GtkResizeMode" T T)))
             (get-g-type-definition "GtkContainer"))))

;;; ----------------------------------------------------------------------------
;;; Check accessor functions of GtContainer
;;; ----------------------------------------------------------------------------

;;; --- gtk-container-border-width ---------------------------------------------

(test gtk-container-border-width.1
  (let ((box (make-instance 'gtk-box)))
    (is (eql 0 (gtk-container-border-width box)))
    (setf (gtk-container-border-width box) 12)
    (is (eql 12 (gtk-container-border-width box)))))

(test gtk-container-border-width.2
  (let ((box (make-instance 'gtk-box :border-width 12)))
    (is (eql 12 (gtk-container-border-width box)))))

;;; --- gtk-container-child ----------------------------------------------------

(test gtk-container-child.1
  (let ((box (make-instance 'gtk-box)))
    ;; The CHILD property is not readable
    (signals (error) (gtk-container-child box))))

(test gtk-container-child.2
  (let ((box (make-instance 'gtk-box))
        (button (make-instance 'gtk-button)))
    ;; Put a button into the box
    (setf (gtk-container-child box) button)
    (is (equal (list button)
               (gtk-container-children box)))))

;;; --- gtk-container-resize-mode ----------------------------------------------

(test gtk-container-resize-mode
  (let ((box (make-instance 'gtk-box)))
    (is (eql :parent (gtk-container-resize-mode box)))
    (setf (gtk-container-resize-mode box) :queue)
    (is (eql :queue (gtk-container-resize-mode box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_container_add
;;;     gtk_container_remove
;;;     gtk_container_add_with_properties
;;;     gtk_container_check_resize

;;;     gtk_container_foreach

(test gtk-container-foreach
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (collect nil))
    ;; Add 10 label to the container
    (dotimes (i 10)
      (is-false (gtk-container-add box (make-instance 'gtk-label))))
    ;; Check the length of the list of children
    (is (= 10 (length (gtk-container-children box))))
    ;; Collect the labels
    (is-false (gtk-container-foreach box (lambda (widget) (push widget collect))))
    ;; Check the number of labels
    (is (= 10 (length collect)))
    ;; Set the string "text" on the labels in the box
    (is-false (gtk-container-foreach box (lambda (widget) (setf (gtk-label-text widget) "text"))))
    ;; Read the string "text" of the labels in the box
    (is-false (gtk-container-foreach box (lambda (widget) (is (string= "text" (gtk-label-text widget))))))))

;;;     gtk_container_get_children

;;;     gtk_container_get_path_for_child

(test gtk-container-path-for-child
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (child (make-instance 'gtk-label)))
    (is-false (gtk-container-add box child))
    (is (eq 'gtk-widget-path (type-of (gtk-container-path-for-child box child))))
    (is (string= "box:dir-ltr.vertical label:dir-ltr"
                 (gtk-widget-path-to-string (gtk-container-path-for-child box child))))))

;;;     gtk_container_set_reallocate_redraws

;;;     gtk_container_get_focus_child
;;;     gtk_container_set_focus_child

(test gtk-container-focus-child
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button)))

    (is-false (gtk-container-add box button1))
    (is-false (gtk-container-add box button2))
    (is-false (gtk-container-add box button3))

    (is-false (gtk-container-focus-child box))

    (is-true (gtk-widget-can-focus button1))
    (is-false (gtk-widget-grab-focus button1))
    (is (equal button1 (gtk-container-focus-child box)))

    (is-true (gtk-widget-can-focus button2))
    (is (equal button2 (setf (gtk-container-focus-child box) button2)))
    (is (equal button2 (gtk-container-focus-child box)))))

;;;     gtk_container_get_focus_vadjustment
;;;     gtk_container_set_focus_vadjustment
;;;     gtk_container_get_focus_hadjustment
;;;     gtk_container_set_focus_hadjustment
;;;     gtk_container_resize_children

;;;     gtk_container_child_type

(test gtk-container-child-type.1
  (let ((box (make-instance 'gtk-box)))
    (is (string= "GtkWidget" (g-type-name (gtk-container-child-type box))))))

(test gtk-container-child-type.2
  (let ((paned (make-instance 'gtk-paned))
        (frame1 (make-instance 'gtk-frame))
        (frame2 (make-instance 'gtk-frame)))
    (is (string= "GtkWidget" (g-type-name (gtk-container-child-type paned))))
    (is-false (gtk-paned-add1 paned frame1))
    (is-false (gtk-paned-add2 paned frame2))
    (is (string= "void" (g-type-name (gtk-container-child-type paned))))))

;;;     gtk_container_child_get

(test gtk-container-child-get
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (button (make-instance 'gtk-button)))
    (is-false (gtk-container-add box button))
    (is (equal '(nil t :start 0 0)
               (gtk-container-child-get box button "expand" "fill" "pack-type" "padding" "position")))))

;;;     gtk_container_child_set

(test gtk-container-child-set
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (button (make-instance 'gtk-button)))
    (is-false (gtk-container-add box button))
    (is-false (gtk-container-child-set box button "expand" t "fill" nil "pack-type" :end "padding" 6))
    (is (equal '(t nil :end 6 0)
               (gtk-container-child-get box button "expand" "fill" "pack-type" "padding" "position")))))

;;;     gtk_container_child_get_property

(test gtk-container-child-get-property
  (let ((box (make-instance 'gtk-box))
        (button (make-instance 'gtk-button)))
    (is-false (gtk-container-add box button))
    (is-false (gtk-container-child-get-property box button "expand"))
    (is-true (gtk-container-child-get-property box button "fill"))
    (is (eq :start (gtk-container-child-get-property box button "pack-type")))
    (is (= 0 (gtk-container-child-get-property box button "padding")))
    (is (= 0 (gtk-container-child-get-property box button "position")))))

;;;     gtk_container_child_set_property

(test gtk-container-child-set-property
  (let ((box (make-instance 'gtk-box))
        (button (make-instance 'gtk-button)))
    (is-false (gtk-container-add box button))

    (is-false (gtk-container-child-set-property box button "expand" t))
    (is-true (gtk-container-child-get-property box button "expand"))

    (is-false (gtk-container-child-set-property box button "fill" nil))
    (is-false (gtk-container-child-get-property box button "fill"))

    (is-false (gtk-container-child-set-property box button "pack-type" :end))
    (is (eq :end (gtk-container-child-get-property box button "pack-type")))

    (is-false (gtk-container-child-set-property box button "padding" 6))
    (is (= 6 (gtk-container-child-get-property box button "padding")))))

;;;     gtk_container_child_get_valist
;;;     gtk_container_child_set_valist
;;;     gtk_container_child_notify
;;;     gtk_container_child_notify_by_pspec

;;;     gtk_container_forall

(test gtk-container-forall
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (collect nil))
    ;; Add 10 label to the container
    (dotimes (i 10)
      (is-false (gtk-container-add box (make-instance 'gtk-label))))
    ;; Check the length of the list of children, we have no internal children
    (is (= 10 (length (gtk-container-children box))))
    ;; Collect the labels
    (is-false (gtk-container-forall box (lambda (widget) (push widget collect))))
    ;; Check the number of labels
    (is (= 10 (length collect)))
    ;; Set the string "text" on the labels in the box
    (is-false (gtk-container-forall box (lambda (widget) (setf (gtk-label-text widget) "text"))))
    ;; Read the string "text" of the labels in the box
    (is-false (gtk-container-forall box (lambda (widget) (is (string= "text" (gtk-label-text widget))))))))

;;;     gtk_container_propagate_draw

;;;     gtk_container_get_focus_chain
;;;     gtk_container_set_focus_chain
;;;     gtk_container_unset_focus_chain

(test gtk-container-focus-chain
  (let ((box (make-instance 'gtk-box :orientation :vertical))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button)))

    (is-false (gtk-container-add box button1))
    (is-false (gtk-container-add box button2))
    (is-false (gtk-container-add box button3))

    (is-false (gtk-container-focus-chain box))

    (is (listp (setf (gtk-container-focus-chain box) (list button1 button2 button3))))

    (is (equal button1 (first (gtk-container-focus-chain box))))
    (is (equal button2 (second (gtk-container-focus-chain box))))
    (is (equal button3 (third (gtk-container-focus-chain box))))

    (is-false (gtk-container-unset-focus-chain box))
    (is-false (gtk-container-focus-chain box))))

;;;     gtk_container_class_find_child_property

(test gtk-container-class-find-child-property
  (is-true (parse-g-param-spec (gtk-container-class-find-child-property (g-type-class-ref "GtkBox")
                                                                        "expand"))))

;;;     gtk_container_class_install_child_property
;;;     gtk_container_class_install_child_properties

;;;     gtk_container_class_list_child_properties

(test gtk-container-class-list-child-properties
    (is (listp (gtk-container-class-list-child-properties "GtkBox")))
    (is (equal '("expand" "fill" "padding" "pack-type" "position")
               (mapcar #'param-spec-name
                       (gtk-container-class-list-child-properties "GtkBox")))))

;;;     gtk_container_class_handle_border_width

