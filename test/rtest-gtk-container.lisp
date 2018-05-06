(in-package :gtk-testsuite)

(def-suite gtk-container :in gtk-suite)
(in-suite gtk-container)

#+sbcl (sb-ext:gc :full t)
#+openmcl (ccl:gc)

;;; --- GtkResizeMode ----------------------------------------------------------

(test gtk-resize-mode
  (is-false (g-type-is-object "GtkResizeMode"))
  (is-false (g-type-is-abstract "GtkResizeMode"))
  (is-true  (g-type-is-derived "GtkResizeMode"))
  (is-false (g-type-is-fundamental "GtkResizeMode"))
  (is-true  (g-type-is-value-type "GtkResizeMode"))
  (is-true  (g-type-has-value-table "GtkResizeMode"))
  (is-true  (g-type-is-classed "GtkResizeMode"))
  (is-false (g-type-is-instantiatable "GtkResizeMode"))
  (is-true  (g-type-is-derivable "GtkResizeMode"))
  (is-false (g-type-is-deep-derivable "GtkResizeMode"))
  (is-false (g-type-is-interface "GtkResizeMode"))

  ;; Check the registered name
  (is (eql 'gtk-resize-mode (gobject::registered-enum-type "GtkResizeMode")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkResizeMode"))))
    (is (equal (gtype "GtkResizeMode") (g-type-from-class class)))
    (is (equal (gtype "GtkResizeMode") (g-enum-class-type class)))
    (is (equal "GtkResizeMode" (g-enum-class-type-name class)))
    (is-true (g-is-enum-class class))
    (is (equal (gtype "GtkResizeMode")
               (g-type-from-class  (g-type-class-peek "GtkResizeMode"))))
    (is (equal (gtype "GtkResizeMode")
               (g-type-from-class  (g-type-class-peek-static "GtkResizeMode"))))
    (g-type-class-unref class))

  ;; Check some more GType information
  (is-true (g-type-is-enum (gtype "GtkResizeMode")))
  (is (equal (gtype "GEnum") (g-type-parent "GtkResizeMode")))
  (is (= 2  (g-type-depth "GtkResizeMode")))
  (is-false (g-type-is-a "GtkResizeMode" "GObject"))
  (is-false (g-type-is-a "GtkResizeMode" "gboolean"))
  (is-true  (g-type-is-a "GtkResizeMode" "GEnum"))

  ;; Get the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkResizeMode"
                             GTK-RESIZE-MODE
                             (:EXPORT T :TYPE-INITIALIZER
                                        "gtk_resize_mode_get_type")
                             (:PARENT 0)
                             (:QUEUE 1)
                             (:IMMEDIATE 2))
             (gobject::get-g-enum-definition "GtkResizeMode"))))

;;; --- GtkContainer -----------------------------------------------------------

(test gtk-container-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkContainer"))
  (is-true  (g-type-is-abstract "GtkContainer"))
  (is-true  (g-type-is-derived "GtkContainer"))
  (is-false (g-type-is-fundamental "GtkContainer"))
  (is-true  (g-type-is-value-type "GtkContainer"))
  (is-true  (g-type-has-value-table "GtkContainer"))
  (is-true  (g-type-is-classed "GtkContainer"))
  (is-true  (g-type-is-instantiatable "GtkContainer")) ; Why is this true?
  (is-true  (g-type-is-derivable "GtkContainer"))
  (is-true  (g-type-is-deep-derivable "GtkContainer"))
  (is-false (g-type-is-interface "GtkContainer"))

  ;; Check the registered name
  (is (eq 'gtk-container
          (registered-object-type-by-name "GtkContainer")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkContainer"))))
    (is (equal (gtype "GtkContainer") (g-type-from-class class)))
    (is (equal (gtype "GtkContainer") (g-object-class-type class)))
    (is (equal "GtkContainer" (g-object-class-name class)))
    (is (equal (gtype "GtkContainer")
               (g-type-from-class  (g-type-class-peek "GtkContainer"))))
    (is (equal (gtype "GtkContainer")
               (g-type-from-class  (g-type-class-peek-static "GtkContainer"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-container)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-container (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkContainer" (gobject-class-g-type-name class)))
    (is (equal "GtkContainer" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_container_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GtkWidget") (g-type-parent "GtkContainer")))
  (is (= 4 (g-type-depth "GtkContainer")))
  (is (equal (gtype "GInitiallyUnowned")
             (g-type-next-base "GtkContainer" "GObject")))
  (is-true  (g-type-is-a "GtkContainer" "GObject"))
  (is-true  (g-type-is-a "GtkContainer" "GInitiallyUnowned"))
  (is-false (g-type-is-a "GtkContainer" "gboolean"))
  (is-false (g-type-is-a "GtkContainer" "GtkWindow"))

  ;; Check the children
  (is (subsetp '("GtkBin" "GtkMenuShell" "GtkBox" "GtkTable" "GtkGrid" "GtkLayout"
                 "GtkFixed" "GtkNotebook" "GtkPaned" "GtkTextView" "GtkTreeView"
                 "GtkIconView" "GtkToolItemGroup" "GtkToolbar" "GtkToolPalette"
                 "GtkSocket" "GtkHeaderBar")
               (mapcar #'gtype-name (g-type-children "GtkContainer"))
               :test #'string=))
             
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkContainer"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkContainer" query)
    (is (equal (gtype "GtkContainer")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkContainer"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 976
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 40
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the class properties.
  (is (subsetp '("app-paintable" "border-width" "can-default" "can-focus" "child"
                 "composite-child" "double-buffered" "events" "expand" "halign"
                 "has-default" "has-focus" "has-tooltip" "height-request"
                 "hexpand" "hexpand-set" "is-focus" "margin" "margin-bottom"
                 "margin-end" "margin-left" "margin-right" "margin-start"
                 "margin-top" "name" "no-show-all" "opacity" "parent"
                 "receives-default" "resize-mode" "scale-factor" "sensitive"
                 "style" "tooltip-markup" "tooltip-text" "valign" "vexpand"
                 "vexpand-set" "visible" "width-request" "window")
                (mapcar #'param-spec-name
                        (g-object-class-list-properties "GtkContainer"))
                :test #'string=))

  ;; Get the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkContainer"))))

  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkContainer"))))

  ;; Get the class definition
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
               (gtk-container-get-children box)))))

;;; --- gtk-container-resize-mode ----------------------------------------------

(test gtk-container-resize-mode
  (let ((box (make-instance 'gtk-box)))
    (is (eql :parent (gtk-container-resize-mode box)))
    (setf (gtk-container-resize-mode box) :queue)
    (is (eql :queue (gtk-container-resize-mode box)))))

