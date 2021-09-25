(def-suite gtk-bin :in gtk-suite)
(in-suite gtk-bin)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBin

(test gtk-bin-class
  ;; Type check
  (is (g-type-is-object "GtkBin"))
  ;; Check the registered name
  (is (eq 'gtk-bin
          (registered-object-type-by-name "GtkBin")))
  ;; Check the type initializer
  (is (eq (gtype "GtkBin")
          (gtype (foreign-funcall "gtk_bin_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkContainer") (g-type-parent "GtkBin")))
  ;; Check the children
  (is (equal '("GtkRevealer" "GtkListBoxRow" "GtkFlowBoxChild" "GtkStackSidebar"
               "GtkActionBar" "GtkOverlay" "GtkExpander" "GtkFrame"
               "GtkScrolledWindow" "GtkWindow" "GtkButton" "GtkSearchBar"
               "GtkMenuItem" "GtkComboBox" "GtkToolItem" "GtkPopover"
               "GtkEventBox" "GtkHandleBox" "GtkViewport" "GtkAlignment")
             (mapcar #'g-type-name (g-type-children "GtkBin"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkBin"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand"
               "focus-on-click" "halign" "has-default" "has-focus" "has-tooltip"
               "height-request" "hexpand" "hexpand-set" "is-focus" "margin"
               "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "name" "no-show-all" "opacity"
               "parent" "receives-default" "resize-mode" "scale-factor"
               "sensitive" "style" "tooltip-markup" "tooltip-text" "valign"
               "vexpand" "vexpand-set" "visible" "width-request" "window")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GtkBin"))
                   #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkBin"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkBin"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkBin" GTK-BIN
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_bin_get_type")
                       NIL)
             (get-g-type-definition "GtkBin"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_bin_get_child

(test gtk-bin-child
  (let ((bin (make-instance 'gtk-frame))
        (child (make-instance 'gtk-label)))
    (is-false (gtk-container-add bin child))
    (is-true (gtk-bin-child bin))
    (is (typep (gtk-bin-child bin) 'gtk-label))))

;;; 2021-9-12
