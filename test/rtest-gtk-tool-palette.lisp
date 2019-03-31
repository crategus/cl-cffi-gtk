
(def-suite gtk-tool-palette :in gtk-suite)
(in-suite gtk-tool-palette)

;;;   GtkToolPalette

(test gtk-tool-palette-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkToolPalette"))
  (is-false (g-type-is-abstract "GtkToolPalette"))
  (is-true  (g-type-is-derived "GtkToolPalette"))
  (is-false (g-type-is-fundamental "GtkToolPalette"))
  (is-true  (g-type-is-value-type "GtkToolPalette"))
  (is-true  (g-type-has-value-table "GtkToolPalette"))
  (is-true  (g-type-is-classed "GtkToolPalette"))
  (is-true  (g-type-is-instantiatable "GtkToolPalette"))
  (is-true  (g-type-is-derivable "GtkToolPalette"))
  (is-true  (g-type-is-deep-derivable "GtkToolPalette"))
  (is-false (g-type-is-interface "GtkToolPalette"))

  ;; Check the registered name
  (is (eq 'gtk-tool-palette
          (registered-object-type-by-name "GtkToolPalette")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkToolPalette"))))
    (is (equal (gtype "GtkToolPalette") (g-type-from-class class)))
    (is (equal (gtype "GtkToolPalette") (g-object-class-type class)))
    (is (equal "GtkToolPalette" (g-object-class-name class)))
    (is (equal (gtype "GtkToolPalette") (g-type-from-class  (g-type-class-peek "GtkToolPalette"))))
    (is (equal (gtype "GtkToolPalette") (g-type-from-class  (g-type-class-peek-static "GtkToolPalette"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-tool-palette)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-tool-palette (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkToolPalette" (gobject-class-g-type-name class)))
    (is (equal "GtkToolPalette" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_tool_palette_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GtkContainer") (g-type-parent "GtkToolPalette")))
  (is (= 5 (g-type-depth "GtkToolPalette")))
  (is (equal (gtype "GtkContainer")
             (g-type-next-base "GtkToolPalette" "GtkWidget")))
  (is-true  (g-type-is-a "GtkToolPalette" "GtkWidget"))
  (is-true  (g-type-is-a "GtkToolPalette" "GtkContainer"))
  (is-false (g-type-is-a "GtkToolPalette" "gboolean"))
  (is-false (g-type-is-a "GtkToolPalette" "GtkWindow"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkToolPalette"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface"
               "GtkBuildable"
               "GtkOrientable"
               "GtkScrollable")
             (mapcar #'gtype-name (g-type-interfaces "GtkToolPalette"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkToolPalette" query)
    (is (equal (gtype "GtkToolPalette")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkToolPalette"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 1008 
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 48
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
 "composite-child" "double-buffered" "events" "expand" "focus-on-click"
 "hadjustment" "halign" "has-default" "has-focus" "has-tooltip"
 "height-request" "hexpand" "hexpand-set" "hscroll-policy" "icon-size"
 "icon-size-set" "is-focus" "margin" "margin-bottom" "margin-end" "margin-left"
 "margin-right" "margin-start" "margin-top" "name" "no-show-all" "opacity"
 "orientation" "parent" "receives-default" "resize-mode" "scale-factor"
 "sensitive" "style" "toolbar-style" "tooltip-markup" "tooltip-text"
 "vadjustment" "valign" "vexpand" "vexpand-set" "visible" "vscroll-policy"
 "width-request" "window")

             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkToolPalette"))
                          #'string-lessp)))

  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'param-spec-name (gtk-widget-class-list-style-properties "GtkToolPalette"))))

  ;; Get the names to the child properties
  (is (equal '("exclusive" "expand")
             (mapcar #'param-spec-name (gtk-container-class-list-child-properties "GtkToolPalette"))))

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkToolPalette" GTK-TOOL-PALETTE
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                         "GtkScrollable")
                        :TYPE-INITIALIZER "gtk_tool_palette_get_type")
                       ((ICON-SIZE GTK-TOOL-PALETTE-ICON-SIZE "icon-size"
                         "GtkIconSize" T T)
                        (ICON-SIZE-SET GTK-TOOL-PALETTE-ICON-SIZE-SET
                         "icon-size-set" "gboolean" T T)
                        (TOOLBAR-STYLE GTK-TOOL-PALETTE-TOOLBAR-STYLE
                         "toolbar-style" "GtkToolbarStyle" T T)))
             (get-g-type-definition "GtkToolPalette"))))

#+nil             
(test gtk-frame-properties
  (let ((widget (make-instance 'gtk-frame)))
    (is-false (gtk-frame-label widget))
    (is-false (gtk-frame-label-widget widget))
    (is (= 0.0 (gtk-frame-label-xalign widget)))
    (is (= 0.5 (gtk-frame-label-yalign widget)))
    (is (eq :etched-in (gtk-frame-shadow-type widget)))
  ))

#+nil
(test gtk-frame-style-properties
  (let ((widget (make-instance 'gtk-frame)))
    (is (= 0.04 (gtk-widget-style-get-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-get-property widget "cursor-color"))
    (is (equal "" (gtk-widget-style-get-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-get-property widget "focus-line-width")))
    (is (= 0 (gtk-widget-style-get-property widget "focus-padding")))
    (is-true (gtk-widget-style-get-property widget "interior-focus"))
    (is (eq 'gdk-color (type-of (gtk-widget-style-get-property widget "link-color"))))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-get-property widget "secondary-cursor-color"))
    (is (=  2 (gtk-widget-style-get-property widget "separator-height")))
    (is (=  2 (gtk-widget-style-get-property widget "separator-width")))
    (is (= 20 (gtk-widget-style-get-property widget "text-handle-height")))
    (is (= 16 (gtk-widget-style-get-property widget "text-handle-width")))
    (is (eq 'gdk-color (type-of (gtk-widget-style-get-property widget "visited-link-color"))))
    (is-false  (gtk-widget-style-get-property widget "wide-separators"))
    (is-false (gtk-widget-style-get-property widget "window-dragging"))
  ))

;;;     gtk_tool_palette_new
;;;     gtk_tool_palette_get_exclusive
;;;     gtk_tool_palette_set_exclusive
;;;     gtk_tool_palette_get_expand
;;;     gtk_tool_palette_set_expand
;;;     gtk_tool_palette_get_group_position
;;;     gtk_tool_palette_set_group_position
;;;     gtk_tool_palette_get_icon_size
;;;     gtk_tool_palette_set_icon_size
;;;     gtk_tool_palette_unset_icon_size
;;;     gtk_tool_palette_get_style
;;;     gtk_tool_palette_set_style
;;;     gtk_tool_palette_unset_style
;;;     gtk_tool_palette_add_drag_dest
;;;     gtk_tool_palette_get_drag_item
;;;     gtk_tool_palette_get_drag_target_group
;;;     gtk_tool_palette_get_drag_target_item
;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;
;;;     GtkToolPaletteDragTargets
;;;
;;;     gtk_tool_palette_set_drag_source
;;;     gtk_tool_palette_get_hadjustment
;;;     gtk_tool_palette_get_vadjustment

