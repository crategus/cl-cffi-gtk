(def-suite gtk-tool-palette :in gtk-suite)
(in-suite gtk-tool-palette)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolPaletteDragTargets

(test gtk-tool-palette-drag-targets
  ;; Check the type
  (is (g-type-is-flags "GtkToolPaletteDragTargets"))
  ;; Check the registered name
  (is (eq 'gtk-tool-palette-drag-targets
          (registered-flags-type "GtkToolPaletteDragTargets")))
  ;; Check the type initializer
  (is (eq (gtype "GtkToolPaletteDragTargets")
          (gtype (foreign-funcall "gtk_tool_palette_drag_targets_get_type"
                                  g-size))))
  ;; Check the names
  (is (equal '("GTK_TOOL_PALETTE_DRAG_ITEMS" "GTK_TOOL_PALETTE_DRAG_GROUPS")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkToolPaletteDragTargets"))))
  ;; Check the values
  (is (equal '(1 2)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkToolPaletteDragTargets"))))
  ;; Check the nick names
  (is (equal '("items" "groups")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkToolPaletteDragTargets"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkToolPaletteDragTargets"
                              GTK-TOOL-PALETTE-DRAG-TARGETS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_tool_palette_drag_targets_get_type")
                              (:ITEMS 1)
                              (:GROUPS 2))
             (get-g-type-definition "GtkToolPaletteDragTargets"))))

;;;     GtkToolPalette

(test gtk-tool-palette-class
  ;; Type check
  (is (g-type-is-object "GtkToolPalette"))
  ;; Check the registered name
  (is (eq 'gtk-tool-palette
          (registered-object-type-by-name "GtkToolPalette")))
  ;; Check the type initializer
  (is (eq (gtype "GtkToolPalette")
          (gtype (foreign-funcall "gtk_tool_palette_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkContainer") (g-type-parent "GtkToolPalette")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkToolPalette"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkScrollable")
             (mapcar #'g-type-name (g-type-interfaces "GtkToolPalette"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand"
               "focus-on-click" "hadjustment" "halign" "has-default" "has-focus"
               "has-tooltip" "height-request" "hexpand" "hexpand-set"
               "hscroll-policy" "icon-size" "icon-size-set" "is-focus" "margin"
               "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "name" "no-show-all" "opacity"
               "orientation" "parent" "receives-default" "resize-mode"
               "scale-factor" "sensitive" "style" "toolbar-style"
               "tooltip-markup" "tooltip-text" "vadjustment" "valign" "vexpand"
               "vexpand-set" "visible" "vscroll-policy" "width-request"
               "window")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GtkToolPalette"))
                   #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkToolPalette"))))
  ;; Get the names of the child properties
  (is (equal '("exclusive" "expand")
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkToolPalette"))))
  ;; Check the class definition
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

;;; --- Functions --------------------------------------------------------------

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

(test gtk-tool-palette-drag-target-group
  (is (equal '("application/x-gtk-tool-palette-group" (:SAME-APP) 0)
             (gtk-tool-palette-drag-target-group))))

;;;     gtk_tool_palette_get_drag_target_item

(test gtk-tool-palette-drag-target-item
  (is (equal '("application/x-gtk-tool-palette-item" (:SAME-APP) 0)
             (gtk-tool-palette-drag-target-item))))

;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;
;;;
;;;     gtk_tool_palette_set_drag_source
;;;     gtk_tool_palette_get_hadjustment
;;;     gtk_tool_palette_get_vadjustment

