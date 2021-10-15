(def-suite gtk-style-context :in gtk-suite)
(in-suite gtk-style-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkJunctionSides

(test gtk-junction-sides
  ;; Check the type
  (is (g-type-is-flags "GtkJunctionSides"))
  ;; Check the registered name
  (is (eq 'gtk-junction-sides
          (registered-flags-type "GtkJunctionSides")))
  ;; Check the names
  (is (equal '("GTK_JUNCTION_NONE" "GTK_JUNCTION_CORNER_TOPLEFT"
               "GTK_JUNCTION_CORNER_TOPRIGHT" "GTK_JUNCTION_CORNER_BOTTOMLEFT"
               "GTK_JUNCTION_CORNER_BOTTOMRIGHT" "GTK_JUNCTION_TOP"
               "GTK_JUNCTION_BOTTOM" "GTK_JUNCTION_LEFT" "GTK_JUNCTION_RIGHT")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkJunctionSides"))))
  ;; Check the values
  (is (equal '(0 1 2 4 8 3 12 5 10)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkJunctionSides"))))
  ;; Check the nick names
  (is (equal '("none" "corner-topleft" "corner-topright" "corner-bottomleft"
               "corner-bottomright" "top" "bottom" "left" "right")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkJunctionSides"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkJunctionSides"
                              GTK-JUNCTION-SIDES
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_junction_sides_get_type")
                              (:NONE 0)
                              (:CORNER-TOPLEFT 1)
                              (:CORNER-TOPRIGHT 2)
                              (:CORNER-BOTTOMLEFT 4)
                              (:CORNER-BOTTOMRIGHT 8)
                              (:TOP 3)
                              (:BOTTOM 12)
                              (:LEFT 5)
                              (:RIGHT 10))
             (get-g-type-definition "GtkJunctionSides"))))

;;;     GtkRegionFlags

(test gtk-region-flags
  ;; Check the type
  (is (g-type-is-flags "GtkRegionFlags"))
  ;; Check the registered name
  (is (eq 'gtk-region-flags
          (registered-flags-type "GtkRegionFlags")))
  ;; Check the names
  (is (equal '("GTK_REGION_EVEN" "GTK_REGION_ODD" "GTK_REGION_FIRST"
               "GTK_REGION_LAST" "GTK_REGION_ONLY" "GTK_REGION_SORTED")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkRegionFlags"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkRegionFlags"))))
  ;; Check the nick names
  (is (equal '("even" "odd" "first" "last" "only" "sorted")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkRegionFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkRegionFlags"
                              GTK-REGION-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_region_flags_get_type")
                              (:EVEN 1)
                              (:ODD 2)
                              (:FIRST 4)
                              (:LAST 8)
                              (:ONLY 16)
                              (:SORTED 32))
             (get-g-type-definition "GtkRegionFlags"))))

;;;     GtkStyleContextPrintFlags

(test gtk-style-context-print-flags
  ;; Check the type
  (is (g-type-is-flags "GtkStyleContextPrintFlags"))
  ;; Check the registered name
  (is (eq 'gtk-style-context-print-flags
          (registered-flags-type "GtkStyleContextPrintFlags")))
  ;; Check the names
  (is (equal '("GTK_STYLE_CONTEXT_PRINT_NONE" "GTK_STYLE_CONTEXT_PRINT_RECURSE"
               "GTK_STYLE_CONTEXT_PRINT_SHOW_STYLE")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkStyleContextPrintFlags"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkStyleContextPrintFlags"))))
  ;; Check the nick names
  (is (equal '("none" "recurse" "show-style")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkStyleContextPrintFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkStyleContextPrintFlags"
                              GTK-STYLE-CONTEXT-PRINT-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_style_context_print_flags_get_type")
                              (:NONE 0)
                              (:RECURSE 1)
                              (:SHOW-STYLE 2))
             (get-g-type-definition "GtkStyleContextPrintFlags"))))

;;;     GtkBorderStyle

(test gtk-border-style
  ;; Check the type
  (is (g-type-is-enum "GtkBorderStyle"))
  ;; Check the registered name
  (is (eq 'gtk-border-style (registered-enum-type "GtkBorderStyle")))
  ;; Check the names
  (is (equal '("GTK_BORDER_STYLE_NONE" "GTK_BORDER_STYLE_SOLID"
               "GTK_BORDER_STYLE_INSET" "GTK_BORDER_STYLE_OUTSET"
               "GTK_BORDER_STYLE_HIDDEN" "GTK_BORDER_STYLE_DOTTED"
               "GTK_BORDER_STYLE_DASHED" "GTK_BORDER_STYLE_DOUBLE"
               "GTK_BORDER_STYLE_GROOVE" "GTK_BORDER_STYLE_RIDGE")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkBorderStyle"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkBorderStyle"))))
  ;; Check the nick names
  (is (equal '("none" "solid" "inset" "outset" "hidden" "dotted" "dashed"
               "double" "groove" "ridge")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkBorderStyle"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkBorderStyle"
                             GTK-BORDER-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_border_style_get_type")
                             (:NONE 0)
                             (:SOLID 1)
                             (:INSET 2)
                             (:OUTSET 3)
                             (:HIDDEN 4)
                             (:DOTTED 5)
                             (:DASHED 6)
                             (:DOUBLE 7)
                             (:GROOVE 8)
                             (:RIDGE 9))
             (get-g-type-definition "GtkBorderStyle"))))

;;;     GtkStyleContext

(test gtk-style-context-class
  ;; Type check
  (is (g-type-is-object "GtkStyleContext"))
  ;; Check the registered name
  (is (eq 'gtk-style-context
          (registered-object-type-by-name "GtkStyleContext")))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GtkStyleContext")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkStyleContext"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkStyleContext"))))
  ;; Check the class properties
  (is (equal '("direction" "paint-clock" "parent" "screen")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GtkStyleContext"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkStyleContext" GTK-STYLE-CONTEXT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_style_context_get_type")
                       ((DIRECTION GTK-STYLE-CONTEXT-DIRECTION "direction"
                         "GtkTextDirection" T T)
                        (PAINT-CLOCK GTK-STYLE-CONTEXT-PAINT-CLOCK
                         "paint-clock" "GdkFrameClock" T T)
                        (PARENT GTK-STYLE-CONTEXT-PARENT "parent"
                         "GtkStyleContext" T T)
                        (SCREEN GTK-STYLE-CONTEXT-SCREEN "screen" "GdkScreen" T
                         T)))
             (get-g-type-definition "GtkStyleContext"))))

;;; --- Properties and Accessors -----------------------------------------------

;;;  GtkTextDirection    direction      Read / Write
;;;     GdkFrameClock*   paint-clock    Read / Write
;;;   GtkStyleContext*   parent         Read / Write
;;;         GdkScreen*   screen         Read / Write

(test gtk-style-context-properties
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-style-context widget)))
  ;; gtk-style-context-direction
  (is (eq :ltr (gtk-style-context-direction context)))
  (is (eq :rtl (setf (gtk-style-context-direction context) :rtl)))
  (is (eq :rtl (gtk-style-context-direction context)))
  ;; gtk-style-context-paint-clock
  (is-false (gtk-style-context-paint-clock context))
  ;; gtk-style-context-parent
  (is-false (gtk-style-context-parent context))
  (is (typep (setf (gtk-style-context-parent context)
                   (gtk-style-context-new))
             'gtk-style-context))
  (is (typep (gtk-style-context-parent context) 'gtk-style-context))
  ;; gtk-style-context-screen
  (is (typep (gtk-style-context-screen context) 'gdk-screen))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_style_context_new

(test gtk-style-context-new
  (is (typep (gtk-style-context-new) 'gtk-style-context)))

;;;     gtk_style_context_add_provider
;;;     gtk_style_context_remove_provider

(test gtk-style-context-add-provider
  (let ((context (gtk-style-context-new))
        (provider (make-instance 'gtk-css-provider)))
    (is-false (gtk-style-context-add-provider
                                            context
                                            provider
                                            +gtk-style-provider-priority-user+))
    (is-false (gtk-style-context-remove-provider context provider))))

;;;     gtk_style_context_add_provider_for_screen
;;;     gtk_style_context_remove_provider_for_screen

(test gtk-style-context-add-provider-for-screen
  (let ((screen (gdk-screen-default))
        (provider (make-instance 'gtk-css-provider)))
    (is-false (gtk-style-context-add-provider-for-screen
                                            screen
                                            provider
                                            +gtk-style-provider-priority-user+))
    (is-false (gtk-style-context-remove-provider-for-screen screen provider))))

;;;     gtk_style_context_get

;;;     gtk-style-context-junction-sides

(test gtk-style-context-junction-sides
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-style-context widget)))
    (is-false (gtk-style-context-junction-sides context))
    (is (eq :top (setf (gtk-style-context-junction-sides context) :top)))
    (is (equal '(:corner-topleft :corner-topright)
               (gtk-style-context-junction-sides context)))))

;;;     gtk-style-context-path

(test gtk-style-context-path
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-style-context widget))
         (path (gtk-widget-path widget)))
    (is (typep (gtk-style-context-path context) 'gtk-widget-path))
    (is (string= "button:dir-ltr"
                 (gtk-widget-path-to-string (gtk-style-context-path context))))
    (is (typep path 'gtk-widget-path))))

;;;     gtk-style-context-property

(test gtk-style-context-property.1
  (let ((context (gtk-style-context-new)))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value)
      (is-false (gtk::%gtk-style-context-property context "color" :normal value))
      (is-true value)
      (is (eq (gtype "GdkRGBA") (g-value-type value)))
      (is (string= "GdkRGBA" (g-value-type-name value)))
      (g-value-unset value))))

(test gtk-style-context-property.2
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-property context "color" :normal) 'gdk-rgba))
    (is-true (gdk-rgba-equal (gdk-rgba-new :red 1.0d0
                                           :green 1.0d0
                                           :blue 1.0d0
                                           :alpha 1.0d0)
                             (gtk-style-context-property context
                                                         "color" :normal)))
    (is (typep (gtk-style-context-property context "opacity" :normal)
               'double-float))
    (is (= 1.0d0 (gtk-style-context-property context "opacity" :normal)))
    (is (typep (gtk-style-context-property context "background-color" :normal)
                'gdk-rgba))
    (is-true (gdk-rgba-equal (gdk-rgba-new)
                             (gtk-style-context-property context
                                                         "background-color"
                                                         :normal)))
    (is (typep (gtk-style-context-property context "font" :normal)
               'pango-font-description))
    #-windows
    (is (string= "Ubuntu 11"
                 (pango-font-description-to-string
                     (gtk-style-context-property context "font" :normal))))
    #+windows
    (is (string= "Segoe UI 9"
                 (pango-font-description-to-string
                     (gtk-style-context-property context "font" :normal))))))

;;;     gtk_style_context_get_frame_clock
;;;     gtk_style_context_set_frame_clock

;;;     gtk-style-context-state

(test gtk-style-context-state
  (let ((context (gtk-style-context-new)))
    (is (equal '(:dir-ltr) (gtk-style-context-state context)))
    (is (eq :active (setf (gtk-style-context-state context) :active)))
    (is (equal '(:active) (gtk-style-context-state context)))
    (is (equal '(:active :dir-ltr)
               (setf (gtk-style-context-state context) '(:active :dir-ltr))))
    (is (equal '(:active :dir-ltr) (gtk-style-context-state context)))))

;;;     gtk_style_context_get_style

;;;     gtk-style-context-style-property

;; FIXME: The testsuite hangs executing this test

#-windows
(test gtk-style-context-style-property
  (let* ((message (make-instance 'gtk-message-dialog))
         (context (gtk-widget-style-context message)))
    (is (= 12
           (gtk-style-context-style-property context
                                             message
                                             "message-border")))))

;;;     gtk_style_context_get_style_valist
;;;     gtk_style_context_get_valist

;;;     gtk_style_context_get_section

;;;     gtk-style-context-color

(test gtk-style-context-color
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-color context :normal) 'gdk-rgba))
    (is-true (gdk-rgba-equal (gdk-rgba-new :red 1.0d0
                                           :green 1.0d0
                                           :blue 1.0d0
                                           :alpha 1.0d0)
                             (gtk-style-context-color context :normal)))))

;;;     gtk-style-context-background-color

(test gtk-style-context-background-color
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-background-color context :normal) 'gdk-rgba))
    (is-true (gdk-rgba-equal (gdk-rgba-new)
                             (gtk-style-context-background-color context
                                                                 :normal)))))

;;;     gtk-style-context-border-color

(test gtk-style-context-border-color
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-border-color context :normal) 'gdk-rgba))
    (is-true (gdk-rgba-equal (gdk-rgba-new :red 1.0d0
                                           :green 1.0d0
                                           :blue 1.0d0
                                           :alpha 1.0d0)
                             (gtk-style-context-border-color context :normal)))))

;;;     gtk-style-context-border

(test gtk-style-context-border
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-border context :normal) 'gtk-border))
    (is (= 0 (gtk-border-left (gtk-style-context-border context :normal))))
    (is (= 0 (gtk-border-right (gtk-style-context-border context :normal))))
    (is (= 0 (gtk-border-top (gtk-style-context-border context :normal))))
    (is (= 0 (gtk-border-bottom (gtk-style-context-border context :normal))))))

;;;     gtk-style-context-padding

(test gtk-style-context-padding
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-padding context :normal) 'gtk-border))
    (is (= 0 (gtk-border-left (gtk-style-context-padding context :normal))))
    (is (= 0 (gtk-border-right (gtk-style-context-padding context :normal))))
    (is (= 0 (gtk-border-top (gtk-style-context-padding context :normal))))
    (is (= 0 (gtk-border-bottom (gtk-style-context-padding context :normal))))))

;;;     gtk-style-context-margin

(test gtk-style-context-margin
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-margin context :normal) 'gtk-border))
    (is (= 0 (gtk-border-left (gtk-style-context-margin context :normal))))
    (is (= 0 (gtk-border-right (gtk-style-context-margin context :normal))))
    (is (= 0 (gtk-border-top (gtk-style-context-margin context :normal))))
    (is (= 0 (gtk-border-bottom (gtk-style-context-margin context :normal))))))

;;;     gtk-style-context-font

(test gtk-style-context-font
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-font context :normal) 'pango-font-description))
    #-windows
    (is (string= "Ubuntu 11"
                 (pango-font-description-to-string
                     (gtk-style-context-font context :normal))))
    #+windows
    (is (string= "Segoe UI 9"
                 (pango-font-description-to-string
                     (gtk-style-context-font context :normal))))))

;;;     gtk_style_context_invalidate
;;;     gtk_style_context_state_is_running

;;;     gtk_style_context_lookup_color

(test gtk-style-context-lookup-color
  (let ((context (gtk-style-context-new)))
    ;; We have no default color map
    (is-false (gtk-style-context-lookup-color context "Red"))
    (is-false (gtk-style-context-lookup-color context "Blue"))
    (is-false (gtk-style-context-lookup-color context "Green"))))

;;;     gtk_style_context_lookup_icon_set

(test gtk-style-context-icon-set
  (let ((context (gtk-style-context-new)))
    (is (typep (gtk-style-context-lookup-icon-set context "gtk-ok")
               'gtk-icon-set))))

;;;     gtk_style_context_notify_state_change
;;;     gtk_style_context_pop_animatable_region
;;;     gtk_style_context_push_animatable_region
;;;     gtk_style_context_cancel_animations
;;;     gtk_style_context_scroll_animations

;;;     gtk_style_context_reset_widgets
;;;     gtk_style_context_set_background
;;;     gtk_style_context_restore
;;;     gtk_style_context_save

;;;     gtk_style_context_add_class
;;;     gtk_style_context_remove_class
;;;     gtk_style_context_has_class
;;;     gtk_style_context_list_classes

(test gtk-style-context-add-class
  (let ((context (gtk-style-context-new)))
    (is-false (gtk-style-context-add-class context "entry"))
    (is-true (gtk-style-context-has-class context "entry"))
    (is-false (gtk-style-context-has-class context "button"))
    (is (equal '("entry") (gtk-style-context-list-classes context)))
    (is-false (gtk-style-context-remove-class context "entry"))
    (is (equal '() (gtk-style-context-list-classes context)))))

;;;     gtk_style_context_add_region
;;;     gtk_style_context_remove_region
;;;     gtk_style_context_has_region
;;;     gtk_style_context_list_regions

(test gtk-style-context-list-regions
  (let ((context (gtk-style-context-new)))
    (is-false (gtk-style-context-add-region context "row" :first))
    (is (equal '(:first) (gtk-style-context-has-region context "row")))
    (is-false (gtk-style-context-has-region context "column"))
    (is (equal '() (gtk-style-context-has-region context "column")))
    (is (equal '("row") (gtk-style-context-list-regions context)))
    (is-false (gtk-style-context-remove-region context "row"))
    (is (equal '() (gtk-style-context-list-regions context)))))

;;;     gtk-style-context-scale

(test gtk-style-context-scale
  (let ((context (gtk-style-context-new)))
    (is (=  1 (gtk-style-context-scale context)))
    (is (= 10 (setf (gtk-style-context-scale context) 10)))
    (is (= 10 (gtk-style-context-scale context)))))

;;;     gtk_style_context_to_string

#-windows
(test gtk-style-context-to-string
  (let* ((window (make-instance 'gtk-message-dialog))
         (context (gtk-widget-style-context window)))
    (is-true (stringp (gtk-style-context-to-string context :recurse)))
    (is-true (string= (gtk-style-context-to-string context :recurse)
"[messagedialog.background.csd:dir(ltr)]
  decoration:dir(ltr)
  box.vertical.dialog-vbox:dir(ltr)
    box.horizontal:dir(ltr)
      image:dir(ltr)
      box.vertical:dir(ltr)
        label:dir(ltr)
        [label:dir(ltr)]
    box.horizontal.dialog-action-box:dir(ltr)
      buttonbox.linked.horizontal.dialog-action-area:dir(ltr)
  box.titlebar.horizontal:dir(ltr)
    [label.title:dir(ltr)]
"))
    (is-true (stringp (gtk-style-context-to-string context :show-style)))
    (is-true (string= (gtk-style-context-to-string context :show-style)
"[messagedialog.background.csd:dir(ltr)]
"))
    (is-true (stringp (gtk-style-context-to-string context :none)))
    (is-true (string= (gtk-style-context-to-string context :none)
"[messagedialog.background.csd:dir(ltr)]
"))))

#+windows
(test gtk-style-context-to-string
  (let* ((window (make-instance 'gtk-message-dialog))
         (context (gtk-widget-style-context window)))
    (is-true (stringp (gtk-style-context-to-string context :recurse)))
    (is-true (stringp (gtk-style-context-to-string context :show-style)))
    (is-true (stringp (gtk-style-context-to-string context :none)))))

;;;     GtkBorder

(test gtk-border
  ;; Type check
  (is (g-type-is-a (gtype "GtkBorder") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GtkBorder")
          (gtype (foreign-funcall "gtk_border_get_type" g-size)))))

;;;     gtk_border_new
;;;     gtk_border_copy
;;;     gtk_border_free

;;;     gtk_render_arrow
;;;     gtk_render_background
;;;     gtk_render_background_get_clip
;;;     gtk_render_check
;;;     gtk_render_expander
;;;     gtk_render_extension
;;;     gtk_render_focus
;;;     gtk_render_frame
;;;     gtk_render_frame_gap
;;;     gtk_render_handle
;;;     gtk_render_layout
;;;     gtk_render_line
;;;     gtk_render_option
;;;     gtk_render_slider
;;;     gtk_render_activity
;;;     gtk_render_icon_pixbuf
;;;     gtk_render_icon_surface
;;;     gtk_render_icon
;;;     gtk_render_insertion_cursor

;;; 2021-10-14
