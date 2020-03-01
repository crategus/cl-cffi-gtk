(def-suite gtk-style-context :in gtk-suite)
(in-suite gtk-style-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStyleContext

(test gtk-style-context-class
  ;; Type check
  (is-true  (g-type-is-object "GtkStyleContext"))
  ;; Check the registered name
  (is (eq 'gtk-style-context
          (registered-object-type-by-name "GtkStyleContext")))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkStyleContext")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkStyleContext"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkStyleContext"))))
  ;; Check the class properties
  (is (equal '("direction" "paint-clock" "parent" "screen")
             (stable-sort (mapcar #'param-spec-name
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
         (context (gtk-widget-get-style-context widget)))
  ;; gtk-style-context-direction
  (is (eq :ltr (gtk-style-context-direction context)))
  (is (eq :rtl (setf (gtk-style-context-direction context) :rtl)))
  (is (eq :rtl (gtk-style-context-direction context)))
  ;; gtk-style-context-paint-clock
  (is-false (gtk-style-context-paint-clock context))
; TODO: We cannot create an instance of a frame clock.
;  (setf (gtk-style-context-paint-clock context) (make-instance 'gdk-frame-clock))
  ;; gtk-style-context-parent
  (is-false (gtk-style-context-parent context))
  (is (eq 'gtk-style-context
          (type-of (setf (gtk-style-context-parent context) (make-instance 'gtk-style-context)))))
  (is (eq 'gtk-style-context (type-of (gtk-style-context-parent context))))
  ;; gtk-style-context-screen
  (is (eq 'gdk-screen (type-of (gtk-style-context-screen context))))
; TODO: This causes a unhandled memory fault.
;  (setf (gtk-style-context-screen context) (make-instance 'gdk-screen))
;  (is-false (type-of (gtk-style-context-screen context)))
  ))

;;;     GtkJunctionSides

(test gtk-junction-sides
  ;; Check the type
  (is-true (g-type-is-flags "GtkJunctionSides"))
  ;; Check the registered name
  (is (eql 'gtk-junction-sides
           (gobject::registered-flags-type "GtkJunctionSides")))
  ;; Check the names
  (is (equal '("GTK_JUNCTION_NONE" "GTK_JUNCTION_CORNER_TOPLEFT"
               "GTK_JUNCTION_CORNER_TOPRIGHT" "GTK_JUNCTION_CORNER_BOTTOMLEFT"
               "GTK_JUNCTION_CORNER_BOTTOMRIGHT" "GTK_JUNCTION_TOP" "GTK_JUNCTION_BOTTOM"
               "GTK_JUNCTION_LEFT" "GTK_JUNCTION_RIGHT")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GtkJunctionSides"))))
  ;; Check the values
  (is (equal '(0 1 2 4 8 3 12 5 10)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GtkJunctionSides"))))
  ;; Check the nick names
  (is (equal '("none" "corner-topleft" "corner-topright" "corner-bottomleft"
               "corner-bottomright" "top" "bottom" "left" "right")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GtkJunctionSides"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkJunctionSides"
                              GTK-JUNCTION-SIDES
                              (:EXPORT T :TYPE-INITIALIZER "gtk_junction_sides_get_type")
                              (:NONE 0)
                              (:CORNER-TOPLEFT 1)
                              (:CORNER-TOPRIGHT 2)
                              (:CORNER-BOTTOMLEFT 4)
                              (:CORNER-BOTTOMRIGHT 8)
                              (:TOP 3)
                              (:BOTTOM 12)
                              (:LEFT 5)
                              (:RIGHT 10))
             (gobject::get-g-type-definition "GtkJunctionSides"))))

;;;     GtkRegionFlags

(test gtk-region-flags
  ;; Check the type
  (is-true (g-type-is-flags "GtkRegionFlags"))
  ;; Check the registered name
  (is (eql 'gtk-region-flags
           (gobject::registered-flags-type "GtkRegionFlags")))
  ;; Check the names
  (is (equal '("GTK_REGION_EVEN" "GTK_REGION_ODD" "GTK_REGION_FIRST" "GTK_REGION_LAST"
               "GTK_REGION_ONLY" "GTK_REGION_SORTED")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GtkRegionFlags"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GtkRegionFlags"))))
  ;; Check the nick names
  (is (equal '("even" "odd" "first" "last" "only" "sorted")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GtkRegionFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkRegionFlags"
                              GTK-REGION-FLAGS
                              (:EXPORT T :TYPE-INITIALIZER "gtk_region_flags_get_type")
                              (:EVEN 1)
                              (:ODD 2)
                              (:FIRST 4)
                              (:LAST 8)
                              (:ONLY 16)
                              (:SORTED 32))
             (gobject::get-g-type-definition "GtkRegionFlags"))))

;;;     GtkStyleContextPrintFlags

(test gtk-style-context-print-flags
  ;; Check the type
  (is-true (g-type-is-flags "GtkStyleContextPrintFlags"))
  ;; Check the registered name
  (is (eql 'gtk-style-context-print-flags
           (gobject::registered-flags-type "GtkStyleContextPrintFlags")))
  ;; Check the names
  (is (equal '("GTK_STYLE_CONTEXT_PRINT_NONE" "GTK_STYLE_CONTEXT_PRINT_RECURSE"
               "GTK_STYLE_CONTEXT_PRINT_SHOW_STYLE")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GtkStyleContextPrintFlags"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GtkStyleContextPrintFlags"))))
  ;; Check the nick names
  (is (equal '("none" "recurse" "show-style")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GtkStyleContextPrintFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkStyleContextPrintFlags"
                              GTK-STYLE-CONTEXT-PRINT-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_style_context_print_flags_get_type")
                              (:NONE 0)
                              (:RECURSE 1)
                              (:SHOW-STYLE 2))
             (gobject::get-g-type-definition "GtkStyleContextPrintFlags"))))

;;;     GtkBorderStyle

(test gtk-border-style
  ;; Check the type
  (is-true (g-type-is-enum "GtkBorderStyle"))
  ;; Check the registered name
  (is (eql 'gtk-border-style (gobject::registered-enum-type "GtkBorderStyle")))
  ;; Check the names
  (is (equal '("GTK_BORDER_STYLE_NONE" "GTK_BORDER_STYLE_SOLID" "GTK_BORDER_STYLE_INSET"
               "GTK_BORDER_STYLE_OUTSET" "GTK_BORDER_STYLE_HIDDEN" "GTK_BORDER_STYLE_DOTTED"
               "GTK_BORDER_STYLE_DASHED" "GTK_BORDER_STYLE_DOUBLE" "GTK_BORDER_STYLE_GROOVE"
               "GTK_BORDER_STYLE_RIDGE")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkBorderStyle"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkBorderStyle"))))
  ;; Check the nick names
  (is (equal '("none" "solid" "inset" "outset" "hidden" "dotted" "dashed" "double" "groove"
               "ridge")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkBorderStyle"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkBorderStyle"
                             GTK-BORDER-STYLE
                             (:EXPORT T :TYPE-INITIALIZER "gtk_border_style_get_type")
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
             (gobject::get-g-type-definition "GtkBorderStyle"))))

;;;     GtkBorder

(test gtk-border-struct
 (is-true (gtype "GtkBorder"))
 (is-true (gobject::get-g-boxed-foreign-info 'gtk-border)))

;;;     GTK_STYLE_PROPERTY_BACKGROUND_COLOR
;;;     GTK_STYLE_PROPERTY_COLOR
;;;     GTK_STYLE_PROPERTY_FONT
;;;     GTK_STYLE_PROPERTY_MARGIN
;;;     GTK_STYLE_PROPERTY_PADDING
;;;     GTK_STYLE_PROPERTY_BORDER_WIDTH
;;;     GTK_STYLE_PROPERTY_BORDER_RADIUS
;;;     GTK_STYLE_PROPERTY_BORDER_STYLE
;;;     GTK_STYLE_PROPERTY_BORDER_COLOR
;;;     GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
;;;
;;;     GTK_STYLE_CLASS_ACCELERATOR
;;;     GTK_STYLE_CLASS_ARROW
;;;     GTK_STYLE_CLASS_BACKGROUND
;;;     GTK_STYLE_CLASS_BOTTOM
;;;     GTK_STYLE_CLASS_BUTTON
;;;     GTK_STYLE_CLASS_CALENDAR
;;;     GTK_STYLE_CLASS_CELL
;;;     GTK_STYLE_CLASS_COMBOBOX_ENTRY
;;;     GTK_STYLE_CLASS_CONTEXT_MENU
;;;     GTK_STYLE_CLASS_CHECK
;;;     GTK_STYLE_CLASS_CSD
;;;     GTK_STYLE_CLASS_CURSOR_HANDLE
;;;     GTK_STYLE_CLASS_DEFAULT
;;;     GTK_STYLE_CLASS_DESTRUCTIVE_ACTION
;;;     GTK_STYLE_CLASS_DIM_LABEL
;;;     GTK_STYLE_CLASS_DND
;;;     GTK_STYLE_CLASS_DOCK
;;;     GTK_STYLE_CLASS_ENTRY
;;;     GTK_STYLE_CLASS_ERROR
;;;     GTK_STYLE_CLASS_EXPANDER
;;;     GTK_STYLE_CLASS_FRAME
;;;     GTK_STYLE_CLASS_FLAT
;;;     GTK_STYLE_CLASS_GRIP
;;;     GTK_STYLE_CLASS_HEADER
;;;     GTK_STYLE_CLASS_HIGHLIGHT
;;;     GTK_STYLE_CLASS_HORIZONTAL
;;;     GTK_STYLE_CLASS_IMAGE
;;;     GTK_STYLE_CLASS_INFO
;;;     GTK_STYLE_CLASS_INLINE_TOOLBAR
;;;     GTK_STYLE_CLASS_INSERTION_CURSOR
;;;     GTK_STYLE_CLASS_LABEL
;;;     GTK_STYLE_CLASS_LEFT
;;;     GTK_STYLE_CLASS_LEVEL_BAR
;;;     GTK_STYLE_CLASS_LINKED
;;;     GTK_STYLE_CLASS_LIST
;;;     GTK_STYLE_CLASS_LIST_ROW
;;;     GTK_STYLE_CLASS_MARK
;;;     GTK_STYLE_CLASS_MENU
;;;     GTK_STYLE_CLASS_MENUBAR
;;;     GTK_STYLE_CLASS_MENUITEM
;;;     GTK_STYLE_CLASS_MESSAGE_DIALOG
;;;     GTK_STYLE_CLASS_MONOSPACE
;;;     GTK_STYLE_CLASS_NEEDS_ATTENTION
;;;     GTK_STYLE_CLASS_NOTEBOOK
;;;     GTK_STYLE_CLASS_OSD
;;;     GTK_STYLE_CLASS_OVERSHOOT
;;;     GTK_STYLE_CLASS_PANE_SEPARATOR
;;;     GTK_STYLE_CLASS_PAPER
;;;     GTK_STYLE_CLASS_POPUP
;;;     GTK_STYLE_CLASS_POPOVER
;;;     GTK_STYLE_CLASS_PRIMARY_TOOLBAR
;;;     GTK_STYLE_CLASS_PROGRESSBAR
;;;     GTK_STYLE_CLASS_PULSE
;;;     GTK_STYLE_CLASS_QUESTION
;;;     GTK_STYLE_CLASS_RADIO
;;;     GTK_STYLE_CLASS_RAISED
;;;     GTK_STYLE_CLASS_READ_ONLY
;;;     GTK_STYLE_CLASS_RIGHT
;;;     GTK_STYLE_CLASS_RUBBERBAND
;;;     GTK_STYLE_CLASS_SCALE
;;;     GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
;;;     GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW
;;;     GTK_STYLE_CLASS_SCROLLBAR
;;;     GTK_STYLE_CLASS_SCROLLBARS_JUNCTION
;;;     GTK_STYLE_CLASS_SEPARATOR
;;;     GTK_STYLE_CLASS_SIDEBAR
;;;     GTK_STYLE_CLASS_SLIDER
;;;     GTK_STYLE_CLASS_SPINBUTTON
;;;     GTK_STYLE_CLASS_SPINNER
;;;     GTK_STYLE_CLASS_STATUSBAR
;;;     GTK_STYLE_CLASS_SUBTITLE
;;;     GTK_STYLE_CLASS_SUGGESTED_ACTION
;;;     GTK_STYLE_CLASS_TITLE
;;;     GTK_STYLE_CLASS_TITLEBAR
;;;     GTK_STYLE_CLASS_TOOLBAR
;;;     GTK_STYLE_CLASS_TOOLTIP
;;;     GTK_STYLE_CLASS_TOUCH_SELECTION
;;;     GTK_STYLE_CLASS_TOP
;;;     GTK_STYLE_CLASS_TROUGH
;;;     GTK_STYLE_CLASS_UNDERSHOOT
;;;     GTK_STYLE_CLASS_VERTICAL
;;;     GTK_STYLE_CLASS_VIEW
;;;     GTK_STYLE_CLASS_WARNING
;;;     GTK_STYLE_CLASS_WIDE
;;;
;;;     GTK_STYLE_REGION_COLUMN
;;;     GTK_STYLE_REGION_COLUMN_HEADER
;;;     GTK_STYLE_REGION_ROW
;;;     GTK_STYLE_REGION_TAB

;;; --- Functions --------------------------------------------------------------

;;;     gtk_style_context_new

(test gtk-style-context-new
  (is (eq 'gtk-style-context (type-of (gtk-style-context-new)))))

;;;     gtk_style_context_add_provider
;;;     gtk_style_context_remove_provider

(test gtk-style-context-add-provider
  (let ((context (make-instance 'gtk-style-context))
        (provider (make-instance 'gtk-css-provider)))
    (is-false (gtk-style-context-add-provider context provider +gtk-style-provider-priority-user+))
    (is-false (gtk-style-context-remove-provider context provider))))

;;;     gtk_style_context_add_provider_for_screen
;;;     gtk_style_context_remove_provider_for_screen

(test gtk-style-context-add-provider-for-screen
  (let ((screen (gdk-screen-get-default))
        (provider (make-instance 'gtk-css-provider)))
    (is-false (gtk-style-context-add-provider-for-screen screen provider +gtk-style-provider-priority-user+))
    (is-false (gtk-style-context-remove-provider-for-screen screen provider))))

;;;     gtk_style_context_get

;;;     gtk_style_context_get_junction_sides
;;;     gtk_style_context_set_junction_sides

(test gtk-style-context-get-junction-sides
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-get-style-context widget)))
    (is-false (gtk-style-context-get-junction-sides context))
    (is-false (gtk-style-context-set-junction-sides context :top))
    (is (equal '(:corner-topleft :corner-topright) (gtk-style-context-get-junction-sides context)))))

;;;     gtk_style_context_get_path
;;;     gtk_style_context_set_path

(test gtk-style-context-get-path
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-get-style-context widget))
         (path (gtk-widget-get-path widget)))
    (is (eq 'gtk-widget-path (type-of (gtk-style-context-get-path context))))
    (is (string= "button:dir-ltr" (gtk-widget-path-to-string (gtk-style-context-get-path context))))
    (is (eq 'gtk-widget-path (type-of path)))
    ;; TODO: Find an example
;    (is-false (gtk-style-context-set-path context path))
  ))

;;;     gtk_style_context_get_property

(test gtk-style-context-get-property
  (let ((context (gtk-style-context-new)))
    (with-foreign-object (value '(:struct g-value))
      (g-value-zero value)
      (is-false (gtk::%gtk-style-context-get-property context "color" :normal value))
      (is-true value)
      (is (equal (gtype "GdkRGBA") (g-value-type value)))
      (is (string= "GdkRGBA" (g-value-type-name value)))
      (g-value-unset value)))
  (let ((context (gtk-style-context-new)))
    (is (eq 'gdk-rgba
            (type-of (gtk-style-context-get-property context "color" :normal))))
    (is-true (gdk-rgba-equal (make-gdk-rgba :red 1.0d0 :green 1.0d0 :blue 1.0d0  :alpha 1.0d0)
                             (gtk-style-context-get-property context "color" :normal)))
    (is (eq 'double-float
            (type-of (gtk-style-context-get-property context "opacity" :normal))))
    (is (= 1.0d0 (gtk-style-context-get-property context "opacity" :normal)))
    (is (eq 'gdk-rgba
            (type-of (gtk-style-context-get-property context "background-color" :normal))))
    (is-true (gdk-rgba-equal (make-gdk-rgba)
                             (gtk-style-context-get-property context "background-color" :normal)))
    (is (eq 'pango-font-description
            (type-of (gtk-style-context-get-property context "font" :normal))))
    (is (string= "Ubuntu 11"
                 (pango-font-description-to-string (gtk-style-context-get-property context "font" :normal))))))

;;;     gtk_style_context_get_frame_clock
;;;     gtk_style_context_set_frame_clock

;;;     gtk_style_context_get_state
;;;     gtk_style_context_set_state

(test gtk-style-context-state
  (let ((context (gtk-style-context-new)))
    (is (equal '(:dir-ltr) (gtk-style-context-get-state context)))
    (is-false (gtk-style-context-set-state context :active))
    (is (equal '(:active) (gtk-style-context-get-state context)))
    (is-false (gtk-style-context-set-state context '(:active :dir-ltr)))
    (is (equal '(:active :dir-ltr) (gtk-style-context-get-state context)))))

;;;     gtk_style_context_get_style

;;;     gtk_style_context_get_style_property

(test gtk-style-context-get-style-property
  (let* ((message (make-instance 'gtk-message-dialog))
         (context (gtk-widget-get-style-context message)))
    (is (= 12 (gtk-style-context-get-style-property context message "message-border")))))

;;;     gtk_style_context_get_style_valist
;;;     gtk_style_context_get_valist
;;;     gtk_style_context_get_section

;;;     gtk_style_context_get_color

(test gtk-style-context-get-color
  (let ((context (gtk-style-context-new)))
    (is (eq 'gdk-rgba (type-of (gtk-style-context-get-color context :normal))))
    (is-true (gdk-rgba-equal (make-gdk-rgba :red 1.0d0 :green 1.0d0 :blue 1.0d0 :alpha 1.0d0)
                             (gtk-style-context-get-color context :normal)))))

;;;     gtk_style_context_get_background_color

(test gtk-style-context-get-background-color
  (let ((context (gtk-style-context-new)))
    (is (eq 'gdk-rgba (type-of (gtk-style-context-get-background-color context :normal))))
    (is-true (gdk-rgba-equal (make-gdk-rgba)
                             (gtk-style-context-get-background-color context :normal)))))

;;;     gtk_style_context_get_border_color

(test gtk-style-context-get-border-color
  (let ((context (gtk-style-context-new)))
    (is (eq 'gdk-rgba (type-of (gtk-style-context-get-border-color context :normal))))
    (is-true (gdk-rgba-equal (make-gdk-rgba :red 1.0d0 :green 1.0d0 :blue 1.0d0 :alpha 1.0d0)
                             (gtk-style-context-get-border-color context :normal)))))

;;;     gtk_style_context_get_border

(test gtk-style-context-get-border
  (let ((context (gtk-style-context-new)))
    (is (eq 'gtk-border (type-of (gtk-style-context-get-border context :normal))))
    (is (= 0 (gtk-border-left (gtk-style-context-get-border context :normal))))
    (is (= 0 (gtk-border-right (gtk-style-context-get-border context :normal))))
    (is (= 0 (gtk-border-top (gtk-style-context-get-border context :normal))))
    (is (= 0 (gtk-border-bottom (gtk-style-context-get-border context :normal))))))

;;;     gtk_style_context_get_padding

(test gtk-style-context-get-padding
  (let ((context (gtk-style-context-new)))
    (is (eq 'gtk-border (type-of (gtk-style-context-get-padding context :normal))))
    (is (= 0 (gtk-border-left (gtk-style-context-get-padding context :normal))))
    (is (= 0 (gtk-border-right (gtk-style-context-get-padding context :normal))))
    (is (= 0 (gtk-border-top (gtk-style-context-get-padding context :normal))))
    (is (= 0 (gtk-border-bottom (gtk-style-context-get-padding context :normal))))))

;;;     gtk_style_context_get_margin

(test gtk-style-context-get-margin
  (let ((context (gtk-style-context-new)))
    (is (eq 'gtk-border (type-of (gtk-style-context-get-margin context :normal))))
    (is (= 0 (gtk-border-left (gtk-style-context-get-margin context :normal))))
    (is (= 0 (gtk-border-right (gtk-style-context-get-margin context :normal))))
    (is (= 0 (gtk-border-top (gtk-style-context-get-margin context :normal))))
    (is (= 0 (gtk-border-bottom (gtk-style-context-get-margin context :normal))))))

;;;     gtk_style_context_get_font

(test gtk-style-context-get-font
  (let ((context (gtk-style-context-new)))
    (is (eq 'pango-font-description (type-of (gtk-style-context-get-font context :normal))))
    (is (string= "Ubuntu 11"
                 (pango-font-description-to-string (gtk-style-context-get-font context :normal))))))

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
    (is (eq 'gtk-icon-set (type-of (gtk-style-context-lookup-icon-set context "gtk-ok"))))))

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

;;;     gtk_style_context_set_scale
;;;     gtk_style_context_get_scale

(test gtk-style-context-set-scale
  (let ((context (gtk-style-context-new)))
    (is (= 1 (gtk-style-context-get-scale context)))
    (is-false (gtk-style-context-set-scale context 10))
    (is (= 10 (gtk-style-context-get-scale context)))))

;;;     gtk_style_context_to_string

(test gtk-style-context-to-string
  (let* ((window (make-instance 'gtk-message-dialog))
         (context (gtk-widget-get-style-context window)))
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

;;;     GtkBorder

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

