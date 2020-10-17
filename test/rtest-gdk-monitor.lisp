(def-suite gdk-monitor :in gdk-suite)
(in-suite gdk-monitor)

;;;     GdkSubpixelLayout

(test gdk-subpixel-layout
  ;; Check the type
  (is-true (g-type-is-enum "GdkSubpixelLayout"))
  ;; Check the registered name
  (is (eql 'gdk-subpixel-layout (gobject::registered-enum-type "GdkSubpixelLayout")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkSubpixelLayout"
    GDK-SUBPIXEL-LAYOUT
    (:EXPORT T :TYPE-INITIALIZER "gdk_subpixel_layout_get_type")
  (:UNKNOWN 0)
  (:NONE 1)
  (:HORIZONTAL-RGB 2)
  (:HORIZONTAL-BGR 3)
  (:VERTICAL-RGB 4)
  (:VERTICAL-BGR 5))
             (gobject::get-g-type-definition "GdkSubpixelLayout")))
  ;; Check the names
  (is (equal '("GDK_SUBPIXEL_LAYOUT_UNKNOWN" "GDK_SUBPIXEL_LAYOUT_NONE"
 "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB" "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR"
 "GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB" "GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GdkSubpixelLayout"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GdkSubpixelLayout"))))
  ;; Check the nick names
  (is (equal '("unknown" "none" "horizontal-rgb" "horizontal-bgr" "vertical-rgb"
 "vertical-bgr")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GdkSubpixelLayout"))))
)

;;;     GdkMonitor

(test gdk-monitor-class
  ;; Type check
  (is-true  (g-type-is-object "GdkMonitor"))
  ;; Check the registered name
  (is (eq 'gdk-monitor
          (registered-object-type-by-name "GdkMonitor")))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GdkMonitor")))
  ;; Check the children
  (is (equal '("GdkX11Monitor" "GdkBroadwayMonitor")
             (mapcar #'gtype-name (g-type-children "GdkMonitor"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkMonitor"))))
  ;; Check the class properties
  (is (equal '("display" "geometry" "height-mm" "manufacturer" "model" "refresh-rate"
 "scale-factor" "subpixel-layout" "width-mm" "workarea")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GdkMonitor"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkMonitor" GDK-MONITOR
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_monitor_get_type")
                       ((DISPLAY GDK-MONITOR-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (GEOMETRY GDK-MONITOR-GEOMETRY "geometry"
                         "GdkRectangle" T NIL)
                        (HEIGHT-MM GDK-MONITOR-HEIGHT-MM "height-mm" "gint" T
                         NIL)
                        (MANUFACTURER GDK-MONITOR-MANUFACTURER "manufacturer"
                         "gchararray" T NIL)
                        (MODEL GDK-MONITOR-MODEL "model" "gchararray" T NIL)
                        (REFRESH-RATE GDK-MONITOR-REFRESH-RATE "refresh-rate"
                         "gint" T NIL)
                        (SCALE-FACTOR GDK-MONITOR-SCALE-FACTOR "scale-factor"
                         "gint" T NIL)
                        (SUBPIXEL-LAYOUT GDK-MONITOR-SUBPIXEL-LAYOUT
                         "subpixel-layout" "GdkSubpixelLayout" T NIL)
                        (WIDTH-MM GDK-MONITOR-WIDTH-MM "width-mm" "gint" T NIL)
                        (WORKAREA GDK-MONITOR-WORKAREA "workarea"
                         "GdkRectangle" T NIL)))
             (get-g-type-definition "GdkMonitor")))
)

(test gdk-monitor-properties
  (let ((monitor (gdk-display-get-primary-monitor (gdk-display-default))))
    ;; gdk-monitor-get-display
    (is (eq 'gdk-display
            (type-of (gdk-monitor-display monitor))))
    ;; gdk-monitor-geometry
    (is-true (gdk-rectangle-equal (make-gdk-rectangle :x 0
                                                      :y 0
                                                      :width 1366
                                                      :height 768)
                                  (gdk-monitor-geometry monitor)))
    ;; gdk-monitor-workarea
    (is-true (gdk-rectangle-equal (make-gdk-rectangle :x 0
                                                      :y 27
                                                      :width 1366
                                                      :height 741)
                                  (gdk-monitor-workarea monitor)))
    ;; gdk-monitor-width-mm
    (is (= 344 (gdk-monitor-width-mm monitor)))
    ;; gdk-monitor-height-mm
    (is (= 193 (gdk-monitor-height-mm monitor)))
    ;; gdk-monitor-manufacturer
    (is (string= "AUO" (gdk-monitor-manufacturer monitor)))
    ;; gdk-monitor-model
    (is (string= "eDP-1" (gdk-monitor-model monitor)))
    ;; gdk-monitor-scale-factor
    (is (= 1 (gdk-monitor-scale-factor monitor)))
    ;; gdk-monitor-refresh-rate
    (is (= 60059 (gdk-monitor-refresh-rate monitor)))
    ;; gdk-monitor-subpixel-layout
    (is (eq :unknown (gdk-monitor-subpixel-layout monitor)))))

;;;     gdk_monitor_is_primary

(test gdk-monitor-property
  (let ((monitor (gdk-display-get-primary-monitor (gdk-display-default))))
    (is-true (gdk-monitor-is-primary monitor))))

