(def-suite gdk-monitor :in gdk-suite)
(in-suite gdk-monitor)


;;;     GdkSubpixelLayout

(test gdk-subpixel-layout
  ;; Type checks
  (is-false (g-type-is-object "GdkSubpixelLayout"))
  (is-false (g-type-is-abstract "GdkSubpixelLayout"))
  (is-true  (g-type-is-derived "GdkSubpixelLayout"))
  (is-false (g-type-is-fundamental "GdkSubpixelLayout"))
  (is-true  (g-type-is-value-type "GdkSubpixelLayout"))
  (is-true  (g-type-has-value-table "GdkSubpixelLayout"))
  (is-true  (g-type-is-classed "GdkSubpixelLayout"))
  (is-false (g-type-is-instantiatable "GdkSubpixelLayout"))
  (is-true  (g-type-is-derivable "GdkSubpixelLayout"))
  (is-false (g-type-is-deep-derivable "GdkSubpixelLayout"))
  (is-false (g-type-is-interface "GdkSubpixelLayout"))

  ;; Check the registered name
  ;; no registered name

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkSubpixelLayout"))))
    (is (equal (gtype "GdkSubpixelLayout") (g-type-from-class class)))
    (is (equal (gtype "GdkSubpixelLayout") (g-object-class-type class)))
    (is (equal "GdkSubpixelLayout" (g-object-class-name class)))
    (is (equal (gtype "GdkSubpixelLayout")
               (g-type-from-class  (g-type-class-peek "GdkSubpixelLayout"))))
    (is (equal (gtype "GdkSubpixelLayout")
               (g-type-from-class  (g-type-class-peek-static "GdkSubpixelLayout"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  ;; no Lisp class implementation

  ;; Check some more GType information
  (is (equal (gtype "GEnum") (g-type-parent "GdkSubpixelLayout")))
  (is (= 2 (g-type-depth "GdkSubpixelLayout")))
  (is (equal (gtype "GdkSubpixelLayout")
             (g-type-next-base "GdkSubpixelLayout" "GEnum")))
  (is-false (g-type-is-a "GdkSubpixelLayout" "GtkWidget"))
  (is-true  (g-type-is-a "GdkSubpixelLayout" "GEnum"))
  (is-false (g-type-is-a "GdkSubpixelLayout" "GFlags"))
  (is-false (g-type-is-a "GdkSubpixelLayout" "gboolean"))
  (is-false (g-type-is-a "GdkSubpixelLayout" "GtkWindow"))

  ;; Check the children
  ;; no children
  ;; Check the interfaces
  ;; no interfaces

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkSubpixelLayout" query)
    (is (equal (gtype "GdkSubpixelLayout")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkSubpixelLayout"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 32  (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  0  (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  ;; no class properties for an Enum

  ;; Get the names of the style properties.
  ;; no style properties for an Enum

  ;; Get the names to the child properties
  ;; no child properties for an Enum

  ;; Get the class definition
  (is (equal '(DEFINE-G-ENUM "GdkSubpixelLayout"
    GDK-SUBPIXEL-LAYOUT
    (:EXPORT T :TYPE-INITIALIZER "gdk_subpixel_layout_get_type")
  (:UNKNOWN 0)
  (:NONE 1)
  (:HORIZONTAL-RGB 2)
  (:HORIZONTAL-BGR 3)
  (:VERTICAL-RGB 4)
  (:VERTICAL-BGR 5))
             (get-g-type-definition "GdkSubpixelLayout"))))

(test gdk-subpixel-layout-value
  (is (=  0 (foreign-enum-value 'gdk-subpixel-layout :unknown)))
  (is (=  1 (foreign-enum-value 'gdk-subpixel-layout :none)))
  (is (=  2 (foreign-enum-value 'gdk-subpixel-layout :horizontal-rgb)))
  (is (=  3 (foreign-enum-value 'gdk-subpixel-layout :horizontal-bgr)))
  (is (=  4 (foreign-enum-value 'gdk-subpixel-layout :vertical-rgb)))
  (is (=  5 (foreign-enum-value 'gdk-subpixel-layout :vertical-bgr))))

(test gdk-subpixel-layout-keyword
  (is (eq :unknown (foreign-enum-keyword 'gdk-subpixel-layout 0)))
  (is (eq :none (foreign-enum-keyword 'gdk-subpixel-layout 1)))
  (is (eq :horizontal-rgb (foreign-enum-keyword 'gdk-subpixel-layout 2)))
  (is (eq :horizontal-bgr (foreign-enum-keyword 'gdk-subpixel-layout 3)))
  (is (eq :vertical-rgb (foreign-enum-keyword 'gdk-subpixel-layout 4)))
  (is (eq :vertical-bgr (foreign-enum-keyword 'gdk-subpixel-layout 5))))

;;;     GdkMonitor

(test gdk-monitor-class
  ;; Type checks
  (is-true  (g-type-is-object "GdkMonitor"))
  ;; GdkSeat is abstract, we have the child class GdkSeatDefault
  (is-false (g-type-is-abstract "GdkMonitor"))
  (is-true  (g-type-is-derived "GdkMonitor"))
  (is-false (g-type-is-fundamental "GdkMonitor"))
  (is-true  (g-type-is-value-type "GdkMonitor"))
  (is-true  (g-type-has-value-table "GdkMonitor"))
  (is-true  (g-type-is-classed "GdkMonitor"))
  (is-true  (g-type-is-instantiatable "GdkMonitor"))
  (is-true  (g-type-is-derivable "GdkMonitor"))
  (is-true  (g-type-is-deep-derivable "GdkMonitor"))
  (is-false (g-type-is-interface "GdkMonitor"))

  ;; Check the registered name
  (is (eq 'gdk-monitor
          (registered-object-type-by-name "GdkMonitor")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkMonitor"))))
    (is (equal (gtype "GdkMonitor") (g-type-from-class class)))
    (is (equal (gtype "GdkMonitor") (g-object-class-type class)))
    (is (equal "GdkMonitor" (g-object-class-name class)))
    (is (equal (gtype "GdkMonitor")
               (g-type-from-class  (g-type-class-peek "GdkMonitor"))))
    (is (equal (gtype "GdkMonitor")
               (g-type-from-class (g-type-class-peek-static "GdkMonitor"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gdk-monitor)))
    ;; Check the class name and type of the class
    (is (eq 'gdk-monitor (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GdkMonitor" (gobject-class-g-type-name class)))
    (is (equal "GdkMonitor" (gobject-class-direct-g-type-name class)))
    (is (equal "gdk_monitor_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GdkMonitor")))
  (is (= 2 (g-type-depth "GdkMonitor")))
  (is (equal (gtype "GdkMonitor")
             (g-type-next-base "GdkMonitor" "GObject")))
  (is-true  (g-type-is-a "GdkMonitor" "GObject"))
  (is-false (g-type-is-a "GdkMonitor" "GtkWidget"))
  (is-false (g-type-is-a "GdkMonitor" "gboolean"))

  ;; Check the children
  (is (equal '("GdkX11Monitor" "GdkBroadwayMonitor")
             (mapcar #'gtype-name (g-type-children "GdkMonitor"))))

  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkMonitor"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkMonitor" query)
    (is (equal (gtype "GdkMonitor")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkMonitor"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 144 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  88 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("display" "manufacturer" "model" "scale-factor" "geometry" "workarea"
 "width-mm" "height-mm" "refresh-rate" "subpixel-layout")
             (mapcar #'param-spec-name
                     (g-object-class-list-properties "GdkMonitor"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
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
             (get-g-type-definition "GdkMonitor"))))

(test gdk-monitor-property
  (let ((monitor (gdk-display-get-primary-monitor (gdk-display-get-default))))
    ;; gdk_monitor_get_display
    (is (eq 'gdk-display
            (type-of (gdk-monitor-display monitor))))
    ;; gdk_monitor_get_geometry
    (is-true (gdk-rectangle-equal (make-gdk-rectangle :x 0 
                                                      :y 0 
                                                      :width 1366 
                                                      :height 768)
                                  (gdk-monitor-geometry monitor)))
    ;; gdk_monitor_get_workarea
    (is-true (gdk-rectangle-equal (make-gdk-rectangle :x 0 
                                                      :y 27 
                                                      :width 1366 
                                                      :height 741)
                                  (gdk-monitor-workarea monitor)))
    ;; gdk_monitor_get_width_mm
    (is (= 344 (gdk-monitor-width-mm monitor)))
    ;; gdk_monitor_get_height_mm
    (is (= 193 (gdk-monitor-height-mm monitor)))
    ;; gdk_monitor_get_manufacturer
    (is-false (gdk-monitor-manufacturer monitor))
    ;; gdk_monitor_get_model
    (is (equal "eDP-1" (gdk-monitor-model monitor)))
    ;; gdk_monitor_get_scale_factor
    (is (= 1 (gdk-monitor-scale-factor monitor)))
    ;; gdk_monitor_get_refresh_rate
    (is (= 60059 (gdk-monitor-refresh-rate monitor)))
    ;; gdk_monitor_get_subpixel_layout
    (is (eq :unknown (gdk-monitor-subpixel-layout monitor)))))

;;;     gdk_monitor_is_primary

(test gdk-monitor-property
  (let ((monitor (gdk-display-get-primary-monitor (gdk-display-get-default))))
    (is-true (gdk-monitor-is-primary monitor))))

