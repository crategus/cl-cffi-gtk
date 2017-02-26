(in-package :gdk)

#+gdk-3-22
(define-g-enum "GdkSubpixelLayout" gdk-subpixel-layout
  (:export t
   :type-initializer "gdk_subpixel_layout_get_type")
  (:unknown 0)
  (:none 1)
  (:horizontal-rgb 2)
  (:horizontal-bgr 3)
  (:vertical-rgb 4)
  (:vertical-bgr 5))

#+gdk-3-22
(define-g-object-class "GdkMonitor" gdk-monitor
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_monitor_get_type")
  ((display
    gdk-monitor-display
    "display" "GdkDisplay" t t)
   (geometry
    gdk-monitor-geometry
    "geometry" "GdkRectangle" nil nil)
   (height-mm
    gdk-monitor-height-mm
    "height-mm" "gint" t nil)
   (manufacturer
    gdk-monitor-manufacturer
    "manufacturer" "gchararray" t nil)
   (model
    gdk-monitor-model
    "model" "gchararray" t nil)
   (refresh-rate
    gdk-monitor-refresh-rate
    "refresh-rate" "gint" t nil)
   (scale-factor
    gdk-monitor-scale-factor
    "scale-factor" "gint" t nil)
   (subpixel-layout
    gdk-monitor-subpixel-layout
    "subpixel-layout" "GdkSubpixelLayout" t nil)
   (width-mm
    gdk-monitor-width-mm
    "width-mm" "gint" t nil)
   (workarea
    gdk-monitor-workarea
    "workarea" "GdkRectangle" nil nil)))

#+gdk-3-22
(defcfun ("gdk_monitor_get_geometry" %gdk-monitor-get-geometry) :void
  (monitor (g-object gdk-monitor))
  (geometry (g-boxed-foreign gdk-rectangle)))

#+gdk-3-22
(defun gdk-monitor-get-geometry (monitor)
  (let ((geometry (make-gdk-rectangle)))
    (%gdk-monitor-get-geometry monitor geometry)
    geometry))

#+gdk-3-22
(export 'gdk-monitor-get-geometry)

#+gdk-3-22
(defcfun ("gdk_monitor_get_workarea" %gdk-monitor-get-workarea) :void
  (monitor (g-object gdk-monitor))
  (workarea (g-boxed-foreign gdk-rectangle)))

#+gdk-3-22
(defun gdk-monitor-get-workarea (monitor)
  (let ((workarea (make-gdk-rectangle)))
    (%gdk-monitor-get-workarea monitor workarea)
    workarea))

#+gdk-3-22
(export 'gdk-monitor-get-workarea)
