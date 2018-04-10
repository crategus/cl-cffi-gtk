;;; ----------------------------------------------------------------------------
;;; gdk.monitor.lisp
;;;
;;; Copyright (C) 2017 Olof-Joachim Frahm
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GdkMonitor
;;;
;;; Object representing an output
;;;
;;; Types and Values
;;;
;;;     GdkMonitor
;;;     GdkSubpixelLayout
;;;
;;; Functions
;;;
;;;     gdk_monitor_get_display          -> Accessor
;;;     gdk_monitor_get_geometry
;;;     gdk_monitor_get_workarea
;;;     gdk_monitor_get_width_mm         -> Accessor
;;;     gdk_monitor_get_height_mm        -> Accessor
;;;     gdk_monitor_get_manufacturer     -> Accessor
;;;     gdk_monitor_get_model            -> Accessor
;;;     gdk_monitor_get_scale_factor     -> Accessor
;;;     gdk_monitor_get_refresh_rate     -> Accessor
;;;     gdk_monitor_get_subpixel_layout  -> Accessor
;;;     gdk_monitor_is_primary
;;; ----------------------------------------------------------------------------

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

#+gdk-3-22
(defcfun gdk-monitor-is-primary :boolean
  (monitor (g-object gdk-monitor)))

#+gdk-3-22
(export 'gdk-monitor-is-primary)
