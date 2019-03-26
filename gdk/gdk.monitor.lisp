;;; ----------------------------------------------------------------------------
;;; gdk.monitor.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;;     Object representing an output
;;;
;;; Types and Values
;;;
;;;     GdkMonitor
;;;     GdkSubpixelLayout
;;;
;;; Functions
;;;
;;;     gdk_monitor_get_display                            Accessor
;;;     gdk_monitor_get_geometry                           Accessor
;;;     gdk_monitor_get_workarea                           Accessor
;;;     gdk_monitor_get_width_mm                           Accessor
;;;     gdk_monitor_get_height_mm                          Accessor
;;;     gdk_monitor_get_manufacturer                       Accessor
;;;     gdk_monitor_get_model                              Accessor
;;;     gdk_monitor_get_scale_factor                       Accessor
;;;     gdk_monitor_get_refresh_rate                       Accessor
;;;     gdk_monitor_get_subpixel_layout                    Accessor
;;;     gdk_monitor_is_primary
;;;
;;; Properties
;;;
;;;            GdkDisplay*  display          Read / Write / Construct Only
;;;          GdkRectangle*  geometry         Read
;;;                  gint   height-mm        Read
;;;                 gchar*  manufacturer     Read
;;;                 gchar*  model            Read
;;;                  gint   refresh-rate     Read
;;;                  gint   scale-factor     Read
;;;     GdkSubpixelLayout   subpixel-layout  Read
;;;                  gint   width-mm         Read
;;;          GdkRectangle*  workarea         Read
;;;
;;; Signals
;;;
;;;     void	invalidate	Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkMonitor
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkSubpixelLayout
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkSubpixelLayout" gdk-subpixel-layout
  (:export t
   :type-initializer "gdk_subpixel_layout_get_type")
  :unkown
  :none
  :horizontal-rgb
  :horizontal-bgr
  :vertical-rgb
  :vertical-bgr)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-subpixel-layout atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-subpixel-layout atdoc:*external-symbols*)
 "@version{2019-3-25}
  @begin{short}
    This enumeration describes how the red, green and blue components of
    physical pixels on an output device are laid out.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkSubPixelLayout\" gdk-sub-pixel-layout
  (:export t
   :type-initializer \"gdk_sub_pixel_layout_get_type\")
  :unkown
  :none
  :horizontal-rgb
  :horizontal-bgr
  :vertical-rgb
  :vertical-bgr)
  @end{pre}
  @begin[code]{table}
    @entry[:unkown]{The layout is not known.}
    @entry[:none]{Not organized in this way.}
    @entry[:horizontal-rgb]{The layout is horizontal, the order is RGB.}
    @entry[:horizontal-bgr]{The layout is horizontal, the order is BGR.}
    @entry[:vertical-rgb]{The layout is vertical, the order is RGB.}
    @entry[:verticla-bgr]{The layout is vertical, the order is BGR.}
  @end{table}
  Since 3.22
  @see-class{gdk-monitor}")

;;; ----------------------------------------------------------------------------
;;; struct GdkMonitor
;;; ----------------------------------------------------------------------------

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
    "geometry" "GdkRectangle" t nil)
   (height-mm
    gdk-monitor-height-mm
    "height-mm" "gint" t nil)
   (manufacturer
    gdk-monitor-manufacturer
    "manufacturer" "gchar" t nil)
   (model
    gdk-monitor-model
    "model" "gchar" t nil)
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
    "workarea" "GdkRectangle" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-monitor 'type)
 "@version{2019-3-25}
  @begin{short}
    @sym{gdk-monitor} objects represent the individual outputs that are
    associated with a @class{gdk-display}.
  @end{short}
  @class{gdk-display} has APIs to enumerate monitors with the functions
  @fun{gdk-display-get-n-monitors} and @fun{gdk-display-get-monitor}, and to
  find particular monitors with the functions
  @fun{gdk-display-get-primary-monitor} or
  @fun{gdk-display-get-monitor-at-window}.

  @sym{gdk-monitor} was introduced in GTK+ 3.22 and supersedes earlier APIs in
  @class{gdk-screen} to obtain monitor-related information.
  @begin[Signal Details]{dictionary}
    @subheading{The \"invalidate\" signal}
    @begin{pre}
 lambda (monitor)    : Run First
    @end{pre}
    @begin[code]{table}
      @entry[monitor]{The @sym{gdk-monitor} on which the signal is emitted.}
    @end{table}
    Since 3.22
  @end{dictionary}
  @see-slot{gdk-monitor-display}
  @see-slot{gdk-monitor-geometry}
  @see-slot{gdk-monitor-height-mm}
  @see-slot{gdk-monitor-manufacturer}
  @see-slot{gdk-monitor-model}
  @see-slot{gdk-monitor-refresh-rate}
  @see-slot{gdk-monitor-scale-factor}
  @see-slot{gdk-monitor-subpixel-layout}
  @see-slot{gdk-monitor-width-mm}
  @see-slot{gdk-monitor-workarea}
  @see-class{gdk-display}
  @see-class{gdk-screen}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-monitor-display ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display" 'gdk-monitor) 't)
 "The @code{display} property of type @class{gdk-display}
  (Read / Write / Construct Only) @br{}
  The @class{gdk-display} of the monitor. @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-display 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-display object) => display}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[display]{a @class{gdk-display}}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{display} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-display}
  gets the display that this monitor belongs to.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-geometry ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "geometry" 'gdk-monitor) 't)
 "The @code{geometry} property of type @class{gdk-rectangle} (Read) @br{}
  The geometry of the monitor. @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-geometry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-geometry 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-geometry object) => geometry}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[geometry]{a @class{gdk-rectangle} with the monitor geometry}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{geometry} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-geometry}
  retrieves the size and position of an individual monitor within the display
  coordinate space. The returned geometry is in \"application pixels\", not in
  \"device pixels\". See the function @fun{gdk-monitor-scale-factor}.

  Since 3.22
  @see-class{gdk-monitor}
  @see-function{gdk-monitor-scale-factor}")

;;; --- gdk-monitor-height-mm --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height-mm" 'gdk-monitor) 't)
 "The @code{height-mm} property of type @code{:int} (Read) @br{}
  The height of the monitor, in millimeters. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-height-mm atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-height-mm 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-height-mm object) => height}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[height]{the physical height of the monitor}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{height-mm} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-height-mm}
  gets the height in millimeters of the monitor.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-manufacturer -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "manufacturer" 'gdk-monitor) 't)
 "The @code{manufacturer} property of type @code{:string} (Read) @br{}
  The manufacturer name. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-manufacturer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-manufacturer 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-manufacturer object) => manufacturer}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[manufacturer]{the name of the manufacturer, or @code{nil}}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{manufacturer} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-manufacturer}
  gets the name of the monitor's manufacturer, if available.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-model ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gdk-monitor) 't)
 "The @code{model} property of type @code{:string} (Read) @br{}
  The model name. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-model 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-model object) => model}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[model]{the monitor model, or @code{nil}}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{model} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-model}
  gets a string identifying the monitor model, if available.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-refresh-rate -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "refresh-rate" 'gdk-monitor) 't)
 "The @code{refresh-rate} property of type @code{:int} (Read) @br{}
  The refresh rate, in millihertz. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-refresh-rate atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-refresh-rate 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-refresh-rate object) => refresh-rate}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[refresh-rate]{the refresh rate in milli-Hertz, or 0}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{refresh-rate} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-refresh-rate}
  gets the refresh rate of the monitor, if available.

  The value is in milli-Hertz, so a refresh rate of 60 Hz is returned as 60000.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-scale-factor -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scale-factor" 'gdk-monitor) 't)
 "The @code{scale-factor} property of type @code{:int} (Read) @br{}
  The scale factor. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1 @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-scale-factor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-scale-factor 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-model object) => scale-factor}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[scale-factor]{the scale factor}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{scale-factor} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-scale-factor}
  gets the internal scale factor that maps from monitor coordinates to the
  actual device pixels. On traditional systems this is 1, but on very high
  density outputs this can be a higher value (often 2).

  This can be used if you want to create pixel based data for a particular
  monitor, but most of the time you’re drawing to a window where it is better
  to use the function @fun{gdk-window-get-scale-factor} instead.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-subpixel-layout --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "subpixel-layout" 'gdk-monitor)
                     't)
 "The @code{subpixel-layout} property of type @symbol{gdk-subpixel-layout}
  (Read) @br{}
  The subpixel layout. @br{}
  Default value: @code{:unknown} @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-subpixel-layout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-subpixel-layout 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-subpixel-layout object) => layout}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[layout]{a subpixel layout}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{subpixel-layout} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-subpixel-layout}
  gets information about the layout of red, green and blue primaries for each
  pixel in this monitor, if available.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-width-mm ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-mm" 'gdk-monitor)
                     't)
 "The @code{width-mm} property of type @code{:int} (Read) @br{}
  The width of the monitor, in millimeters. @br{}
  Allowed values: >= 0
  Default value: 0 @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-width-mm atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-width-mm 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-width-mm object) => width}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[width]{the physical width of the monitor}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{width-mm} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-width-mm}
  gets the width in millimeters of the monitor.

  Since 3.22
  @see-class{gdk-monitor}")

;;; --- gdk-monitor-workarea ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "workarea" 'gdk-monitor) 't)
 "The @code{workarea} property of type @class{gdk-rectangle} (Read) @br{}
  The workarea of the monitor. @br{}
  Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-monitor-workarea atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-monitor-workarea 'function)
 "@version{2019-3-25}
  @syntax[]{(gdk-monitor-workarea object) => workarea}
  @argument[object]{a @class{gdk-monitor} object}
  @argument[workarea]{a @class{gdk-rectangle} with the monitor workarea}
  @begin{short}
    Accessor of the slot @slot[gdk-monitor]{workarea} of the
    @class{gdk-monitor} class.
  @end{short}

  The slot access function @sym{gdk-monitor-workarea}
  retrieves the size and position of the \"work area\" on a monitor within the
  display coordinate space. The returned geometry is in \"application pixels\",
  not in \"device pixels\". See the function @fun{gdk-monitor-scale-factor}.

  The work area should be considered when positioning menus and similar popups,
  to avoid placing them below panels, docks or other desktop components.

  Note that not all backends may have a concept of workarea. This function will
  return the monitor geometry if a workarea is not available, or does not apply.

  Since 3.22
  @see-class{gdk-monitor}
  @fun{gdk-monitor-scale-factor}")

;;; ----------------------------------------------------------------------------
;;; gdk_monitor_is_primary ()
;;;
;;; gboolean gdk_monitor_is_primary (GdkMonitor *monitor);
;;;
;;; Gets whether this monitor should be considered primary (see
;;; gdk_display_get_primary_monitor()).
;;;
;;; monitor :
;;;     a GdkMonitor
;;;
;;; Returns :
;;;     TRUE if monitor is primary
;;;
;;; Since: 3.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.monitor.lisp -------------------------------------------
