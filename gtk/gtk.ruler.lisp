;;; ----------------------------------------------------------------------------
;;; gtk.ruler.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkRuler
;;; 
;;; Base class for horizontal or vertical rulers
;;; 
;;; Synopsis
;;; 
;;;     GtkRuler
;;;     GtkRulerMetric
;;;
;;;     gtk_ruler_set_metric
;;;     gtk_ruler_set_range
;;;     gtk_ruler_get_metric
;;;     gtk_ruler_get_range
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkRuler
;;;                            +----GtkHRuler
;;;                            +----GtkVRuler
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkRuler implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Properties
;;; 
;;;   "lower"                    gdouble               : Read / Write
;;;   "max-size"                 gdouble               : Read / Write
;;;   "metric"                   GtkMetricType         : Read / Write
;;;   "position"                 gdouble               : Read / Write
;;;   "upper"                    gdouble               : Read / Write
;;; 
;;; Description
;;; Note
;;; 
;;; This widget is considered too specialized/little-used for GTK+, and will be
;;; removed in GTK 3. If your application needs this widget, feel free to use
;;; it, as the widget is useful in some applications; it's just not of general
;;; interest. However, we are not accepting new features for the widget, and it
;;; will move out of the GTK+ distribution.
;;; 
;;; The GTKRuler widget is a base class for horizontal and vertical rulers.
;;; Rulers are used to show the mouse pointer's location in a window. The ruler
;;; can either be horizontal or vertical on the window. Within the ruler a small
;;; triangle indicates the location of the mouse relative to the horizontal or
;;; vertical ruler. See GtkHRuler to learn how to create a new horizontal ruler.
;;; See GtkVRuler to learn how to create a new vertical ruler.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "lower" property
;;; 
;;;   "lower"                    gdouble               : Read / Write
;;; 
;;; Lower limit of ruler.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-size" property
;;; 
;;;   "max-size"                 gdouble               : Read / Write
;;; 
;;; Maximum size of the ruler.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "metric" property
;;; 
;;;   "metric"                   GtkMetricType         : Read / Write
;;; 
;;; The metric used for the ruler.
;;; 
;;; Default value: GTK_PIXELS
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "position" property
;;; 
;;;   "position"                 gdouble               : Read / Write
;;; 
;;; Position of mark on the ruler.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "upper" property
;;; 
;;;   "upper"                    gdouble               : Read / Write
;;; 
;;; Upper limit of ruler.
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkMetricType
;;; 
;;; typedef enum {
;;;   GTK_PIXELS,
;;;   GTK_INCHES,
;;;   GTK_CENTIMETERS
;;; } GtkMetricType;
;;; 
;;; Used to indicate which metric is used by a GtkRuler.
;;; 
;;; GTK_PIXELS
;;;     Pixels
;;; 
;;; GTK_INCHES
;;;     Inches
;;; 
;;; GTK_CENTIMETERS
;;;     Centimeters 
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMetricType" gtk-metric-type
  (:export t
   :type-initializer "gtk_metric_type_get_type")
  (:pixels 0)
  (:inches 1)
  (:centimeters 2))

;;; ----------------------------------------------------------------------------
;;; struct GtkRuler
;;; 
;;; struct GtkRuler;
;;; 
;;; Warning
;;; 
;;; GtkRuler is deprecated and should not be used in newly-written code.
;;; 
;;; All distances are in 1/72nd's of an inch. (According to Adobe thats a point,
;;; but points are really 1/72.27 in.)
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRuler" gtk-ruler
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_ruler_get_type")
  ((lower
    gtk-ruler-lower
    "lower" "gdouble" t t)
   (max-size
    gtk-ruler-max-size
    "max-size" "gdouble" t t)
   (metric
    gtk-ruler-metri
    "metric" "GtkMetricType" t t)
   (position
    gtk-ruler-position
    "position" "gdouble" t t)
   (upper
    gtk-ruler-upper
    "upper" "gdouble" t t)))

;;; ----------------------------------------------------------------------------
;;; struct GtkRulerMetric
;;; 
;;; struct GtkRulerMetric {
;;;   gchar *metric_name;
;;;   gchar *abbrev;
;;;   /* This should be points_per_unit. This is the size of the unit
;;;    * in 1/72nd's of an inch and has nothing to do with screen pixels */
;;;   gdouble pixels_per_unit;
;;;   gdouble ruler_scale[10];
;;;   gint subdivide[5];        /* five possible modes of subdivision */
;;; };
;;; 
;;; Warning
;;; 
;;; GtkRulerMetric has been deprecated since version 2.24 and should not be used
;;; in newly-written code. GtkRuler has been removed from GTK 3 for being
;;; unmaintained and too specialized. There is no replacement.
;;; 
;;; This should be points_per_unit. This is the size of the unit in 1/72nd's of
;;; an inch and has nothing to do with screen pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ruler_set_metric ()
;;; 
;;; void gtk_ruler_set_metric (GtkRuler *ruler, GtkMetricType metric);
;;; 
;;; Warning
;;; 
;;; gtk_ruler_set_metric has been deprecated since version 2.24 and should not
;;; be used in newly-written code. GtkRuler has been removed from GTK 3 for
;;; being unmaintained and too specialized. There is no replacement.
;;; 
;;; This calls the GTKMetricType to set the ruler to units defined. Available
;;; units are GTK_PIXELS, GTK_INCHES, or GTK_CENTIMETERS. The default unit of
;;; measurement is GTK_PIXELS.
;;; 
;;; ruler :
;;;     the gtkruler
;;; 
;;; metric :
;;;     the unit of measurement
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ruler_set_range ()
;;; 
;;; void gtk_ruler_set_range (GtkRuler *ruler,
;;;                           gdouble lower,
;;;                           gdouble upper,
;;;                           gdouble position,
;;;                           gdouble max_size);
;;; 
;;; Warning
;;; 
;;; gtk_ruler_set_range is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; This sets the range of the ruler.
;;; 
;;; Deprecated: 2.24: GtkRuler has been removed from GTK 3 for being
;;; unmaintained and too specialized. There is no replacement.
;;; 
;;; ruler :
;;;     the gtkruler
;;; 
;;; lower :
;;;     the lower limit of the ruler
;;; 
;;; upper :
;;;     the upper limit of the ruler
;;; 
;;; position :
;;;     the mark on the ruler
;;; 
;;; max_size :
;;;     the maximum size of the ruler used when calculating the space to leave
;;;     for the text
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ruler_get_metric ()
;;; 
;;; GtkMetricType gtk_ruler_get_metric (GtkRuler *ruler);
;;; 
;;; Warning
;;; 
;;; gtk_ruler_get_metric is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Gets the units used for a GtkRuler. See gtk_ruler_set_metric().
;;; 
;;; ruler :
;;;     a GtkRuler
;;; 
;;; Returns :
;;;     the units currently used for ruler Deprecated: 2.24: GtkRuler has been
;;;     removed from GTK 3 for being unmaintained and too specialized. There is
;;;     no replacement.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ruler_get_range ()
;;; 
;;; void gtk_ruler_get_range (GtkRuler *ruler,
;;;                           gdouble *lower,
;;;                           gdouble *upper,
;;;                           gdouble *position,
;;;                           gdouble *max_size);
;;; 
;;; Warning
;;; 
;;; gtk_ruler_get_range is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Retrieves values indicating the range and current position of a GtkRuler.
;;; See gtk_ruler_set_range().
;;; 
;;; Deprecated: 2.24: GtkRuler has been removed from GTK 3 for being
;;; unmaintained and too specialized. There is no replacement.
;;; 
;;; ruler :
;;;     a GtkRuler
;;; 
;;; lower :
;;;     location to store lower limit of the ruler, or NULL
;;; 
;;; upper :
;;;     location to store upper limit of the ruler, or NULL
;;; 
;;; position :
;;;     location to store the current position of the mark on the ruler, or NULL
;;; 
;;; max_size :
;;;     location to store the maximum size of the ruler used when calculating
;;;     the space to leave for the text, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkHRuler
;;; 
;;; GtkHRuler — A horizontal ruler
;;; 
;;; Synopsis
;;; 
;;;     GtkHRuler
;;;
;;;     gtk_hruler_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkRuler
;;;                            +----GtkHRuler
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHRuler implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Description
;;;
;;; Note
;;; 
;;; This widget is considered too specialized/little-used for GTK+, and will be
;;; removed in GTK 3. If your application needs this widget, feel free to use
;;; it, as the widget is useful in some applications; it's just not of general
;;; interest. However, we are not accepting new features for the widget, and it
;;; will move out of the GTK+ distribution.
;;; 
;;; The HRuler widget is a widget arranged horizontally creating a ruler that
;;; is utilized around other widgets such as a text widget. The ruler is used
;;; to show the location of the mouse on the window and to show the size of the
;;; window in specified units. The available units of measurement are
;;; GTK_PIXELS, GTK_INCHES and GTK_CENTIMETERS. GTK_PIXELS is the default.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHRuler
;;; 
;;; struct GtkHRuler;
;;; 
;;; Warning
;;; 
;;; GtkHRuler is deprecated and should not be used in newly-written code.
;;; 
;;; The GtkHRuler struct contains private data and should be accessed with the
;;; functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkHRuler" gtk-h-ruler
  (:superclass gtk-ruler
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hruler_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_hruler_new ()
;;; 
;;; GtkWidget * gtk_hruler_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_hruler_new has been deprecated since version 2.24 and should not be used
;;; in newly-written code. GtkRuler has been removed from GTK 3 for being
;;; unmaintained and too specialized. There is no replacement.
;;; 
;;; Creates a new horizontal ruler.
;;; 
;;; Returns :
;;;     a new GtkHRuler.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkVRuler
;;; 
;;; GtkVRuler — A vertical ruler
;;; 
;;; Synopsis
;;; 
;;;     GtkVRuler
;;;
;;;     gtk_vruler_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkRuler
;;;                            +----GtkVRuler
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVRuler implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Description
;;; 
;;; Note
;;;
;;; This widget is considered too specialized/little-used for GTK+, and will be
;;; removed in GTK 3. If your application needs this widget, feel free to use
;;; it, as the widget is useful in some applications; it's just not of general
;;; interest. However, we are not accepting new features for the widget, and it
;;; will move out of the GTK+ distribution.
;;; 
;;; The VRuler widget is a widget arranged vertically creating a ruler that is
;;; utilized around other widgets such as a text widget. The ruler is used to
;;; show the location of the mouse on the window and to show the size of the
;;; window in specified units. The available units of measurement are
;;; GTK_PIXELS, GTK_INCHES and GTK_CENTIMETERS. GTK_PIXELS is the default unit
;;; of measurement.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVRuler
;;; 
;;; struct GtkVRuler;
;;; 
;;; Warning
;;; 
;;; GtkVRuler is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkVRuler" gtk-v-ruler
  (:superclass gtk-ruler
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vruler_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_vruler_new ()
;;; 
;;; GtkWidget * gtk_vruler_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_vruler_new is deprecated and should not be used in newly-written code.
;;; 
;;; Creates a new vertical ruler
;;; 
;;; Returns :
;;;     a new GtkVRuler. Deprecated: 2.24: GtkRuler has been removed from GTK 3
;;;     for being unmaintained and too specialized. There is no replacement.
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.ruler.lisp ---------------------------------------------
