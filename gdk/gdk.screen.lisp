;;; ----------------------------------------------------------------------------
;;; gdk.screen.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;;﻿
;;; GdkScreen
;;;
;;; Object representing a physical screen
;;;
;;; Synopsis
;;;
;;;     GdkScreen
;;;
;;;     gdk_screen_get_default
;;;     gdk_screen_get_system_visual
;;;     gdk_screen_get_rgba_visual
;;;     gdk_screen_is_composited
;;;     gdk_screen_get_root_window
;;;     gdk_screen_get_display
;;;     gdk_screen_get_number
;;;     gdk_screen_get_width
;;;     gdk_screen_get_height
;;;     gdk_screen_get_width_mm
;;;     gdk_screen_get_height_mm
;;;     gdk_screen_list_visuals
;;;     gdk_screen_get_toplevel_windows
;;;     gdk_screen_make_display_name
;;;     gdk_screen_get_n_monitors
;;;     gdk_screen_get_primary_monitor
;;;     gdk_screen_get_monitor_geometry
;;;     gdk_screen_get_monitor_workarea
;;;     gdk_screen_get_monitor_at_point
;;;     gdk_screen_get_monitor_at_window
;;;     gdk_screen_get_monitor_height_mm
;;;     gdk_screen_get_monitor_width_mm
;;;     gdk_screen_get_monitor_plug_name
;;;     gdk_screen_get_setting
;;;     gdk_screen_get_font_options
;;;     gdk_screen_set_font_options
;;;     gdk_screen_get_resolution
;;;     gdk_screen_set_resolution
;;;     gdk_screen_get_active_window
;;;     gdk_screen_get_window_stack
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkScreen
;;;
;;; Properties
;;;
;;;   "font-options"             gpointer              : Read / Write
;;;   "resolution"               gdouble               : Read / Write
;;;
;;; Signals
;;;
;;;   "composited-changed"                             : Run Last
;;;   "monitors-changed"                               : Run Last
;;;   "size-changed"                                   : Run Last
;;;
;;; Description
;;;
;;; GdkScreen objects are the GDK representation of the screen on which windows
;;; can be displayed and on which the pointer moves. X originally identified
;;; screens with physical screens, but nowadays it is more common to have a
;;; single GdkScreen which combines several physical monitors (see
;;; gdk_screen_get_n_monitors()).
;;;
;;; GdkScreen is used throughout GDK and GTK+ to specify which screen the top
;;; level windows are to be displayed on. it is also used to query the screen
;;; specification and default settings such as the default visual
;;; (gdk_screen_get_system_visual()), the dimensions of the physical monitors
;;; (gdk_screen_get_monitor_geometry()), etc.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-options" property
;;;
;;;   "font-options"             gpointer              : Read / Write
;;;
;;; The default font options for the screen.
;;;
;;; ----------------------------------------------------------------------------
;;; The "resolution" property
;;;
;;;   "resolution"               gdouble               : Read / Write
;;;
;;; The resolution for fonts on the screen.
;;;
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "composited-changed" signal
;;;
;;; void user_function (GdkScreen *screen,
;;;                     gpointer   user_data)      : Run Last
;;;
;;; The ::composited-changed signal is emitted when the composited status of the
;;; screen changes
;;;
;;; screen :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "monitors-changed" signal
;;;
;;; void user_function (GdkScreen *screen,
;;;                     gpointer   user_data)      : Run Last
;;;
;;; The ::monitors-changed signal is emitted when the number, size or position
;;; of the monitors attached to the screen change.
;;;
;;; Only for X11 and OS X for now. A future implementation for Win32 may be a
;;; possibility.
;;;
;;; screen :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-changed" signal
;;;
;;; void user_function (GdkScreen *screen,
;;;                     gpointer   user_data)      : Run Last
;;;
;;; The ::size-changed signal is emitted when the pixel width or height of a
;;; screen changes.
;;;
;;; screen :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkScreen
;;;
;;; typedef struct _GdkScreen GdkScreen;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkScreen" gdk-screen
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_screen_get_type")
  ((font-options
    gdk-screen-font-options
    "font-options" "gpointer" t t)
   (resolution
    gdk-screen-resolution
    "resolution" "gdouble" t t)))

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_default ()
;;;
;;; GdkScreen * gdk_screen_get_default (void);
;;;
;;; Gets the default screen for the default display. (See
;;; gdk_display_get_default()).
;;;
;;; Returns :
;;;     a GdkScreen, or NULL if there is no default display
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_default" gdk-screen-get-default)
    (g-object gdk-screen))

(export 'gdk-screen-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_system_visual ()
;;;
;;; GdkVisual * gdk_screen_get_system_visual (GdkScreen *screen);
;;;
;;; Get the system's default visual for screen. This is the visual for the root
;;; window of the display. The return value should not be freed.
;;;
;;; screen :
;;;     a GdkScreen.
;;;
;;; Returns :
;;;     the system visual
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_system_visual" gdk-screen-get-system-visual)
    (g-object gdk-visual)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-system-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgba_visual ()
;;;
;;; GdkVisual * gdk_screen_get_rgba_visual (GdkScreen *screen);
;;;
;;; Gets a visual to use for creating windows with an alpha channel. The
;;; windowing system on which GTK+ is running may not support this capability,
;;; in which case NULL will be returned. Even if a non-NULL value is returned,
;;; its possible that the window's alpha channel won't be honored when
;;; displaying the window on the screen: in particular, for X an appropriate
;;; windowing manager and compositing manager must be running to provide
;;; appropriate display.
;;;
;;; This functionality is not implemented in the Windows backend.
;;;
;;; For setting an overall opacity for a top-level window, see
;;; gdk_window_set_opacity().
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     a visual to use for windows with an alpha channel or NULL if the
;;;     capability is not available
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_rgba_visual" gdk-screen-get-rgba-visual)
    (g-object gdk-visual)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-rgba-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_is_composited ()
;;;
;;; gboolean gdk_screen_is_composited (GdkScreen *screen);
;;;
;;; Returns whether windows with an RGBA visual can reasonably be expected to
;;; have their alpha channel drawn correctly on the screen.
;;;
;;; On X11 this function returns whether a compositing manager is compositing
;;; screen.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     Whether windows with RGBA visuals can reasonably be expected to have
;;;     their alpha channels drawn correctly on the screen.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_is_composited" gdk-screen-is-composited) :boolean
  (screen (g-object gdk-screen)))

(export 'gdk-screen-is-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_root_window ()
;;;
;;; GdkWindow * gdk_screen_get_root_window (GdkScreen *screen);
;;;
;;; Gets the root window of screen.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the root window
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_root_window" gdk-screen-get-root-window)
    (g-object gdk-window)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_display ()
;;;
;;; GdkDisplay * gdk_screen_get_display (GdkScreen *screen);
;;;
;;; Gets the display to which the screen belongs.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the display to which screen belongs
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_display" gdk-screen-get-display)
    (g-object gdk-display)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_number ()
;;;
;;; gint gdk_screen_get_number (GdkScreen *screen);
;;;
;;; Gets the index of screen among the screens in the display to which it
;;; belongs. (See gdk_screen_get_display())
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the index
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_number" gdk-screen-get-number) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-number)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width ()
;;;
;;; gint gdk_screen_get_width (GdkScreen *screen);
;;;
;;; Gets the width of screen in pixels
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the width of screen in pixels.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_width" gdk-screen-get-width) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height ()
;;;
;;; gint gdk_screen_get_height (GdkScreen *screen);
;;;
;;; Gets the height of screen in pixels
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the height of screen in pixels.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_height" gdk-screen-get-height) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width_mm ()
;;;
;;; gint gdk_screen_get_width_mm (GdkScreen *screen);
;;;
;;; Gets the width of screen in millimeters. Note that on some X servers this
;;; value will not be correct.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the width of screen in millimeters.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_width_mm" gdk-screen-get-width-mm) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height_mm ()
;;;
;;; gint gdk_screen_get_height_mm (GdkScreen *screen);
;;;
;;; Returns the height of screen in millimeters. Note that on some X servers
;;; this value will not be correct.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the heigth of screen in millimeters.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_height_mm" gdk-screen-get-height-mm) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_list_visuals ()
;;;
;;; GList * gdk_screen_list_visuals (GdkScreen *screen);
;;;
;;; Lists the available visuals for the specified screen. A visual describes a
;;; hardware image data format. For example, a visual might support 24-bit
;;; color, or 8-bit color, and might expect pixels to be in a certain format.
;;;
;;; Call g_list_free() on the return value when you're finished with it.
;;;
;;; screen :
;;;     the relevant GdkScreen.
;;;
;;; Returns :
;;;     a list of visuals; the list must be freed, but not its contents
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_list_visuals" gdk-screen-list-visuals)
    (g-list (g-object gdk-visual) :free-from-foreign t)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_toplevel_windows ()
;;;
;;; GList * gdk_screen_get_toplevel_windows (GdkScreen *screen);
;;;
;;; Obtains a list of all toplevel windows known to GDK on the screen screen. A
;;; toplevel window is a child of the root window (see
;;; gdk_get_default_root_window()).
;;;
;;; The returned list should be freed with g_list_free(), but its elements need
;;; not be freed.
;;;
;;; screen :
;;;     The GdkScreen where the toplevels are located.
;;;
;;; Returns :
;;;     list of toplevel windows, free with g_list_free()
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_toplevel_windows" gdk-screen-get-toplevel-windows)
    (g-list (g-object gdk-window) :free-from-foreign t)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-toplevel-windows)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_make_display_name ()
;;;
;;; gchar * gdk_screen_make_display_name (GdkScreen *screen);
;;;
;;; Determines the name to pass to gdk_display_open() to get a GdkDisplay with
;;; this screen as the default screen.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     a newly allocated string, free with g_free()
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_make_display_name" gdk-screen-make-display-name)
    (g-string :free-from-foreign t)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-make-display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_n_monitors ()
;;;
;;; gint gdk_screen_get_n_monitors (GdkScreen *screen);
;;;
;;; Returns the number of monitors which screen consists of.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     number of monitors which screen consists of
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_n_monitors" gdk-screen-get-n-monitors) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-n-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_primary_monitor ()
;;;
;;; gint gdk_screen_get_primary_monitor (GdkScreen *screen);
;;;
;;; Gets the primary monitor for screen. The primary monitor is considered the
;;; monitor where the 'main desktop' lives. While normal application windows
;;; typically allow the window manager to place the windows, specialized desktop
;;; applications such as panels should place themselves on the primary monitor.
;;;
;;; If no primary monitor is configured by the user, the return value will be 0,
;;; defaulting to the first monitor.
;;;
;;; screen :
;;;     a GdkScreen.
;;;
;;; Returns :
;;;     An integer index for the primary monitor, or 0 if none is configured.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_primary_monitor" gdk-screen-get-primary-monitor) :int
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-primary-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_geometry ()
;;;
;;; void gdk_screen_get_monitor_geometry (GdkScreen *screen,
;;;                                       gint monitor_num,
;;;                                       GdkRectangle *dest);
;;;
;;; Retrieves the GdkRectangle representing the size and position of the
;;; individual monitor within the entire screen area.
;;;
;;; Monitor numbers start at 0. To obtain the number of monitors of screen, use
;;; gdk_screen_get_n_monitors().
;;;
;;; Note that the size of the entire screen area can be retrieved via
;;; gdk_screen_get_width() and gdk_screen_get_height().
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; monitor_num :
;;;     the monitor number
;;;
;;; dest :
;;;     a GdkRectangle to be filled with the monitor geometry
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_geometry" %gdk-screen-get-monitor-geometry)
    :void
  (screen (g-object gdk-screen))
  (monitor-num :int)
  (dest (g-boxed-foreign gdk-rectangle)))

(defun gdk-screen-get-monitor-geometry (screen monitor-num)
  (let ((dest (make-gdk-rectangle)))
    (%gdk-screen-get-monitor-geometry screen monitor-num dest)
    dest))

(export 'gdk-screen-get-monitor-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_workarea ()
;;;
;;; void gdk_screen_get_monitor_workarea (GdkScreen *screen,
;;;                                       gint monitor_num,
;;;                                       GdkRectangle *dest);
;;;
;;; Retrieves the GdkRectangle representing the size and position of the
;;; "work area" on a monitor within the entire screen area.
;;;
;;; The work area should be considered when positioning menus and similar
;;; popups, to avoid placing them below panels, docks or other desktop
;;; components.
;;;
;;; Monitor numbers start at 0. To obtain the number of monitors of screen, use
;;; gdk_screen_get_n_monitors().
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; monitor_num :
;;;     the monitor number
;;;
;;; dest :
;;;     a GdkRectangle to be filled with the monitor workarea
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_workarea" %gdk-screen-get-monitor-workarea)
    :void
  (screen (g-object gdk-screen))
  (monitor-num :int)
  (dest (g-boxed-foreign gdk-rectangle)))

(defun gdk-screen-get-monitor-workarea (screen monitor-num)
  (let ((dest (make-gdk-rectangle)))
    (%gdk-screen-get-monitor-workarea screen monitor-num dest)
    dest))

(export 'gdk-screen-get-monitor-workarea)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_point ()
;;;
;;; gint gdk_screen_get_monitor_at_point (GdkScreen *screen, gint x, gint y);
;;;
;;; Returns the monitor number in which the point (x,y) is located.
;;;
;;; screen :
;;;     a GdkScreen.
;;;
;;; x :
;;;     the x coordinate in the virtual screen.
;;;
;;; y :
;;;     the y coordinate in the virtual screen.
;;;
;;; Returns :
;;;     the monitor number in which the point (x,y) lies, or a monitor close to
;;;     (x,y) if the point is not in any monitor.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_at_point" gdk-screen-get-monitor-at-point)
    :int
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-screen-get-monitor-at-point)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_window ()
;;;
;;; gint gdk_screen_get_monitor_at_window (GdkScreen *screen, GdkWindow *window)
;;;
;;; Returns the number of the monitor in which the largest area of the bounding
;;; rectangle of window resides.
;;;
;;; screen :
;;;     a GdkScreen.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; Returns :
;;;     the monitor number in which most of window is located, or if window does
;;;     not intersect any monitors, a monitor, close to window.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_at_window" gdk-screen-get-monitor-at-window)
    :int
  (screen (g-object gdk-screen))
  (window (g-object gdk-window)))

(export 'gdk-screen-get-monitor-at-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_height_mm ()
;;;
;;; gint gdk_screen_get_monitor_height_mm (GdkScreen *screen, gint monitor_num);
;;;
;;; Gets the height in millimeters of the specified monitor.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; monitor_num :
;;;     number of the monitor, between 0 and gdk_screen_get_n_monitors (screen)
;;;
;;; Returns :
;;;     the height of the monitor, or -1 if not available
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_height_mm" gdk-screen-get-monitor-height-mm)
    :int
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_width_mm ()
;;;
;;; gint gdk_screen_get_monitor_width_mm (GdkScreen *screen, gint monitor_num);
;;;
;;; Gets the width in millimeters of the specified monitor, if available.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; monitor_num :
;;;     number of the monitor, between 0 and gdk_screen_get_n_monitors (screen)
;;;
;;; Returns :
;;;     the width of the monitor, or -1 if not available
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_width_mm" gdk-screen-get-monitor-width-mm)
    :int
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_plug_name ()
;;;
;;; gchar * gdk_screen_get_monitor_plug_name (GdkScreen *screen,
;;;                                           gint monitor_num);
;;;
;;; Returns the output name of the specified monitor. Usually something like
;;; VGA, DVI, or TV, not the actual product name of the display device.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; monitor_num :
;;;     number of the monitor, between 0 and gdk_screen_get_n_monitors (screen)
;;;
;;; Returns :
;;;     a newly-allocated string containing the name of the monitor, or NULL if
;;;     the name cannot be determined
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_plug_name" gdk-screen-get-monitor-plug-name)
    (g-string :free-from-foreign t)
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-plug-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_setting ()
;;;
;;; gboolean gdk_screen_get_setting (GdkScreen *screen,
;;;                                  const gchar *name,
;;;                                  GValue *value);
;;;
;;; Retrieves a desktop-wide setting such as double-click time for the GdkScreen
;;; screen.
;;;
;;; FIXME needs a list of valid settings here, or a link to more information.
;;;
;;; screen :
;;;     the GdkScreen where the setting is located
;;;
;;; name :
;;;     the name of the setting
;;;
;;; value :
;;;     location to store the value of the setting
;;;
;;; Returns :
;;;     TRUE if the setting existed and a value was stored in value, FALSE
;;;     otherwise.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_setting" %gdk-screen-get-setting) :boolean
  (screen (g-object gdk-screen))
  (name :string)
  (value :pointer))

(defun gdk-screen-get-setting (screen name)
  (with-foreign-object (value 'g-value)
    (g-value-init value)
    (when (%gdk-screen-get-setting screen name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-screen-get-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_font_options ()
;;;
;;; const cairo_font_options_t * gdk_screen_get_font_options
;;;                                                         (GdkScreen *screen);
;;;
;;; Gets any options previously set with gdk_screen_set_font_options().
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the current font options, or NULL if no default font options have been
;;;     set.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-get-font-options))

(defun gdk-screen-get-font-options (screen)
  (gdk-screen-font-options screen))

(export 'gdk-screen-get-font-options)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_set_font_options ()
;;;
;;; void gdk_screen_set_font_options (GdkScreen *screen,
;;;                                   const cairo_font_options_t *options);
;;;
;;; Sets the default font options for the screen. These options will be set on
;;; any PangoContext's newly created with gdk_pango_context_get_for_screen().
;;; Changing the default set of font options does not affect contexts that have
;;; already been created.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; options :
;;;     a cairo_font_options_t, or NULL to unset any previously set default font
;;;     options
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-set-font-options))

(defun gdk-screen-set-font-options (screen options)
  (setf (gdk-screen-font-options screen) options))

(export 'gdk-screen-set-font-options)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_resolution ()
;;;
;;; gdouble gdk_screen_get_resolution (GdkScreen *screen);
;;;
;;; Gets the resolution for font handling on the screen; see
;;; gdk_screen_set_resolution() for full details.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the current resolution, or -1 if no resolution has been set.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-get-resolution))

(defun gdk-screen-get-resolution (screen)
  (gdk-screen-resolution screen))

(export 'gdk-screen-get-resolution)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_set_resolution ()
;;;
;;; void gdk_screen_set_resolution (GdkScreen *screen, gdouble dpi);
;;;
;;; Sets the resolution for font handling on the screen. This is a scale factor
;;; between points specified in a PangoFontDescription and cairo units. The
;;; default value is 96, meaning that a 10 point font will be 13 units high.
;;; (10 * 96. / 72. = 13.3).
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; dpi :
;;;     the resolution in "dots per inch". (Physical inches aren't actually
;;;     involved; the terminology is conventional.)
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-set-resolution))

(defun gdk-screen-set-resolution (screen dpi)
  (setf (gdk-screen-resolution screen) dpi))

(export 'gdk-screen-set-resolution)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_active_window ()
;;;
;;; GdkWindow * gdk_screen_get_active_window (GdkScreen *screen);
;;;
;;; Returns the screen's currently active window.
;;;
;;; On X11, this is done by inspecting the _NET_ACTIVE_WINDOW property on the
;;; root window, as described in the Extended Window Manager Hints. If there is
;;; no currently currently active window, or the window manager does not support
;;; the _NET_ACTIVE_WINDOW hint, this function returns NULL.
;;;
;;; On other platforms, this function may return NULL, depending on whether it
;;; is implementable on that platform.
;;;
;;; The returned window should be unrefed using g_object_unref() when no longer
;;; needed.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     the currently active window, or NULL
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_active_window" gdk-screen-get-active-window)
    (g-object gdk-window)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-active-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_window_stack ()
;;;
;;; GList * gdk_screen_get_window_stack (GdkScreen *screen);
;;;
;;; Returns a GList of GdkWindows representing the current window stack.
;;;
;;; On X11, this is done by inspecting the _NET_CLIENT_LIST_STACKING property on
;;; the root window, as described in the Extended Window Manager Hints. If the
;;; window manager does not support the _NET_CLIENT_LIST_STACKING hint, this
;;; function returns NULL.
;;;
;;; On other platforms, this function may return NULL, depending on whether it
;;; is implementable on that platform.
;;;
;;; The returned list is newly allocated and owns references to the windows it
;;; contains, so it should be freed using g_list_free() and its windows unrefed
;;; using g_object_unref() when no longer needed.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Returns :
;;;     a list of GdkWindows for the current window stack, or NULL
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_window_stack" gdk-screen-get-window-stack)
    (g-list (g-object gdk-window) :free-from-foreign t)
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-window-stack)

;;; --- End of file gdk.screen.lisp --------------------------------------------
