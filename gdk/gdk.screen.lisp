;;; ----------------------------------------------------------------------------
;;; gdk.screen.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;; GdkScreen
;;; 
;;; Object representing a physical screen
;;; 
;;; Synopsis
;;; 
;;;     GdkScreen
;;;
;;;     gdk_screen_get_default
;;;     gdk_screen_get_default_colormap
;;;     gdk_screen_set_default_colormap
;;;     gdk_screen_get_system_colormap
;;;     gdk_screen_get_system_visual
;;;     gdk_screen_get_rgb_colormap
;;;     gdk_screen_get_rgb_visual
;;;     gdk_screen_get_rgba_colormap
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
;;;     gdk_screen_get_monitor_at_point
;;;     gdk_screen_get_monitor_at_window
;;;     gdk_screen_get_monitor_height_mm
;;;     gdk_screen_get_monitor_width_mm
;;;     gdk_screen_get_monitor_plug_name
;;;     gdk_screen_broadcast_client_message
;;;     gdk_screen_get_setting
;;;     gdk_screen_get_font_options
;;;     gdk_screen_set_font_options
;;;     gdk_screen_get_resolution
;;;     gdk_screen_set_resolution
;;;     gdk_screen_get_active_window
;;;     gdk_screen_get_window_stack
;;;     gdk_spawn_on_screen
;;;     gdk_spawn_on_screen_with_pipes
;;;     gdk_spawn_command_line_on_screen
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
;;; GdkScreen objects are the GDK representation of a physical screen. It is
;;; used throughout GDK and GTK+ to specify which screen the top level windows
;;; are to be displayed on. It is also used to query the screen specification
;;; and default settings such as the default colormap
;;; (gdk_screen_get_default_colormap()), the screen width
;;; (gdk_screen_get_width()), etc.
;;; 
;;; Note that a screen may consist of multiple monitors which are merged to
;;; form a large screen area.
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
;;; void user_function (GdkScreen *screen, gpointer user_data)      : Run Last
;;; 
;;; The ::composited-changed signal is emitted when the composited status of
;;; the screen changes.
;;; 
;;; screen :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "monitors-changed" signal
;;; 
;;; void user_function (GdkScreen *screen, gpointer user_data)      : Run Last
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
;;;     user data set when the signal handler was connected
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-changed" signal
;;; 
;;; void user_function (GdkScreen *screen, gpointer user_data)      : Run Last
;;; 
;;; The ::size-changed signal is emitted when the pixel width or height of a
;;; screen changes.
;;; 
;;; screen :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(in-package :gdk)

(defmacro with-foreign-string-array ((var strings &key (null-terminated t))
                                                  &body body)
  (let ((strings-var (gensym))
        (s (gensym))
        (i (gensym))
        (n (gensym)))
    `(let* ((,strings-var ,strings)
            (,n (length ,strings-var)))
       (with-foreign-object (,var :pointer ,(if null-terminated `(1+ ,n) `,n))
         (iter (for ,s in ,strings-var)
               (for ,i from 0)
               (setf (mem-aref ,var :pointer ,i) (foreign-string-alloc ,s))
               ,@(when null-terminated
                   (list `(finally (setf (mem-aref ,var :pointer ,n)
                                         (null-pointer))))))
         (unwind-protect (progn ,@body)
           (iter (for ,i from 0 below ,n)
                 (foreign-string-free (mem-aref ,var :pointer ,i))))))))

;;; ----------------------------------------------------------------------------
;;; GdkScreen
;;; 
;;; typedef struct _GdkScreen GdkScreen;
;;; 
;;; This is a currently just a placeholder typedef for the first argument of
;;; the window_at_pointer function in GdkPointerHooks. It will be used when GDK
;;; gets multihead support.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkScreen" gdk-screen
  (:type-initializer "gdk_screen_get_type")
  ((font-options
    gdk-screen-font-options
    "font-options" "gpointer" t t)
   (resolution
    gdk-screen-resolution
    "resolution" "gdouble" t t)
   (:cffi default-colormap
          gdk-screen-default-colormap (g-object gdk-colormap)
          "gdk_screen_get_default_colormap" "gdk_screen_set_default_colormap")
   (:cffi system-colormap
          gdk-screen-system-colormap (g-object gdk-colormap)
          "gdk_screen_get_system_colormap" nil)
   (:cffi system-visual
          gdk-screen-system-visual (g-object gdk-visual)
          "gdk_screen_get_system_visual" nil)
   (:cffi rgb-colormap
          gdk-screen-rgb-colormap (g-object gdk-colormap)
          "gdk_screen_get_rgb_colormap" nil)
   (:cffi rgb-visual
          gdk-screen-rgb-visual (g-object gdk-visual)
          "gdk_screen_get_rgb_visual" nil)
   (:cffi rgba-colormap
          gdk-screen-rgba-colormap (g-object gdk-colormap)
          "gdk_screen_get_rgba_colormap" nil)
   (:cffi rgba-visual
          gdk-screen-rgba-visual (g-object gdk-visual)
          "gdk_screen_get_rgba_visual" nil)
   (:cffi composited-p
          gdk-screen-composited-p :boolean
          "gdk_screen_is_composited" nil)
   (:cffi root-window
          gdk-screen-root-window (g-object gdk-window)
          "gdk_screen_get_root_window" nil)
   (:cffi display
          gdk-screen-display (g-object gdk-display)
          "gdk_screen_get_display" nil)
   (:cffi number
          gdk-screen-number :int
          "gdk_screen_get_number" nil)
   (:cffi width
          gdk-screen-width :int
          "gdk_screen_get_width" nil)
   (:cffi height
          gdk-screen-height :int
          "gdk_screen_get_height" nil)
   (:cffi width-mm
          gdk-screen-width-mm :int
          "gdk_screen_get_width_mm" nil)
   (:cffi height-mm
          gdk-screen-height-mm :int
          "gdk_screen_get_height_mm" nil)
   (:cffi visuals
          gdk-screen-visuals (g-list (g-object gdk-visual) :free-from-foreign t)
          "gdk_screen_list_visuals" nil)
   (:cffi toplevel-windows
          gdk-screen-toplevel-windows
          (g-list (g-object gdk-window) :free-from-foreign t)
          "gdk_screen_get_toplevel_windows" nil)
   (:cffi display-name
          gdk-screen-display-name (g-string :free-from-foreign t)
          "gdk_screen_make_display_name" nil)
   (:cffi n-monitors
          gdk-screen-n-monitors :int
          "gdk_screen_get_n_monitors" nil)
   (:cffi active-window
          gdk-screen-active-window (g-object gdk-window)
          "gdk_screen_get_active_window" nil)
   (:cffi window-stack
          gdk-screen-window-stack
          (g-list (g-object gdk-window) :free-from-foreign t)
          "gdk_screen_get_window_stack" nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_default ()
;;; 
;;; GdkScreen * gdk_screen_get_default (void);
;;; 
;;; Gets the default screen for the default display.
;;; (See gdk_display_get_default()).
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
;;; gdk_screen_get_default_colormap ()
;;; 
;;; GdkColormap * gdk_screen_get_default_colormap (GdkScreen *screen)
;;; 
;;; Gets the default colormap for screen.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     the default GdkColormap
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-default-colormap (screen)
  (gdk-screen-default-colormap screen))

(export 'gdk-screen-get-default-colormap)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_set_default_colormap ()
;;; 
;;; void gdk_screen_set_default_colormap (GdkScreen *screen,
;;;                                       GdkColormap *colormap);
;;; 
;;; Sets the default colormap for screen.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; colormap :
;;;     a GdkColormap
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-set-default-colormap (screen colormap)
  (setf (gdk-screen-default-colormap screen) colormap))

(export 'gdk-screen-set-default-colormap)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_system_colormap ()
;;; 
;;; GdkColormap * gdk_screen_get_system_colormap (GdkScreen *screen);
;;; 
;;; Gets the system's default colormap for screen
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     the default colormap for screen
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-system-colormap (screen)
  (gdk-screen-system-colormap screen))

(export 'gdk-screen-get-system-colormap)

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

(defun gdk-screen-get-system-visual (screen)
  (gdk-screen-system-visual screen))

(export 'gdk-screen-get-system-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgb_colormap ()
;;; 
;;; GdkColormap * gdk_screen_get_rgb_colormap (GdkScreen *screen);
;;; 
;;; Warning
;;; 
;;; gdk_screen_get_rgb_colormap has been deprecated since version 2.22 and
;;; should not be used in newly-written code.
;;; Use gdk_screen_get_system_colormap()
;;; 
;;; Gets the preferred colormap for rendering image data on screen. Not a very
;;; useful function; historically, GDK could only render RGB image data to one
;;; colormap and visual, but in the current version it can render to any
;;; colormap and visual. So there's no need to call this function.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     the preferred colormap
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-rgb-colormap (screen)
  (gdk-screen-rgb-colormap screen))

(export 'gdk-screen-get-rgb-colormap)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgb_visual ()
;;; 
;;; GdkVisual * gdk_screen_get_rgb_visual (GdkScreen *screen);
;;; 
;;; Warning
;;; 
;;; gdk_screen_get_rgb_visual has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use gdk_screen_get_system_visual()
;;; 
;;; Gets a "preferred visual" chosen by GdkRGB for rendering image data on
;;; screen. In previous versions of GDK, this was the only visual GdkRGB could
;;; use for rendering. In current versions, it's simply the visual GdkRGB would
;;; have chosen as the optimal one in those previous versions. GdkRGB can now
;;; render to drawables with any visual.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     the GdkVisual chosen by GdkRGB
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-rgb-visual (screen)
  (gdk-screen-rgb-visual screen))

(export 'gdk-screen-get-rgb-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgba_colormap ()
;;; 
;;; GdkColormap * gdk_screen_get_rgba_colormap (GdkScreen *screen);
;;; 
;;; Gets a colormap to use for creating windows or pixmaps with an alpha
;;; channel. The windowing system on which GTK+ is running may not support this
;;; capability, in which case NULL will be returned. Even if a non-NULL value
;;; is returned, its possible that the window's alpha channel won't be honored
;;; when displaying the window on the screen: in particular, for X an
;;; appropriate windowing manager and compositing manager must be running to
;;; provide appropriate display.
;;; 
;;; This functionality is not implemented in the Windows backend.
;;; 
;;; For setting an overall opacity for a top-level window,
;;; see gdk_window_set_opacity().
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     a colormap to use for windows with an alpha channel or NULL if the
;;;     capability is not available
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-rgba-colormap (screen)
  (gdk-screen-rgba-colormap screen))

(export 'gdk-screen-get-rgba-colormap)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgba_visual ()
;;; 
;;; GdkVisual * gdk_screen_get_rgba_visual (GdkScreen *screen);
;;; 
;;; Gets a visual to use for creating windows or pixmaps with an alpha channel.
;;; See the docs for gdk_screen_get_rgba_colormap() for caveats.
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

(defun gdk-screen-get-rgba-visual (screen)
  (gdk-screen-rgba-visual screen))

(export 'gdk-screen-rgba-visual)

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

(defun gdk-screen-is-composited (screen)
  (gdk-screen-composited-p screen))

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

(defun gdk-screen-get-root-window (screen)
  (gdk-screen-root-window screen))

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

(defun gdk-screen-get-display (screen)
  (gdk-screen-display screen))

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

(defun gdk-screen-get-number (screen)
  (gdk-screen-number screen))

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
;;;     the width of screen in pixels
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-width (screen)
  (gdk-screen-width screen))

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
;;;     the height of screen in pixels
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-height (screen)
  (gdk-screen-height screen))

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
;;;     the width of screen in millimeters
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-width-mm (screen)
  (gdk-screen-width-mm screen))

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
;;;     the heigth of screen in millimeters
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-height-mm (screen)
  (gdk-screen-height-mm screen))

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
;;;     the relevant GdkScreen
;;; 
;;; Returns :
;;;     a list of visuals; the list must be freed, but not its contents
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-list-visuals (screen)
  (gdk-screen-visuals screen))

(export 'gdk-screen-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_toplevel_windows ()
;;; 
;;; GList * gdk_screen_get_toplevel_windows (GdkScreen *screen);
;;; 
;;; Obtains a list of all toplevel windows known to GDK on the screen screen.
;;; A toplevel window is a child of the root window (see
;;; gdk_get_default_root_window()).
;;; 
;;; The returned list should be freed with g_list_free(), but its elements
;;; need not be freed.
;;; 
;;; screen :
;;;     The GdkScreen where the toplevels are located.
;;; 
;;; Returns :
;;;     list of toplevel windows, free with g_list_free()
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gdk-screen-get-toplevel-windows (screen)
  (gdk-screen-toplevel-windows screen))

(export 'gdk-screen-toplevel-windows)

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

(defun gdk-screen-make-display-name (screen)
  (gdk-screen-display-name screen))

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

(defun gdk-screen-get-n-monitors (screen)
  (gdk-screen-n-monitors screen))

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
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     An integer index for the primary monitor, or 0 if none is configured.
;;; 
;;; Since 2.20-obje
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
;;; Note that the size of the entire screen area can be retrieved via
;;; gdk_screen_get_width() and gdk_screen_get_height().
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; monitor_num :
;;;     the monitor number, between 0 and gdk_screen_get_n_monitors (screen)
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
;;; gdk_screen_get_monitor_at_point ()
;;; 
;;; gint gdk_screen_get_monitor_at_point (GdkScreen *screen, gint x, gint y)
;;; 
;;; Returns the monitor number in which the point (x,y) is located.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; x :
;;;     the x coordinate in the virtual screen
;;; 
;;; y :
;;;     the y coordinate in the virtual screen
;;; 
;;; Returns :
;;;     the monitor number in which the point (x,y) lies, or a monitor close
;;;     to (x,y) if the point is not in any monitor
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
;;; gint gdk_screen_get_monitor_at_window (GdkScreen *screen,
;;;                                        GdkWindow *window);
;;; 
;;; Returns the number of the monitor in which the largest area of the bounding
;;; rectangle of window resides.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the monitor number in which most of window is located, or if window
;;;     does not intersect any monitors, a monitor, close to window
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
;;; gint gdk_screen_get_monitor_height_mm (GdkScreen *screen, gint monitor_num)
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
;;; gint gdk_screen_get_monitor_width_mm (GdkScreen *screen, gint monitor_num)
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
;;;     a newly-allocated string containing the name of the monitor, or NULL
;;;     if the name cannot be determined
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_plug_name" gdk-screen-get-monitor-plug-name)
    (g-string :free-from-foreign t)
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-plug-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_broadcast_client_message ()
;;; 
;;; void gdk_screen_broadcast_client_message (GdkScreen *screen,
;;;                                           GdkEvent *event);
;;; 
;;; On X11, sends an X ClientMessage event to all toplevel windows on screen.
;;; 
;;; Toplevel windows are determined by checking for the WM_STATE property, as
;;; described in the Inter-Client Communication Conventions Manual (ICCCM). If
;;; no windows are found with the WM_STATE property set, the message is sent to
;;; all children of the root window.
;;; 
;;; On Windows, broadcasts a message registered with the name
;;; GDK_WIN32_CLIENT_MESSAGE to all top-level windows. The amount of data is
;;; limited to one long, i.e. four bytes.
;;; 
;;; screen :
;;;     the GdkScreen where the event will be broadcasted
;;; 
;;; event :
;;;     the GdkEvent
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_broadcast_client_message"
          gdk-screen-broadcast-client-message) :void
  (screen (g-object gdk-screen))
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-screen-broadcast-client-message)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_setting ()
;;; 
;;; gboolean gdk_screen_get_setting (GdkScreen *screen,
;;;                                  const gchar *name,
;;;                                  GValue *value);
;;; 
;;; Retrieves a desktop-wide setting such as double-click time for the
;;; GdkScreen screen.
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
;;;     TRUE if the setting existed and a value was stored in value,
;;;     FALSE otherwise
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
;;; const cairo_font_options_t * gdk_screen_get_font_options (GdkScreen *screen)
;;; 
;;; Gets any options previously set with gdk_screen_set_font_options().
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; Returns :
;;;     the current font options, or NULL if no default font options have been
;;;     set
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

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
;;;     a cairo_font_options_t, or NULL to unset any previously set default
;;;     font options
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

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
;;;     the current resolution, or -1 if no resolution has been set
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

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

(defun gdk-screen-get-active-window (screen)
  (gdk-screen-active-window screen))

(export 'gdk-screen-get-active-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_window_stack ()
;;; 
;;; GList * gdk_screen_get_window_stack (GdkScreen *screen);
;;; 
;;; Returns a GList of GdkWindows representing the current window stack.
;;; 
;;; On X11, this is done by inspecting the _NET_CLIENT_LIST_STACKING property
;;; on the root window, as described in the Extended Window Manager Hints. If
;;; the window manager does not support the _NET_CLIENT_LIST_STACKING hint,
;;; this function returns NULL.
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

(defun gdk-screen-get-window-stack (screen)
  (gdk-screen-window-stack screen))

(export 'gdk-screen-get-window-stack)

;;; ----------------------------------------------------------------------------
;;; gdk_spawn_on_screen ()
;;; 
;;; gboolean gdk_spawn_on_screen (GdkScreen *screen,
;;;                               const gchar *working_directory,
;;;                               gchar **argv,
;;;                               gchar **envp,
;;;                               GSpawnFlags flags,
;;;                               GSpawnChildSetupFunc child_setup,
;;;                               gpointer user_data,
;;;                               gint *child_pid,
;;;                               GError **error);
;;; 
;;; Warning
;;; 
;;; gdk_spawn_on_screen has been deprecated since version 2.24 and should not
;;; be used in newly-written code. This function is being removed in 3.0. Use
;;; either g_spawn_sync(), g_spawn_async(), or GdkAppLaunchContext instead.
;;; 
;;; Like g_spawn_async(), except the child process is spawned in such an
;;; environment that on calling gdk_display_open() it would be returned a
;;; GdkDisplay with screen as the default screen.
;;; 
;;; This is useful for applications which wish to launch an application on a
;;; specific screen.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; working_directory :
;;;     child's current working directory, or NULL to inherit parent's
;;; 
;;; argv :
;;;     child's argument vector
;;; 
;;; envp :
;;;     child's environment, or NULL to inherit parent's
;;; 
;;; flags :
;;;     flags from GSpawnFlags
;;; 
;;; child_setup :
;;;     function to run in the child just before exec()
;;; 
;;; user_data :
;;;     user data for child_setup
;;; 
;;; child_pid :
;;;     return location for child process ID, or NULL
;;; 
;;; error :
;;;     return location for error
;;; 
;;; Returns :
;;;     TRUE on success, FALSE if error is set
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_spawn_on_screen" %gdk-spawn-on-screen) :boolean
  (screen (g-object gdk-screen))
  (working-directory :string)
  (argv :pointer)
  (envp :pointer)
  (flags g-spawn-flags)
  (child-setup :pointer)
  (user-data :pointer)
  (child-pid (:pointer :int))
  (g-error :pointer))

(defun gdk-spawn-on-screen (screen argv &key working-directory
                                             env
                                             (flags '(:search-path))
                                             with-pipes)
  (unless working-directory (setf working-directory (null-pointer)))
  (with-g-error (err)
    (with-foreign-objects ((pid :int) (stdin :int) (stdout :int) (stderr :int))
      (with-foreign-string-array (argvp argv)
        (if (null env)
            (if with-pipes
                (gdk-spawn-on-screen-with-pipes screen
                                                working-directory
                                                argvp
                                                (null-pointer)
                                                flags
                                                (null-pointer)
                                                (null-pointer)
                                                pid stdin stdout stderr err)
                (%gdk-spawn-on-screen screen
                                      working-directory
                                      argvp
                                      (null-pointer)
                                      flags
                                      (null-pointer)
                                      (null-pointer)
                                      pid err))
            (with-foreign-string-array (envp env)
              (if with-pipes
                  (gdk-spawn-on-screen-with-pipes screen
                                                  working-directory
                                                  argvp envp flags
                                                  (null-pointer) (null-pointer)
                                                  pid stdin stdout stderr err)
                  (%gdk-spawn-on-screen screen
                                        working-directory
                                        argvp envp
                                        flags (null-pointer) (null-pointer)
                                        pid err)))))
      (if with-pipes
          (values (mem-ref pid :int)
                  (mem-ref stdin :int)
                  (mem-ref stdout :int)
                  (mem-ref stderr :int))
          (mem-ref pid :int)))))

(export 'gdk-spawn-on-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_spawn_on_screen_with_pipes ()
;;; 
;;; gboolean gdk_spawn_on_screen_with_pipes (GdkScreen *screen,
;;;                                          const gchar *working_directory,
;;;                                          gchar **argv,
;;;                                          gchar **envp,
;;;                                          GSpawnFlags flags,
;;;                                          GSpawnChildSetupFunc child_setup,
;;;                                          gpointer user_data,
;;;                                          gint *child_pid,
;;;                                          gint *standard_input,
;;;                                          gint *standard_output,
;;;                                          gint *standard_error,
;;;                                          GError **error);
;;; 
;;; Warning
;;; 
;;; gdk_spawn_on_screen_with_pipes has been deprecated since version 2.24 and
;;; should not be used in newly-written code. This function is being removed in
;;; 3.0. Use either g_spawn_async_with_pipes() or GdkAppLaunchContext instead.
;;; 
;;; Like g_spawn_async_with_pipes(), except the child process is spawned in such
;;; an environment that on calling gdk_display_open() it would be returned a
;;; GdkDisplay with screen as the default screen.
;;; 
;;; This is useful for applications which wish to launch an application on a
;;; specific screen.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; working_directory :
;;;     child's current working directory, or NULL to inherit parent's
;;; 
;;; argv :
;;;     child's argument vector
;;; 
;;; envp :
;;;     child's environment, or NULL to inherit parent's
;;; 
;;; flags :
;;;     flags from GSpawnFlags
;;; 
;;; child_setup :
;;;     function to run in the child just before exec()
;;; 
;;; user_data :
;;;     user data for child_setup
;;; 
;;; child_pid :
;;;     return location for child process ID, or NULL
;;; 
;;; standard_input :
;;;     return location for file descriptor to write to child's stdin, or NULL
;;; 
;;; standard_output :
;;;     return location for file descriptor to read child's stdout, or NULL
;;; 
;;; standard_error :
;;;     return location for file descriptor to read child's stderr, or NULL
;;; 
;;; error :
;;;     return location for error
;;; 
;;; Returns :
;;;     TRUE on success, FALSE if an error was set
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_spawn_on_screen_with_pipes" gdk-spawn-on-screen-with-pipes)
    :boolean
  (screen (g-object gdk-screen))
  (working-directory :string)
  (argv :pointer)
  (envp :pointer)
  (flags g-spawn-flags)
  (child-setup :pointer)
  (user-data :pointer)
  (child-pid (:pointer :int))
  (std-input (:pointer :int))
  (std-output (:pointer :int))
  (std-err (:pointer :int))
  (g-error :pointer))

;;; ----------------------------------------------------------------------------
;;; gdk_spawn_command_line_on_screen ()
;;; 
;;; gboolean gdk_spawn_command_line_on_screen (GdkScreen *screen,
;;;                                            const gchar *command_line,
;;;                                            GError **error);
;;; 
;;; Warning
;;; 
;;; gdk_spawn_command_line_on_screen has been deprecated since version 2.24 and
;;; should not be used in newly-written code. This function is being removed in
;;; 3.0. Use either g_spawn_command_line_sync(), g_spawn_command_line_async()
;;; or GdkAppLaunchContext instead.
;;; 
;;; Like g_spawn_command_line_async(), except the child process is spawned in
;;; such an environment that on calling gdk_display_open() it would be returned
;;; a GdkDisplay with screen as the default screen.
;;; 
;;; This is useful for applications which wish to launch an application on a
;;; specific screen.
;;; 
;;; screen :
;;;     a GdkScreen
;;; 
;;; command_line :
;;;     a command line
;;; 
;;; error :
;;;     return location for errors
;;; 
;;; Returns :
;;;     TRUE on success, FALSE if error is set
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_spawn_command_line_on_screen" %gdk-spawn-command-line-on-screen)
    :boolean
  (screen (g-object gdk-screen))
  (command-line :string)
  (error :pointer))

(defun gdk-spawn-command-line-on-screen (screen command-line)
  (with-g-error (err)
    (%gdk-spawn-command-line-on-screen screen command-line err)))

(export 'gdk-spawn-command-line-on-screen)

;;; --- End of file gdk.screen.lisp --------------------------------------------
