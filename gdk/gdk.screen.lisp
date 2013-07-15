;;; ----------------------------------------------------------------------------
;;; gdk.screen.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkScreen
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-screen 'type)
 "@version{2013-6-17}
  @begin{short}
    @sym{gdk-screen} objects are the GDK representation of the screen on which
    windows can be displayed and on which the pointer moves. X originally
    identified screens with physical screens, but nowadays it is more common to
    have a single @sym{gdk-screen} which combines several physical monitors.
    See the function @fun{gdk-screen-get-n-monitors}.
  @end{short}

  @sym{gdk-screen} is used throughout GDK and GTK+ to specify which screen the
  top level windows are to be displayed on. It is also used to query the screen
  specification and default settings such as the default visual with the
  function @fun{gdk-screen-get-system-visual} or the dimensions of the physical
  monitors with the function @fun{gdk-screen-get-monitor-geometry}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"composited-changed\" signal}
      @begin{pre}
 lambda (screen)   : Run Last
      @end{pre}
      The \"composited-changed\" signal is emitted when the composited status
      of the @arg{screen} changes.
      @begin[code]{table}
        @entry[screen]{The @sym{gdk-screen} object on which the signal is
          emitted.}
      @end{table}
      Since 2.10

    @subheading{The \"monitors-changed\" signal}
      @begin{pre}
 lambda (screen)   : Run Last
      @end{pre}
      The \"monitors-changed\" signal is emitted when the number, size or
      position of the monitors attached to the @arg{screen} change.
      Only for X11 and OS X for now. A future implementation for Win32 may be
      a possibility.
      @begin[code]{table}
        @entry[screen]{The @sym{gdk-screen} object on which the signal is
          emitted.}
      @end{table}
      Since 2.14

    @subheading{The \"size-changed\" signal}
      @begin{pre}
 lambda (screen)   : Run Last
      @end{pre}
      The \"size-changed\" signal is emitted when the pixel width or height of
      a @arg{screen} changes.
      @begin[code]{table}
        @entry[screen]{The @sym{gdk-screen} object on which the signal is
          emitted.}
      @end{table}
      Since 2.2
  @end{dictionary}
  @see-slot{gdk-screen-font-options}
  @see-slot{gdk-screen-resolution}
  @see-function{gdk-screen-get-n-monitors}
  @see-function{gdk-screen-get-system-visual}
  @see-function{gdk-screen-get-monitor-geometry}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-options" 'gdk-screen) 't)
 "The @code{\"font-options\"} property of type @code{:pointer}
  (Read / Write) @br{}
  The default font options for the screen.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resolution" 'gdk-screen) 't)
 "The @code{\"resolution\"} property of type @code{:double} (Read / Write) @br{}
  The resolution for fonts on the screen. @br{}
  Default value: -1")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-screen-font-options atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-screen-font-options 'function)
 "@version{2013-6-30}
  Accessor of the slot @code{\"font-options\"} of the @class{gdk-screen}
  class.
  @see-function{gdk-screen-get-font-options}
  @see-function{gdk-screen-set-font-options}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-screen-resolution atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-screen-resolution 'function)
 "@version{2013-6-30}
  Accessor of the slot @code{\"resolution\"} of the @class{gdk-screen}
  class.
  @see-function{gdk-screen-get-resolution}
  @see-function{gdk-screen-set-resolution}")

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_default" gdk-screen-get-default)
    (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @begin{return}
    A @class{gdk-screen} object, or @code{nil} if there is no default display.
  @end{return}
  @begin{short}
    Gets the default screen for the default display.
  @end{short}
  See the function @fun{gdk-display-get-default}.

  Since 2.2
  @see-function{gdk-display-get-default}")

(export 'gdk-screen-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_system_visual ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_system_visual" gdk-screen-get-system-visual)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The system visual.}
  @begin{short}
    Get the system's default visual for @arg{screen}. This is the visual for the
    root window of the display. The return value should not be freed.
  @end{short}

  Since 2.2"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-system-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgba_visual ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_rgba_visual" gdk-screen-get-rgba-visual)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    A visual to use for windows with an alpha channel or @code{nil} if the
    capability is not available.
  @end{return}
  @begin{short}
    Gets a visual to use for creating windows with an alpha channel.
  @end{short}
  The windowing system on which GTK+ is running may not support this capability,
  in which case @code{nil} will be returned. Even if a non-@code{nil} value is
  returned, it is possible that the window's alpha channel will not be honored
  when displaying the window on the screen: in particular, for X an appropriate
  windowing manager and compositing manager must be running to provide
  appropriate display.

  This functionality is not implemented in the Windows backend.

  For setting an overall opacity for a top-level window, see the function
  @fun{gdk-window-set-opacity}.

  Since 2.8
  @see-function{gdk-window-set-opacity}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-rgba-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_is_composited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_is_composited" gdk-screen-is-composited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    Whether windows with RGBA visuals can reasonably be expected to have
    their alpha channels drawn correctly on the @arg{screen}.
  @end{return}
  @begin{short}
    Returns whether windows with an RGBA visual can reasonably be expected to
    have their alpha channel drawn correctly on the @arg{screen}.
  @end{short}

  On X11 this function returns whether a compositing manager is compositing
  @arg{screen}.

  Since 2.10"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-is-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_root_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_root_window" gdk-screen-get-root-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The root window.}
  @short{Gets the root window of @arg{screen}.}

  Since 2.2"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_display" gdk-screen-get-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The display to which @arg{screen} belongs.}
  @short{Gets the display to which the @arg{screen} belongs.}

  Since 2.2"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_number ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_number" gdk-screen-get-number) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The index of @arg{screen}.}
  @begin{short}
    Gets the index of @arg{screen} among the screens in the display to which
    it belongs.
  @end{short}
  See the function @fun{gdk-screen-get-display}.

  Since 2.2
  @see-function{gdk-screen-get-display}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-number)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_width" gdk-screen-get-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The width of @arg{screen} in pixels.}
  @short{Gets the width of @arg{screen} in pixels.}

  Since 2.2
  @see-function{gdk-screen-get-height}
  @see-function{gdk-screen-get-width-mm}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_height" gdk-screen-get-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The height of @arg{screen} in pixels.}
  @short{Gets the height of @arg{screen} in pixels.}

  Since 2.2
  @see-function{gdk-screen-get-width}
  @see-function{gdk-screen-get-height-mm}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_width_mm" gdk-screen-get-width-mm) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The width of @arg{screen} in millimeters.}
  @begin{short}
    Gets the width of @arg{screen} in millimeters.
  @end{short}
  Note that on some X servers this value will not be correct.

  Since 2.2
  @see-function{gdk-screen-get-width}
  @see-function{gdk-screen-get-height-mm}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_height_mm" gdk-screen-get-height-mm) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The height of @arg{screen} in millimeters.}
  @begin{short}
    Returns the height of @arg{screen} in millimeters.
  @end{short}
  Note that on some X servers this value will not be correct.

  Since 2.2
  @see-function{gdk-screen-get-height}
  @see-function{gdk-screen-get-width-mm}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_list_visuals ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_list_visuals" gdk-screen-list-visuals)
    (g-list (g-object gdk-visual) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{the relevant @class{gdk-screen} object}
  @return{A list of visuals for @arg{screen}.}
  @begin{short}
    Lists the available visuals for the specified @arg{screen}.
  @end{short}
  A visual describes a hardware image data format. For example, a visual might
  support 24-bit color, or 8-bit color, and might expect pixels to be in a
  certain format.

  Since 2.2"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_toplevel_windows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_toplevel_windows" gdk-screen-get-toplevel-windows)
    (g-list (g-object gdk-window) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{the @class{gdk-screen} object where the toplevels are
    located}
  @return{List of toplevel windows for @arg{screen}.}
  @begin{short}
    Obtains a list of all toplevel windows known to GDK on the screen
    @arg{screen}.
  @end{short}
  A toplevel window is a child of the root window. See the function
  @fun{gdk-get-default-root-window}.

  Since 2.2
  @see-function{gdk-get-default-root-window}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-toplevel-windows)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_make_display_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_make_display_name" gdk-screen-make-display-name)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{A newly allocated string.}
  @begin{short}
    Determines the name to pass to the function @fun{gdk-display-open} to get a
    @class{gdk-display} object with this @arg{screen} as the default screen.
  @end{short}

  Since 2.2
  @see-function{gdk-display-open}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-make-display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_n_monitors ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_n_monitors" gdk-screen-get-n-monitors) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{Number of monitors which @arg{screen} consists of.}
  @short{Returns the number of monitors which @arg{screen} consists of.}

  Since 2.2
  @see-function{gdk-screen-get-primary-monitor}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-n-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_primary_monitor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_primary_monitor" gdk-screen-get-primary-monitor) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer index for the primary monitor, or 0 if none is configured.}
  @begin{short}
    Gets the primary monitor for @arg{screen}.
  @end{short}
  The primary monitor is considered the monitor where the \"main desktop\"
  lives. While normal application windows typically allow the window manager to
  place the windows, specialized desktop applications such as panels should
  place themselves on the primary monitor.

  If no primary monitor is configured by the user, the return value will be 0,
  defaulting to the first monitor.

  Since 2.20
  @see-function{gdk-screen-get-n-monitors}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-primary-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_geometry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_geometry" %gdk-screen-get-monitor-geometry)
    :void
  (screen (g-object gdk-screen))
  (monitor-num :int)
  (dest (g-boxed-foreign gdk-rectangle)))

(defun gdk-screen-get-monitor-geometry (screen monitor-num)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{the monitor number}
  @return{A @class{gdk-rectangle} filled with the monitor geometry.}
  @begin{short}
    Retrieves the @class{gdk-rectangle} representing the size and position of
    the individual monitor within the entire screen area.
  @end{short}

  Monitor numbers start at 0. To obtain the number of monitors of @arg{screen},
  use the function @fun{gdk-screen-get-n-monitors}.

  Note that the size of the entire screen area can be retrieved via the
  functions @fun{gdk-screen-get-width} and @fun{gdk-screen-get-height}.

  Since 2.2
  @see-function{gdk-screen-get-n-monitors}
  @see-function{gdk-screen-get-width}
  @see-function{gdk-screen-get-height}"
  (let ((dest (make-gdk-rectangle)))
    (%gdk-screen-get-monitor-geometry screen monitor-num dest)
    dest))

(export 'gdk-screen-get-monitor-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_workarea ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_workarea" %gdk-screen-get-monitor-workarea)
    :void
  (screen (g-object gdk-screen))
  (monitor-num :int)
  (dest (g-boxed-foreign gdk-rectangle)))

(defun gdk-screen-get-monitor-workarea (screen monitor-num)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{the monitor number}
  @return{A @class{gdk-rectangle} filled with the monitor workarea.}
  @begin{short}
    Retrieves the @class{gdk-rectangle} representing the size and position of
    the \"work area\" on a monitor within the entire screen area.
  @end{short}

  The work area should be considered when positioning menus and similar
  popups, to avoid placing them below panels, docks or other desktop
  components.

  Monitor numbers start at 0. To obtain the number of monitors of @arg{screen},
  use the function @fun{gdk-screen-get-n-monitors}.

  Since 3.4
  @see-function{gdk-screen-get-n-monitors}"
  (let ((dest (make-gdk-rectangle)))
    (%gdk-screen-get-monitor-workarea screen monitor-num dest)
    dest))

(export 'gdk-screen-get-monitor-workarea)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_at_point" gdk-screen-get-monitor-at-point)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[x]{the x coordinate in the virtual @arg{screen}}
  @argument[y]{the y coordinate in the virtual @arg{screen}}
  @begin{return}
    The monitor number in which the point (x,y) lies, or a monitor close to
    (x,y) if the point is not in any monitor.
  @end{return}
  @begin{short}
    Returns the monitor number in which the point (x,y) is located.
  @end{short}

  Since 2.2
  @see-function{gdk-screen-get-monitor-at-window}"
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-screen-get-monitor-at-point)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_at_window" gdk-screen-get-monitor-at-window)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    The monitor number in which most of @arg{window} is located, or if
    @arg{window} does not intersect any monitors, a monitor, close to
    @arg{window}.
  @end{return}
  @begin{short}
    Returns the number of the monitor in which the largest area of the bounding
    rectangle of @arg{window} resides.
  @end{short}

  Since 2.2
  @see-function{gdk-screen-get-monitor-at-point}"
  (screen (g-object gdk-screen))
  (window (g-object gdk-window)))

(export 'gdk-screen-get-monitor-at-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_height_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_height_mm" gdk-screen-get-monitor-height-mm)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{number of the monitor, between 0 and
    and the result of @code{(gdk-screen-get-n-monitors @arg{screen})}}
  @return{The height of the monitor, or -1 if not available.}
  @begin{short}
    Gets the height in millimeters of the specified monitor.
  @end{short}

  Since 2.14
  @see-function{gdk-screen-get-monitor-width-mm}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_width_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_width_mm" gdk-screen-get-monitor-width-mm)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{number of the monitor, between 0 and
    and the result of @code{(gdk-screen-get-n-monitors @arg{screen})}}
  @return{The width of the monitor, or -1 if not available.}
  @begin{short}
    Gets the width in millimeters of the specified monitor, if available.
  @end{short}

  Since 2.14
  @see-function{gdk-screen-get-monitor-height-mm}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_plug_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_plug_name" gdk-screen-get-monitor-plug-name)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{number of the monitor, between 0 and
    the result of @code{(gdk-screen-get-n-monitors @arg{screen})}}
  @begin{return}
    A string containing the name of the monitor, or @code{nil} if the name
    cannot be determined.
  @end{return}
  @begin{short}
    Returns the output name of the specified monitor. Usually something like
    VGA, DVI, or TV, not the actual product name of the display device.
  @end{short}

  Since 2.14"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-plug-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_setting ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_setting" %gdk-screen-get-setting) :boolean
  (screen (g-object gdk-screen))
  (name :string)
  (value :pointer))

(defun gdk-screen-get-setting (screen name)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{the @class{gdk-screen} object where the setting is located}
  @argument[name]{the name of the setting}
  @begin{return}
    @code{value} -- the value of the setting, or @code{nil} if the setting does
    not exist.
  @end{return}
  @begin{short}
    Retrieves a desktop wide setting such as double-click time for the
    @class{gdk-screen} @arg{screen}.
  @end{short}

  FIXME needs a list of valid settings here, or a link to more information.

  Since 2.2"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value)
    (when (%gdk-screen-get-setting screen name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-screen-get-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_font_options ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-get-font-options))

(defun gdk-screen-get-font-options (screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    The current font options, or @code{nil} if no default font options have
    been set.
  @end{return}
  @begin{short}
    Gets any options previously set with the function
    @fun{gdk-screen-set-font-options}.
  @end{short}

  Since 2.10
  @see-function{gdk-screen-set-font-options}"
  (gdk-screen-font-options screen))

(export 'gdk-screen-get-font-options)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_set_font_options ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-set-font-options))

(defun gdk-screen-set-font-options (screen options)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[options]{a @symbol{cairo-font-options-t}, or @code{nil} to unset
    any previously set default font options}
  @begin{short}
    Sets the default font options for the @arg{screen}.
  @end{short}
  These options will be set on any @class{pango-context}'s newly created with
  the function @fun{gdk-pango-context-get-for-screen}. Changing the default set
  of font options does not affect contexts that have already been created.

  Since 2.10
  @see-function{gdk-screen-get-font-options}
  @see-function{gdk-pango-context-get-for-screen}"
  (setf (gdk-screen-font-options screen) options))

(export 'gdk-screen-set-font-options)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_resolution ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-get-resolution))

(defun gdk-screen-get-resolution (screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The current resolution, or -1 if no resolution has been set.}
  @begin{short}
    Gets the resolution for font handling on the @arg{screen}.
  @end{short}
  See the function @fun{gdk-screen-set-resolution} for full details.

  Since 2.10
  @see-function{gdk-screen-set-resolution}"
  (gdk-screen-resolution screen))

(export 'gdk-screen-get-resolution)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_set_resolution ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-screen-set-resolution))

(defun gdk-screen-set-resolution (screen dpi)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[dpi]{the resolution in \"dots per inch\". (Physical inches are not
    actually involved; the terminology is conventional.)}
  @begin{short}
    Sets the resolution for font handling on the @arg{screen}.
  @end{short}
  This is a scale factor between points specified in a
  @class{pango-font-description} and cairo units. The default value is 96,
  meaning that a 10 point font will be 13 units high. (10 * 96. / 72. = 13.3).

  Since 2.10
  @see-class{pango-font-description}"
  (setf (gdk-screen-resolution screen) dpi))

(export 'gdk-screen-set-resolution)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_active_window ()
;;; ----------------------------------------------------------------------------

;; The returned window should be unrefed.

(defcfun ("gdk_screen_get_active_window" gdk-screen-get-active-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The currently active window, or @code{nil}.}
  @begin{short}
    Returns the @arg{screen}'s currently active window.
  @end{short}

  On X11, this is done by inspecting the @code{_NET_ACTIVE_WINDOW} property on
  the root window, as described in the Extended Window Manager Hints. If there
  is no currently currently active window, or the window manager does not
  support the @code{_NET_ACTIVE_WINDOW} hint, this function returns @code{nil}.

  On other platforms, this function may return @code{nil}, depending on whether
  it is implementable on that platform.

  The returned window should be unrefed using the function @fun{g-object-unref}
  when no longer needed.

  Since 2.10"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-active-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_window_stack ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_window_stack" gdk-screen-get-window-stack)
    (g-list (g-object gdk-window) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    A list of @class{gdk-window} objects for the current window stack, or
    @code{nil}.
  @end{return}
  @begin{short}
    Returns a list of @class{gdk-window} objects representing the current
    window stack.
  @end{short}

  On X11, this is done by inspecting the @code{_NET_CLIENT_LIST_STACKING}
  property on the root window, as described in the Extended Window Manager
  Hints. If the window manager does not support the
  @code{_NET_CLIENT_LIST_STACKING} hint, this function returns @code{nil}.

  On other platforms, this function may return @code{nil}, depending on whether
  it is implementable on that platform.

  Since 2.10"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-window-stack)

;;; --- End of file gdk.screen.lisp --------------------------------------------
