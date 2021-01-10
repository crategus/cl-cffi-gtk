;;; ----------------------------------------------------------------------------
;;; gdk.screen.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Object representing a physical screen
;;;
;;; Types and Values
;;;
;;;     GdkScreen
;;;
;;; Functions
;;;
;;;     gdk_screen_get_default
;;;     gdk_screen_get_system_visual
;;;     gdk_screen_get_rgba_visual
;;;     gdk_screen_is_composited
;;;     gdk_screen_get_root_window
;;;     gdk_screen_get_display
;;;     gdk_screen_get_number                              deprecated
;;;     gdk_screen_get_width                               deprecated
;;;     gdk_screen_get_height                              deprecated
;;;     gdk_screen_get_width_mm                            deprecated
;;;     gdk_screen_get_height_mm                           deprecated
;;;     gdk_screen_list_visuals
;;;     gdk_screen_get_toplevel_windows
;;;     gdk_screen_make_display_name                       deprecated
;;;     gdk_screen_get_n_monitors                          deprecated
;;;     gdk_screen_get_primary_monitor                     deprecated
;;;     gdk_screen_get_monitor_geometry                    deprecated
;;;     gdk_screen_get_monitor_workarea                    deprecated
;;;     gdk_screen_get_monitor_at_point                    deprecated
;;;     gdk_screen_get_monitor_at_window                   deprecated
;;;     gdk_screen_get_monitor_height_mm                   deprecated
;;;     gdk_screen_get_monitor_width_mm                    deprecated
;;;     gdk_screen_get_monitor_plug_name                   deprecated
;;;     gdk_screen_get_monitor_scale_factor                deprecated
;;;     gdk_screen_get_setting
;;;     gdk_screen_get_font_options                        Accessor
;;;     gdk_screen_set_font_options                        Accessor
;;;     gdk_screen_get_resolution                          Accessor
;;;     gdk_screen_set_resolution                          Accessor
;;;     gdk_screen_get_active_window                       deprecated
;;;     gdk_screen_get_window_stack
;;;
;;; Properties
;;;
;;;     gpointer    font-options          Read / Write
;;;      gdouble    resolution            Read / Write
;;;
;;; Signals
;;;
;;;         void    composited-changed    Run Last
;;;         void    monitors-changed      Run Last
;;;         void    size-changed          Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkScreen
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
    "font-options" "cairo_font_options_t" t t)
   (resolution
    gdk-screen-resolution
    "resolution" "gdouble" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-screen 'type)
 "@version{2020-9-25}
  @begin{short}
    @sym{gdk-screen} objects are the GDK representation of the screen on which
    windows can be displayed and on which the pointer moves.
  @end{short}
  X11 originally identified screens with physical screens, but nowadays it is
  more common to have a single @sym{gdk-screen} object which combines several
  physical monitors. See the function @fun{gdk-screen-n-monitors}.

  @sym{gdk-screen} is used throughout GDK and GTK+ to specify which screen the
  top level windows are to be displayed on. It is also used to query the screen
  specification and default settings such as the default visual with the
  function @fun{gdk-screen-system-visual} or the dimensions of the physical
  monitors with the function @fun{gdk-screen-monitor-geometry}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"composited-changed\" signal}
      @begin{pre}
 lambda (screen)    : Run Last
      @end{pre}
      The \"composited-changed\" signal is emitted when the composited status
      of the screen changes.
      @begin[code]{table}
        @entry[screen]{The @sym{gdk-screen} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"monitors-changed\" signal}
      @begin{pre}
 lambda (screen)    : Run Last
      @end{pre}
      The \"monitors-changed\" signal is emitted when the number, size or
      position of the monitors attached to the screen change. Only for X11 and
      OS X for now. A future implementation for Win32 may be a possibility.
      @begin[code]{table}
        @entry[screen]{The @sym{gdk-screen} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"size-changed\" signal}
      @begin{pre}
 lambda (screen)    : Run Last
      @end{pre}
      The \"size-changed\" signal is emitted when the pixel width or height of
      a the screen changes.
      @begin[code]{table}
        @entry[screen]{The @sym{gdk-screen} object on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk-screen-font-options}
  @see-slot{gdk-screen-resolution}
  @see-function{gdk-screen-n-monitors}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-monitor-geometry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-screen-font-options ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-options" 'gdk-screen) 't)
 "The @code{font-options} property of type @symbol{cairo-font-options-t}
  (Read / Write) @br{}
  The default font options for the screen.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-screen-font-options atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-screen-font-options 'function)
 "@version{2020-9-18}
  @syntax[]{(gdk-screen-font-options object) => options}
  @syntax[]{(setf (gdk-screen-font-options object) options)}
  @argument[object]{a @class{gdk-screen} object}
  @argument[options]{a @symbol{cairo-font-options-t} structure, or @code{nil}
    to unset any previously set default font options}
  @begin{short}
    Accessor of the @slot[gdk-screen]{font-options} slot of the
    @class{gdk-screen} class.
  @end{short}

  The slot access function @sym{gdk-screen-font-options} returns the current
  font options, or @code{nil} if no default font options have been set. The
  slot access function @sym{(setf gdk-screen-font-options)} sets the default
  font options for the screen.

  These options will be set on any Pango context's newly created with the
  function @fun{gdk-pango-context-for-screen}. Changing the default set
  of font options does not affect contexts that have already been created.
  @see-class{gdk-screen}
  @see-symbol{cairo-font-options-t}
  @see-function{gdk-pango-context-for-screen}")

;;; --- gdk-screen-resolution --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resolution" 'gdk-screen) 't)
 "The @code{resolution} property of type @code{:double} (Read / Write) @br{}
  The resolution for fonts on the screen. @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-screen-resolution atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-screen-resolution 'function)
 "@version{2021-1-10}
  @syntax[]{(gdk-screen-resolution object) => dpi}
  @syntax[]{(setf (gdk-screen-resolution object) dpi)}
  @argument[object]{a @class{gdk-screen} object}
  @argument[dpi]{the resolution of type @code{:double} in \"dots per inch\"}
  @begin{short}
    Accessor of the @slot[gdk-screen]{resolution} slot of the @class{gdk-screen}
    class.
  @end{short}

  The slot access function @sym{gdk-screen-resolution} gets the resolution for
  font handling on the screen, or -1 if no resolution has been set. The slot
  access function @sym{(setf gdk-screen-resolution)} sets the resolution for
  font handling on the screen.

  This is a scale factor between points specified in a
  @class{pango-font-description} structure and Cairo units. The default value
  is 96, meaning that a 10 point font will be 13 units high
  (10 * 96 / 72 = 13.3).
  @see-class{gdk-screen}
  @see-class{pango-font-description}")

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_default () -> gdk-screen-default
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_default" gdk-screen-default) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @begin{return}
    A @class{gdk-screen} object, or @code{nil} if there is no default display.
  @end{return}
  @begin{short}
    Gets the default screen for the default display.
  @end{short}
  See the function @fun{gdk-display-default}.
  @see-class{gdk-screen}
  @see-function{gdk-display-default}")

(export 'gdk-screen-default)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_system_visual () -> gdk-screen-system-visual
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_system_visual" gdk-screen-system-visual)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The system @class{gdk-visual} object.}
  @begin{short}
    Get the system's default visual for the screen.
  @end{short}
  This is the visual for the root window of the display.
  @see-class{gdk-screen}
  @see-class{gdk-visual}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-system-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgba_visual () -> gdk-screen-rgba-visual
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_rgba_visual" gdk-screen-rgba-visual)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    A @class{gdk-visual} object to use for windows with an alpha channel or
    @code{nil} if the capability is not available.
  @end{return}
  @begin{short}
    Gets a visual to use for creating windows with an alpha channel.
  @end{short}
  The windowing system on which GTK+ is running may not support this capability,
  in which case @code{nil} will be returned. Even if a non-@code{nil} value is
  returned, it is possible that the window's alpha channel will not be honored
  when displaying the window on the screen: in particular, for X11 an
  appropriate windowing manager and compositing manager must be running to
  provide appropriate display.

  This functionality is not implemented in the Windows backend.

  For setting an overall opacity for a top-level window, see the function
  @fun{gdk-window-set-opacity}.
  @see-class{gdk-screen}
  @see-class{gdk-visual}
  @see-function{gdk-window-set-opacity}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-rgba-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_is_composited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_is_composited" gdk-screen-is-composited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    A boolean whether windows with RGBA visuals can reasonably be expected to
    have their alpha channels drawn correctly on @arg{screen}.
  @end{return}
  @begin{short}
    Returns whether windows with an RGBA visual can reasonably be expected to
    have their alpha channel drawn correctly on the screen.
  @end{short}

  On X11 this function returns whether a compositing manager is compositing
  the screen.
  @see-class{gdk-screen}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-is-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_root_window () -> gdk-screen-root-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_root_window" gdk-screen-root-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-18}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The root @class{gdk-window} object.}
  @short{Gets the root window of the screen.}
  @see-class{gdk-screen}
  @see-class{gdk-window}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_display () -> gdk-screen-display
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_display" gdk-screen-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The @class{gdk-display} object to which @arg{screen} belongs.}
  @short{Gets the display to which the screen belongs.}
  @see-class{gdk-screen}
  @see-class{gdk-display}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-display)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_number () -> gdk-screen-number
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_number" gdk-screen-number) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the index of @arg{screen}.}
  @begin{short}
    Gets the index of the screen among the screens in the display to which
    it belongs.
  @end{short}
  See the function @fun{gdk-screen-display}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-number} has been deprecated since version
    3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-display}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-number)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width () -> gdk-screen-width
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_width" %gdk-screen-width) :int
  (screen (g-object gdk-screen)))

(defun gdk-screen-width (&optional (screen (gdk-screen-default)))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the width of @arg{screen} in pixels.}
  @short{Gets the width of the screen in pixels.}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-width} has been deprecated since version
    3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-height}
  @see-function{gdk-screen-width-mm}"
  (%gdk-screen-width screen))

(export 'gdk-screen-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height () -> gdk-screen-height
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_height" %gdk-screen-height) :int
  (screen (g-object gdk-screen)))

(defun gdk-screen-height (&optional (screen (gdk-screen-default)))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the height of @arg{screen} in pixels.}
  @short{Gets the height of the screen in pixels.}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-height} has been deprecated since version
    3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-width}
  @see-function{gdk-screen-height-mm}"
  (%gdk-screen-height screen))

(export 'gdk-screen-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width_mm () -> gdk-screen-width-mm
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_width_mm" %gdk-screen-width-mm) :int
  (screen (g-object gdk-screen)))

(defun gdk-screen-width-mm (&optional (screen (gdk-screen-default)))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the width of @arg{screen} in millimeters.}
  @begin{short}
    Gets the width of the screen in millimeters.
  @end{short}
  Note that on some X servers this value will not be correct.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-width-mm} has been deprecated since version
    3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-width}
  @see-function{gdk-screen-height-mm}"
  (%gdk-screen-width-mm screen))

(export 'gdk-screen-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_height_mm" %gdk-screen-height-mm) :int
  (screen (g-object gdk-screen)))

(defun gdk-screen-height-mm (&optional (screen (gdk-screen-default)))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the height of @arg{screen} in millimeters.}
  @begin{short}
    Returns the height of the screen in millimeters.
  @end{short}
  Note that on some X servers this value will not be correct.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-height-mm} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-height}
  @see-function{gdk-screen-width-mm}"
  (%gdk-screen-height-mm screen))

(export 'gdk-screen-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_list_visuals ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_list_visuals" gdk-screen-list-visuals)
    (g-list (g-object gdk-visual) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[screen]{the relevant @class{gdk-screen} object}
  @return{A list of @class{gdk-visual} objects for @arg{screen}.}
  @begin{short}
    Lists the available visuals for the specified screen.
  @end{short}
  A visual describes a hardware image data format. For example, a visual might
  support 24-bit color, or 8-bit color, and might expect pixels to be in a
  certain format.
  @see-class{gdk-screen}
  @see-class{gdk-visual}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_toplevel_windows () -> gdk-screen-toplevel-windows
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_toplevel_windows" gdk-screen-toplevel-windows)
    (g-list (g-object gdk-window :free-from-foreign nil) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[screen]{the @class{gdk-screen} object where the toplevels are
    located}
  @return{List of toplevel @class{gdk-window} objects for @arg{screen}.}
  @begin{short}
    Obtains a list of all toplevel windows known to GDK on the screen.
  @end{short}
  A toplevel window is a child of the root window. See the function
  @fun{gdk-default-root-window}.
  @see-class{gdk-screen}
  @see-class{gdk-window}
  @see-function{gdk-default-root-window}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-toplevel-windows)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_make_display_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_make_display_name" gdk-screen-make-display-name)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{A string with the name of the default display.}
  @begin{short}
    Determines the name to pass to the function @fun{gdk-display-open} to get
    a @class{gdk-display} object with this screen as the default screen.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-make-display-name} has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gdk-window}
  @see-function{gdk-display-open}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-make-display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_n_monitors () -> gdk-screen-n-monitors
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_n_monitors" gdk-screen-n-monitors) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the number of monitors which @arg{screen} consists
    of.}
  @short{Returns the number of monitors which the screen consists of.}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-n-monitors} has been deprecated since version
    3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-display-n-monitors} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-primary-monitor}
  @see-function{gdk-display-n-monitors}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-n-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_primary_monitor () -> gdk-screen-primary-monitor
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_primary_monitor" gdk-screen-primary-monitor) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An integer with the index for the primary monitor, or 0 if none is
    configured.}
  @begin{short}
    Gets the primary monitor for the screen.
  @end{short}
  The primary monitor is considered the monitor where the \"main desktop\"
  lives. While normal application windows typically allow the window manager to
  place the windows, specialized desktop applications such as panels should
  place themselves on the primary monitor.

  If no primary monitor is configured by the user, the return value will be 0,
  defaulting to the first monitor.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-primary-monitor} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-display-primary-monitor} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-n-monitors}
  @see-function{gdk-display-primary-monitor}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-primary-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_geometry () -> gdk-screen-monitor-geometry
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_geometry" %gdk-screen-monitor-geometry) :void
  (screen (g-object gdk-screen))
  (monitor-num :int)
  (dest (g-boxed-foreign gdk-rectangle)))

(defun gdk-screen-monitor-geometry (screen monitor-num)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{an integer with the monitor number}
  @return{A @class{gdk-rectangle} structure filled with the monitor geometry.}
  @begin{short}
    Retrieves the rectangle representing the size and position of the
    individual monitor within the entire screen area.
  @end{short}
  Monitor numbers start at 0. To obtain the number of monitors of the screen,
  use the function @fun{gdk-screen-n-monitors}.

  Note that the size of the entire screen area can be retrieved via the
  functions @fun{gdk-screen-width} and @fun{gdk-screen-height}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-geometry} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-monitor-geometry} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-n-monitors}
  @see-function{gdk-screen-width}
  @see-function{gdk-screen-height}"
  (let ((dest (make-gdk-rectangle)))
    (%gdk-screen-monitor-geometry screen monitor-num dest)
    dest))

(export 'gdk-screen-monitor-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_workarea () -> gdk-screen-monitor-workarea
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_workarea" %gdk-screen-monitor-workarea) :void
  (screen (g-object gdk-screen))
  (monitor-num :int)
  (dest (g-boxed-foreign gdk-rectangle)))

(defun gdk-screen-monitor-workarea (screen monitor-num)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{an integer with the monitor number}
  @return{A @class{gdk-rectangle} structure filled with the monitor workarea.}
  @begin{short}
    Retrieves the rectangle representing the size and position of the
    \"work area\" on a monitor within the entire screen area.
  @end{short}

  The work area should be considered when positioning menus and similar
  popups, to avoid placing them below panels, docks or other desktop
  components.

  Monitor numbers start at 0. To obtain the number of monitors of the screen,
  use the function @fun{gdk-screen-n-monitors}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-workarea} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-monitor-workarea} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-n-monitors}"
  (let ((dest (make-gdk-rectangle)))
    (%gdk-screen-monitor-workarea screen monitor-num dest)
    dest))

(export 'gdk-screen-monitor-workarea)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_point () -> gdk-screen-monitor-at-point
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_at_point" gdk-screen-monitor-at-point) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[x]{an integer with the x coordinate in the virtual @arg{screen}}
  @argument[y]{an integer with the y coordinate in the virtual @arg{screen}}
  @begin{return}
    The monitor number in which the point (@arg{x}, @arg{y}) lies, or a monitor
    close to the point if not in any monitor.
  @end{return}
  @begin{short}
    Returns the monitor number in which the point (@arg{x}, @arg{y}) is located.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-at-point} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-display-monitor-at-point} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-monitor-at-window}
  @see-function{gdk-display-monitor-at-point}"
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-screen-monitor-at-point)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_window () -> gdk-screen-monitor-at-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_at_window" gdk-screen-monitor-at-window) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    An integer with the monitor number in which most of @arg{window} is
    located, or if @arg{window} does not intersect any monitors, a monitor,
    close to @arg{window}.
  @end{return}
  @begin{short}
    Returns the number of the monitor in which the largest area of the bounding
    rectangle of the window resides.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-at-window} has been deprecated
    since version 3.22 and should not be used in newly-written code. Use the
    function @fun{gdk-display-monitor-at-window} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gdk-window}
  @see-function{gdk-screen-monitor-at-point}
  @see-function{gdk-display-monitor-at-window}"
  (screen (g-object gdk-screen))
  (window (g-object gdk-window)))

(export 'gdk-screen-monitor-at-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_height_mm () -> gdk-screen-monitor-height-mm
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_height_mm" gdk-screen-monitor-height-mm) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{an integer with the number of the monitor, between
    0 and and the result of @code{(gdk-screen-n-monitors @arg{screen})}}
  @return{An integer with the height of the monitor, or -1 if not available.}
  @begin{short}
    Gets the height in millimeters of the specified monitor.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-height-mm} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the
    functon @fun{gdk-monitor-height-mm} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-monitor-width-mm}
  @see-function{gdk-monitor-height-mm}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-monitor-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_width_mm () -> gdk-screen-monitor-width-mm
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_width_mm" gdk-screen-monitor-width-mm) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{an integer with the number of the monitor, between
    0 and and the result of @code{(gdk-screen-n-monitors @arg{screen})}}
  @return{An integer with the width of the monitor, or -1 if not available.}
  @begin{short}
    Gets the width in millimeters of the specified monitor, if available.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-width-mm} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-monitor-width-mm} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-monitor-height-mm}
  @see-function{gdk-monitor-width-mm}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-monitor-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_plug_name () -> gdk-screen-monitor-plug-name
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_monitor_plug_name" gdk-screen-monitor-plug-name)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[monitor-num]{an integer with the number of the monitor, between
    0 and the result of @code{(gdk-screen-n-monitors @arg{screen})}}
  @begin{return}
    A string containing the name of the monitor, or @code{nil} if the name
    cannot be determined.
  @end{return}
  @begin{short}
    Returns the output name of the specified monitor. Usually something like
    VGA, DVI, or TV, not the actual product name of the display device.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-plug-name} function has been deprecated
    since version 3.22 and should not be used in newly-written code. Use the
    function @fun{gdk-monitor-model} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-monitor-model}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-monitor-plug-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_scale_factor () -> gdk-screen-monitor-scale-factor
;;; ----------------------------------------------------------------------------

#+gdk-3-10
(defcfun ("gdk_screen_get_monitor_scale_factor" gdk-screen-monitor-scale-factor)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object to get scale factor for}
  @argument[monitor-num]{an integer with the number of the monitor, between
    0 and @code{(gdk-screen-n-monitors @arg{screen})}}
  @return{An integer with the scale factor.}
  @begin{short}
    Returns the internal scale factor that maps from monitor coordiantes to the
    actual device pixels.
  @end{short}
  On traditional systems this is 1, but on very high density outputs this can
  be a higher value (often 2).

  This can be used if you want to create pixel based data for a particula
  monitor, but most of the time you are drawing to a window where it is better
  to use the function @fun{gdk-window-scale-factor} instead.

  Since 3.10
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-monitor-scale-factor} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-monitor-scale-factor} instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-monitor-scale-factor}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

#+gdk-3-10
(export 'gdk-screen-monitor-scale-factor)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_setting () -> gdk-screen-setting
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_setting" %gdk-screen-setting) :boolean
  (screen (g-object gdk-screen))
  (name :string)
  (value (:pointer (:struct g-value))))

(defun gdk-screen-setting (screen name gtype)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{the @class{gdk-screen} object where the setting is located}
  @argument[name]{a string with the name of the setting}
  @argument[gtype]{a string with the @code{GType} of the setting}
  @begin{return}
    The value of the setting, or @code{nil} if the setting does not exist.
  @end{return}
  @begin{short}
    Retrieves a desktop wide setting such as double-click time for the screen.
  @end{short}

  See the @class{gtk-settings} class for the available settings.
  @begin[Example]{dictionary}
    @begin{pre}
 (let ((screen (gdk-display-default-screen (gdk-display-default))))
   (gdk-screen-setting screen \"gtk-double-click-time\" \"gint\"))
=> 400
    @end{pre}
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gtk-settings}"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value gtype)
    (when (%gdk-screen-setting screen name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-screen-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_active_window () -> gdk-screen-active-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_active_window" gdk-screen-active-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The currently active @class{gdk-window} object, or @code{nil}.}
  @begin{short}
    Returns the screen's currently active window.
  @end{short}

  On X11, this is done by inspecting the @code{_NET_ACTIVE_WINDOW} property on
  the root window, as described in the Extended Window Manager Hints. If there
  is no currently currently active window, or the window manager does not
  support the @code{_NET_ACTIVE_WINDOW} hint, this function returns @code{nil}.

  On other platforms, this function may return @code{nil}, depending on whether
  it is implementable on that platform.
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-active-window} has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gdk-window}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-active-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_window_stack () -> gdk-screen-window-stack
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_window_stack" gdk-screen-window-stack)
    (g-list (g-object gdk-window :free-from-foreign t) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-8}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    A list of @class{gdk-window} objects for the current window stack, or
    @code{nil}.
  @end{return}
  @begin{short}
    Returns a list of windows representing the current window stack.
  @end{short}

  On X11, this is done by inspecting the @code{_NET_CLIENT_LIST_STACKING}
  property on the root window, as described in the Extended Window Manager
  Hints. If the window manager does not support the
  @code{_NET_CLIENT_LIST_STACKING} hint, this function returns @code{nil}.

  On other platforms, this function may return @code{nil}, depending on whether
  it is implementable on that platform.
  @see-class{gdk-screen}
  @see-class{gdk-window}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-window-stack)

;;; --- End of file gdk.screen.lisp --------------------------------------------
