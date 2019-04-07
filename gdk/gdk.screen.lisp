;;; ----------------------------------------------------------------------------
;;; gdk.screen.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     gdk_screen_get_number                              * deprecated
;;;     gdk_screen_get_width                               * deprecated
;;;     gdk_screen_get_height                              * deprecated
;;;     gdk_screen_get_width_mm                            * deprecated
;;;     gdk_screen_get_height_mm                           * deprecated
;;;     gdk_screen_list_visuals
;;;     gdk_screen_get_toplevel_windows
;;;     gdk_screen_make_display_name                       * deprecated
;;;     gdk_screen_get_n_monitors                          * deprecated
;;;     gdk_screen_get_primary_monitor                     * deprecated
;;;     gdk_screen_get_monitor_geometry                    * deprecated
;;;     gdk_screen_get_monitor_workarea                    * deprecated
;;;     gdk_screen_get_monitor_at_point                    * deprecated
;;;     gdk_screen_get_monitor_at_window                   * deprecated
;;;     gdk_screen_get_monitor_height_mm                   * deprecated
;;;     gdk_screen_get_monitor_width_mm                    * deprecated
;;;     gdk_screen_get_monitor_plug_name                   * deprecated
;;;     gdk_screen_get_monitor_scale_factor                * deprecated
;;;     gdk_screen_get_setting
;;;     gdk_screen_get_font_options                          Accessor
;;;     gdk_screen_set_font_options                          Accessor
;;;     gdk_screen_get_resolution                            Accessor
;;;     gdk_screen_set_resolution                            Accessor
;;;     gdk_screen_get_active_window                       * deprecated
;;;     gdk_screen_get_window_stack
;;;
;;; Properties
;;;
;;;     gpointer   font-options          Read / Write
;;;      gdouble   resolution            Read / Write
;;;
;;; Signals
;;;
;;;         void   composited-changed    Run Last
;;;         void   monitors-changed      Run Last
;;;         void   size-changed          Run Last
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
 "@version{2013-6-17}
  @begin{short}
    @sym{gdk-screen} objects are the GDK representation of the screen on which
    windows can be displayed and on which the pointer moves.
  @end{short}
  X11 originally identified screens with physical screens, but nowadays it is
  more common to have a single @sym{gdk-screen} object which combines several
  physical monitors. See the @fun{gdk-screen-get-n-monitors} function.

  @sym{gdk-screen} is used throughout GDK and GTK+ to specify which screen the
  top level windows are to be displayed on. It is also used to query the screen
  specification and default settings such as the default visual with the
  function @fun{gdk-screen-get-system-visual} or the dimensions of the physical
  monitors with the function @fun{gdk-screen-get-monitor-geometry}.
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
      position of the monitors attached to the screen change.
      Only for X11 and OS X for now. A future implementation for Win32 may be
      a possibility.
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
  @see-function{gdk-screen-get-n-monitors}
  @see-function{gdk-screen-get-system-visual}
  @see-function{gdk-screen-get-monitor-geometry}")

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
 "@version{2015-12-30}
  @syntax[]{(gdk-screen-font-options object) => options}
  @syntax[]{(setf (gdk-screen-font-options object) options)}
  @argument[object]{a @class{gdk-screen} object}
  @argument[options]{a @symbol{cairo-font-options-t} structure, or @code{nil}
    to unset any previously set default font options}
  @begin{short}
    Accessor of the slot @slot[gdk-screen]{font-options} of the
    @class{gdk-screen} class.
  @end{short}

  The slot access function @sym{gdk-screen-font-options} returns the current
  font options, or @code{nil} if no default font options have been set.

  The slot access function @sym{(setf gdk-screen-font-options)} sets the default
  font options for the screen.

  These options will be set on any Pango context's newly created with
  the @fun{gdk-pango-context-get-for-screen} function. Changing the default set
  of font options does not affect contexts that have already been created.
  @see-class{gdk-screen}
  @see-function{gdk-pango-context-get-for-screen}")

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
 "@version{2015-12-30}
  @syntax[]{(gdk-screen-resolution object) => dpi}
  @syntax[]{(setf (gdk-screen-resolution object) dpi)}
  @argument[object]{a @class{gdk-screen} object}
  @argument[dpi]{the resolution of type @code{:double} in \"dots per inch\".}
  @begin{short}
    Accessor of the slot @slot[gdk-screen]{resolution} of the @class{gdk-screen}
    class.
  @end{short}

  The slot access function @sym{gdk-screen-resolution} gets the resolution for
  font handling on the screen, or -1 if no resolution has been set.

  The slot access function @sym{(setf gdk-screen-resolution)} sets the
  resolution for font handling on the screen.

  This is a scale factor between points specified in a
  @class{pango-font-description} structure and cairo units. The default value is
  96, meaning that a 10 point font will be 13 units high
  (10 * 96. / 72. = 13.3).
  @see-class{gdk-screen}
  @see-class{pango-font-description}")

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
  See the @fun{gdk-display-get-default} function.
  @see-class{gdk-screen}
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
  @return{The system @class{gdk-visual} object.}
  @begin{short}
    Get the system's default visual for the screen.
  @end{short}
  This is the visual for the root window of the display.
  @see-class{gdk-screen}"
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

  For setting an overall opacity for a top-level window, see the
  @fun{gdk-window-set-opacity} function.
  @see-class{gdk-screen}
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
    A @code{:boolean}, whether windows with RGBA visuals can reasonably be
    expected to have their alpha channels drawn correctly on @arg{screen}.
  @end{return}
  @begin{short}
    Returns whether windows with an RGBA visual can reasonably be expected to
    have their alpha channel drawn correctly on the screen.
  @end{short}

  On X11 this function returns whether a compositing manager is compositing
  @arg{screen}.
  @see-class{gdk-screen}"
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
  @return{The root @class{gdk-window} object.}
  @short{Gets the root window of the screen.}
  @see-class{gdk-screen}
  @see-class{gdk-window}"
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
  @return{The @class{gdk-display} object to which @arg{screen} belongs.}
  @short{Gets the display to which the screen belongs.}
  @see-class{gdk-screen}
  @see-class{gdk-display}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_number ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_number" gdk-screen-get-number) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{The @code{:int} index of @arg{screen}.}
  @begin{short}
    Gets the index of the screen among the screens in the display to which
    it belongs.
  @end{short}
  See the @fun{gdk-screen-get-display} function.
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-get-number} function has been deprecated since version
    3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-width} has been deprecated since version
    3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-height} has been deprecated since version
    3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-width-mm} has been deprecated since version
    3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-height-mm} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use per-monitor
    information instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @return{A list of @class{gdk-visual} objects for @arg{screen}.}
  @begin{short}
    Lists the available visuals for the specified screen.
  @end{short}
  A visual describes a hardware image data format. For example, a visual might
  support 24-bit color, or 8-bit color, and might expect pixels to be in a
  certain format.
  @see-class{gdk-screen}"
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
  @return{List of toplevel @class{gdk-window} objects for @arg{screen}.}
  @begin{short}
    Obtains a list of all toplevel windows known to GDK on the screen.
  @end{short}
  A toplevel window is a child of the root window. See the
  @fun{gdk-get-default-root-window} function.
  @see-class{gdk-screen}
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
  @return{A string.}
  @begin{short}
    Determines the name to pass to the @fun{gdk-display-open} function to get a
    @class{gdk-display} object with this screen as the default screen.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-make-display-name} function has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @return{Number of monitors of type @code{:int} which @arg{screen} consists
    of.}
  @short{Returns the number of monitors which the screen consists of.}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-n-monitors} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the
    @fun{gdk-display-get-n-monitors} function instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-get-primary-monitor}
  @see-function{gdk-display-get-n-monitors}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-n-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_primary_monitor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_primary_monitor" gdk-screen-get-primary-monitor) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{a @class{gdk-screen} object}
  @return{An @code{:int} index for the primary monitor, or 0 if none is
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
    The function @sym{gdk-screen-get-primary-monitor} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the
    @fun{gdk-display-get-primary-monitor} function instead.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-function{gdk-screen-get-n-monitors}
  @see-function{gdk-display-get-primary-monitor}"
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
  @argument[monitor-num]{the monitor number of type @code{:int}}
  @return{A @class{gdk-rectangle} structure filled with the monitor geometry.}
  @begin{short}
    Retrieves the @class{gdk-rectangle} structure representing the size and
    position of the individual monitor within the entire screen area.
  @end{short}

  Monitor numbers start at 0. To obtain the number of monitors of the screen,
  use the @fun{gdk-screen-get-n-monitors} function.

  Note that the size of the entire screen area can be retrieved via the
  @fun{gdk-screen-get-width} and @fun{gdk-screen-get-height} functions.
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-get-monitor-geometry} function has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-monitor-get-geometry} instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @argument[monitor-num]{the monitor number of type @code{:int}}
  @return{A @class{gdk-rectangle} structure filled with the monitor workarea.}
  @begin{short}
    Retrieves the @class{gdk-rectangle} structure representing the size and
    position of the \"work area\" on a monitor within the entire screen area.
  @end{short}

  The work area should be considered when positioning menus and similar
  popups, to avoid placing them below panels, docks or other desktop
  components.

  Monitor numbers start at 0. To obtain the number of monitors of the screen,
  use the @fun{gdk-screen-get-n-monitors} function.

  Since 3.4
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-monitor-workarea} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the
    @fun{gdk-monitor-get-workarea} function instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @argument[x]{the @code{:int} x coordinate in the virtual @arg{screen}}
  @argument[y]{the @code{:int} y coordinate in the virtual @arg{screen}}
  @begin{return}
    The monitor number in which the point (@arg{x}, @arg{y}) lies, or a monitor
    close to the point if not in any monitor.
  @end{return}
  @begin{short}
    Returns the monitor number in which the point (@arg{x}, @arg{y}) is located.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-screen-get-monitor-at-point} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-display-get-monitor-at-point} instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
    The monitor number of type @code{:int} in which most of @arg{window} is
    located, or if @arg{window} does not intersect any monitors, a monitor,
    close to @arg{window}.
  @end{return}
  @begin{short}
    Returns the number of the monitor in which the largest area of the bounding
    rectangle of the window resides.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-get-monitor-at-window} function has been deprecated
    since version 3.22 and should not be used in newly-written code. Use the
    @fun{gdk-display-get-monitor-at-window} function instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-get-monitor-height-mm} function has been deprecated
    since version 3.22 and should not be used in newly-written code. Use the
    functon @fun{gdk-monitor-get-height-mm} instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-get-monitor-width-mm} function has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-monitor-get-width-mm} instead.
  @end{dictionary}
  @see-class{gdk-screen}
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
  @begin[Warning]{dictionary}
    The @sym{gdk-screen-get-monitor-plug-name} function has been deprecated
    since version 3.22 and should not be used in newly-written code. Use the
    function @fun{gdk-monitor-get-model} instead.
  @end{dictionary}
  @see-class{gdk-screen}"
  (screen (g-object gdk-screen))
  (monitor-num :int))

(export 'gdk-screen-get-monitor-plug-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_scale_factor ()
;;;
;;; gint
;;; gdk_screen_get_monitor_scale_factor (GdkScreen *screen,
;;;                                      gint monitor_num);
;;;
;;; Returns the internal scale factor that maps from monitor coordiantes to the
;;; actual device pixels. On traditional systems this is 1, but on very high
;;; density outputs this can be a higher value (often 2).
;;;
;;; This can be used if you want to create pixel based data for a particula
;;; monitor, but most of the time you're drawing to a window where it is better
;;; to use gdk_window_get_scale_factor() instead.
;;;
;;; Warning
;;;
;;; The function gdk_screen_get_monitor_scale_factor has been deprecated since
;;; version 3.22 and should not be used in newly-written code. Use the function
;;; gdk_monitor_get_scale_factor() instead.
;;;
;;; Parameters
;;;
;;; screen
;;;     screen to get scale factor for
;;;
;;; monitor_num
;;;     number of the monitor, between 0 and gdk_screen_get_n_monitors (screen)
;;;
;;; Returns
;;;     the scale factor
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_setting ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_setting" %gdk-screen-get-setting) :boolean
  (screen (g-object gdk-screen))
  (name :string)
  (value (:pointer (:struct g-value))))

(defun gdk-screen-get-setting (screen name gtype)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-7}
  @argument[screen]{the @class{gdk-screen} object where the setting is located}
  @argument[name]{the name of type @code{:string} of the setting}
  @argument[gtype]{a @code{:string} with the GType of the setting}
  @begin{return}
    The value of the setting, or @code{nil} if the setting does not exist.
  @end{return}
  @begin{short}
    Retrieves a desktop wide setting such as double-click time for the screen.
  @end{short}

  See the @class{gtk-settings} class for the available settings.
  @begin[Example]{dictionary}
    @begin{pre}
 (let ((screen (gdk-display-get-default-screen (gdk-display-get-default)))) 
   (gdk-screen-get-setting screen \"gtk-double-click-time\" \"gint\"))
=> 400
    @end{pre}
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gtk-settings}"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value gtype)
    (when (%gdk-screen-get-setting screen name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-screen-get-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_active_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_get_active_window" gdk-screen-get-active-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
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
    The function @sym{gdk-screen-get-active-window} has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-screen}"
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
  @see-class{gdk-screen}"
  (screen (g-object gdk-screen)))

(export 'gdk-screen-get-window-stack)

;;; --- End of file gdk.screen.lisp --------------------------------------------
