;;; ----------------------------------------------------------------------------
;;; gtk.status-icon.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkStatusIcon
;;;
;;;     Display an icon in the system tray
;;;
;;; Types and Values
;;;
;;;     GtkStatusIcon
;;;
;;; Functions
;;;
;;;     gtk_status_icon_new
;;;     gtk_status_icon_new_from_pixbuf
;;;     gtk_status_icon_new_from_file
;;;     gtk_status_icon_new_from_stock
;;;     gtk_status_icon_new_from_icon_name
;;;     gtk_status_icon_new_from_gicon
;;;     gtk_status_icon_set_from_pixbuf
;;;     gtk_status_icon_set_from_file
;;;     gtk_status_icon_set_from_stock
;;;     gtk_status_icon_set_from_icon_name
;;;     gtk_status_icon_set_from_gicon
;;;     gtk_status_icon_get_storage_type
;;;     gtk_status_icon_get_pixbuf
;;;     gtk_status_icon_get_stock
;;;     gtk_status_icon_get_icon_name
;;;     gtk_status_icon_get_gicon
;;;     gtk_status_icon_get_size
;;;     gtk_status_icon_set_screen
;;;     gtk_status_icon_get_screen
;;;     gtk_status_icon_set_tooltip_text
;;;     gtk_status_icon_get_tooltip_text
;;;     gtk_status_icon_set_tooltip_markup
;;;     gtk_status_icon_get_tooltip_markup
;;;     gtk_status_icon_set_has_tooltip
;;;     gtk_status_icon_get_has_tooltip
;;;     gtk_status_icon_set_title
;;;     gtk_status_icon_get_title
;;;     gtk_status_icon_set_name
;;;     gtk_status_icon_set_visible
;;;     gtk_status_icon_get_visible
;;;     gtk_status_icon_is_embedded
;;;     gtk_status_icon_position_menu
;;;     gtk_status_icon_get_geometry
;;;     gtk_status_icon_get_x11_window_id
;;;
;;; Properties
;;;
;;;           gboolean    embedded              Read
;;;              gchar*   file                  Write
;;;              GIcon*   gicon                 Read / Write
;;;           gboolean    has-tooltip           Read / Write
;;;              gchar*   icon-name             Read / Write
;;;     GtkOrientation    orientation	        Read
;;;          GdkPixbuf*   pixbuf                Read / Write
;;;          GdkScreen*   screen                Read / Write
;;;               gint    size                  Read
;;;              gchar*   stock                 Read / Write
;;;       GtkImageType    storage-type          Read
;;;              gchar*   title                 Read / Write
;;;              gchar*   tooltip-markup        Read / Write
;;;              gchar*   tooltip-text          Read / Write
;;;           gboolean    visible               Read / Write
;;;
;;; Signals
;;;
;;;               void    activate              Action
;;;           gboolean    button-press-event    Run Last
;;;           gboolean    button-release-event  Run Last
;;;               void    popup-menu            Action
;;;           gboolean    query-tooltip         Run Last
;;;           gboolean    scroll-event          Run Last
;;;           gboolean    size-changed          Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkStatusIcon
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStatusIcon
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStatusIcon" gtk-status-icon
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_status_icon_get_type")
  ((embedded
    gtk-status-icon-embedded
    "embedded" "gboolean" t nil)
   (file
    gtk-status-icon-file
    "file" "gchararray" nil t)
   (gicon
    gtk-status-icon-gicon
    "gicon" "GIcon" t t)
   (has-tooltip
    gtk-status-icon-has-tooltip
    "has-tooltip" "gboolean" t t)
   (icon-name
    gtk-status-icon-icon-name
    "icon-name" "gchararray" t t)
   (orientation
    gtk-status-icon-orientation
    "orientation" "GtkOrientation" t nil)
   (pixbuf
    gtk-status-icon-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (screen
    gtk-status-icon-screen
    "screen" "GdkScreen" t t)
   (size
    gtk-status-icon-size
    "size" "gint" t nil)
   (stock
    gtk-status-icon-stock
    "stock" "gchararray" t t)
   (storage-type
    gtk-status-icon-storage-type
    "storage-type" "GtkImageType" t nil)
   (title
    gtk-status-icon-title
    "title" "gchararray" t t)
   (tooltip-markup
    gtk-status-icon-tooltip-markup
    "tooltip-markup" "gchararray" t t)
   (tooltip-text
    gtk-status-icon-tooltip-text
    "tooltip-text" "gchararray" t t)
   (visible
    gtk-status-icon-visible
    "visible" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-status-icon 'type)
 "@version{2021-7-22}
  @begin{short}
    The \"system tray\" or notification area is normally used for transient
    icons that indicate some special state.
  @end{short}
  For example, a system tray icon might appear to tell the user that they have
  new mail, or have an incoming instant message, or something along those lines.
  The basic idea is that creating an icon in the notification area is less
  annoying than popping up a dialog.

  A @sym{gtk-status-icon} object can be used to display an icon in a
  \"system tray\". The icon can have a tooltip, and the user can interact with
  it by activating it or popping up a context menu.

  It is very important to notice that status icons depend on the existence of a
  notification area being available to the user. You should not use status icons
  as the only way to convey critical information regarding your application, as
  the notification area may not exist on the environment of the user, or may
  have been removed. You should always check that a status icon has been
  embedded into a notification area by using the function
  @fun{gtk-status-icon-is-embedded}, and gracefully recover if the function
  returns @code{nil}.

  On X11, the implementation follows the freedesktop.org \"System Tray\"
  specification. Implementations of the \"tray\" side of this specification can
  be found e.g. in the GNOME 2 and KDE panel applications.

  Note that a status icon is not a widget, but just a @class{g-object} object.
  Making it a widget would be impractical, since the system tray on Win32 does
  not allow to embed arbitrary widgets.
  @begin[Warning]{dictionary}
    The @sym{gtk-status-icon} class has been deprecated in 3.14. You should
    consider using notifications or more modern platform-specific APIs instead.
    GLib provides the @code{GNotification} API which works well with the
    @class{gtk-application} class on multiple platforms and environments, and
    should be the preferred mechanism to notify the users of transient status
    updates.
    See @url[https://wiki.gnome.org/HowDoI/GNotification]{GNotification} for
    code examples.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (icon)    :action
      @end{pre}
      Gets emitted when the user activates the status icon. If and how status
      icons can be activated is platform dependent. Unlike most @code{:action}
      signals, this signal is meant to be used by applications and should be
      wrapped by language bindings.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
      @end{table}
    @subheading{The \"button-press-event\" signal}
      @begin{pre}
 lambda (icon event)    :run-last
      @end{pre}
      The signal will be emitted when a button, typically from a mouse, is
      pressed. Whether this event is emitted is platform dependent. Use the
      \"activate\" and \"popup-menu\" signals in preference.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
        @entry[event]{The @class{gdk-event-button} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"button-release-event\" signal}
      @begin{pre}
 lambda (icon event)    :run-last
      @end{pre}
      The signal will be emitted when a button, typically from a mouse, is
      released. Whether this event is emitted is platform dependent. Use the
      \"activate\" and \"popup-menu\" signals in preference.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
        @entry[event]{The @class{gdk-event-button} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"popup-menu\" signal}
      @begin{pre}
 lambda (icon button time)    :action
      @end{pre}
      Gets emitted when the user brings up the context menu of the status icon.
      Whether status icons can have context menus and how these are activated is
      platform dependent. The @arg{button} and @arg{time} parameters should be
      passed as the last arguments to the function @fun{gtk-menu-popup}. Unlike
      most @code{:action} signals, this signal is meant to be used by
      applications and should be wrapped by language bindings.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
        @entry[button]{An unsigned integer with the button that was pressed, or
          0 if the signal is not emitted in response to a button press event.}
        @entry[time]{An unsigned integer with the timestamp of the event that
          triggered the signal emission.}
      @end{table}
    @subheading{The \"query-tooltip\" signal}
      @begin{pre}
 lambda (icon x y mode tooltip)    :run-last
      @end{pre}
      Emitted when the @slot[gtk-settings]{gtk-tooltip-timeout} setting has
      expired with the cursor hovering above the status icon, or emitted when
      the status icon got focus in keyboard mode. Using the given coordinates,
      the signal handler should determine whether a tooltip should be shown for
      the status icon. If this is the case @em{true} should be returned,
      @em{false} otherwise. Note that if @arg{mode} is @em{true}, the values of
      @arg{x} and @arg{y} are undefined and should not be used. The signal
      handler is free to manipulate the tooltip with the therefore destined
      function calls. Whether this signal is emitted is platform dependent. For
      plain text tooltips, use the @code{tooltip-text} property in preference.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
        @entry[x]{An integer with the x coordinate of the cursor position where
          the request has been emitted, relative to @arg{icon}.}
        @entry[y]{An integer with the y coordinate of the cursor position where
          the request has been emitted, relative to @arg{icon}.}
        @entry[mode]{@em{True} if the tooltip was trigged using the keyboard.}
        @entry[tooltip]{A @class{gtk-tooltip} object.}
        @entry[Returns]{@em{True} if the tooltip should be shown right now,
          @em{false} otherwise.}
      @end{table}
    @subheading{The \"scroll-event\" signal}
      @begin{pre}
 lambda (icon event)    :run-last
      @end{pre}
      The signal is emitted when a button in the 4 to 7 range is pressed. Wheel
      mice are usually configured to generate button press events for buttons 4
      and 5 when the wheel is turned. Whether this event is emitted is platform
      dependent.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
        @entry[event]{The @class{gdk-event-scroll} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"size-changed\" signal}
      @begin{pre}
 lambda (icon size)    :run-last
      @end{pre}
      Gets emitted when the size available for the image changes, e.g. because
      the notification area got resized.
      @begin[code]{table}
        @entry[icon]{The @sym{gtk-status-icon} object which received the
          signal.}
        @entry[size]{An integer with the new size.}
        @entry[Returns]{@em{True} if the icon was updated for the new size.
          Otherwise, GTK will scale the icon as necessary.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-status-icon-embedded}
  @see-slot{gtk-status-icon-file}
  @see-slot{gtk-status-icon-gicon}
  @see-slot{gtk-status-icon-has-tooltip}
  @see-slot{gtk-status-icon-icon-name}
  @see-slot{gtk-status-icon-orientation}
  @see-slot{gtk-status-icon-pixbuf}
  @see-slot{gtk-status-icon-screen}
  @see-slot{gtk-status-icon-size}
  @see-slot{gtk-status-icon-stock}
  @see-slot{gtk-status-icon-storage-type}
  @see-slot{gtk-status-icon-title}
  @see-slot{gtk-status-icon-tooltip-markup}
  @see-slot{gtk-status-icon-tooltip-text}
  @see-slot{gtk-status-icon-visible}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-status-icon-embedded -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embedded" 'gtk-status-icon) 't)
 "The @code{embedded} property of type @code{:boolean} (Read) @br{}
  @em{True} if the status icon is embedded in a notification area. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-embedded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-embedded 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-embedded object) => embedded}
  @argument[object]{a @class{gtk-status-icon} object}
  @argument[embedded]{a boolean whether the status icon is embedded}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{embedded} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  @em{True} if the status icon is embedded in a notification area.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-embedded} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-is-embedded}")

;;; --- gtk-status-icon-file ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "file" 'gtk-status-icon) 't)
 "The @code{file} property of type @code{:string} (Write) @br{}
  Filename to load and display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-file atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-file 'function)
 "@version{2021-7-22}
  @syntax[]{(setf (gtk-status-icon-file object) file)}
  @argument[object]{a @class{gtk-status-icon} object}
  @argument[file]{a string with a filename}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{file} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  Filename to load and display.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-file} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}")

;;; --- gtk-status-icon-gicon --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-status-icon) 't)
 "The @code{gicon} property of type @class{g-icon} (Read / Write) @br{}
  The icon displayed in the status icon. For themed icons, the image will be
  updated automatically if the theme changes.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-gicon 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-gicon object) => icon}
  @syntax[]{(setf (gtk-status-icon-gicon object) icon)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{gicon} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-gicon} retrieves icon being
  displayed by the status icon. The slot access function
  @sym{(setf gtk-status-icon-gicon)} sets the icon.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:gicon} of the @symbol{gtk-image-type} enumeration. See the function
  @fun{gtk-status-icon-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-gicon} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-class{g-icon}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-storage-type}")

;;; --- gtk-status-icon-has-tooltip --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-tooltip"
                                               'gtk-status-icon) 't)
 "The @code{has-tooltip} property of type @code{:boolean} (Read / Write) @br{}
  Enables or disables the emission of \"query-tooltip\" signals on the status
  icon. A value of @em{true} indicates that the status icon can have a tooltip,
  in this case the status icon will be queried using the \"query-tooltip\"
  signal to determine whether it will provide a tooltip or not. Note that
  setting this property to @em{true} for the first time will change the event
  masks of the windows of this status icon to include \"leave-notify\" and
  \"motion-notify\" signals. This will not be undone when the property is set
  to @em{false} again. Whether this property is respected is platform dependent.
  For plain text tooltips, use the @code{tooltip-text} property in preference.
  @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-has-tooltip atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-has-tooltip 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-has-tooltip object) => has-tooltip}
  @syntax[]{(setf (gtk-status-icon-has-tooltip object) has-tooltip)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[has-tooltip]{a boolean whether or not the status icon has a tooltip}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{has-tooltip} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-has-tooltip} returns the
  current value of the @slot[gtk-status-icon]{has-tooltip} property. The slot
  access function @sym{(setf gtk-status-icon-has-tooltip)} sets the property.

  Enables or disables the emission of \"query-tooltip\" signals on the status
  icon. A value of @em{true} indicates that the status icon can have a tooltip,
  in this case the status icon will be queried using the \"query-tooltip\"
  signal to determine whether it will provide a tooltip or not. Note that
  setting this property to @em{true} for the first time will change the event
  masks of the windows of this status icon to include \"leave-notify\" and
  \"motion-notify\" signals. This will not be undone when the property is set
  to @em{false} again. Whether this property is respected is platform dependent.
  For plain text tooltips, use the @slot[gtk-status-icon]{tooltip-text}
  property in preference.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-has-tooltip} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-tooltip-text}")

;;; --- gtk-status-icon-icon-name ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-status-icon) 't)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon from the icon theme. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-icon-name 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-icon-name object) => name}
  @syntax[]{(setf (gtk-status-icon-icon-name object) name)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[name]{a string with the name of the icon from the icon theme}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{icon-name} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-icon-name} gets the name of the
  icon being displayed by the status icon. The slot access function
  @sym{(setf gtk-status-icon-icon-name)} sets the name.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:icon-name} of the @symbol{gtk-image-type} enumeration. See the
  function @fun{gtk-status-icon-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-icon-name} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-storage-type}")

;;; --- gtk-status-icon-orientation --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "orientation"
                                               'gtk-status-icon) 't)
 "The @code{orientation} property of type @symbol{gtk-orientation} (Read) @br{}
  The orientation of the tray in which the status icon is embedded. @br{}
  Default value: @code{:horizontal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-orientation 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-orientation object) => orientation}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{orientation} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The orientation of the tray in which the status icon is embedded.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-orientation} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-symbol{gtk-orientation}")

;;; --- gtk-status-icon-pixbuf -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf" 'gtk-status-icon) 't)
 "The @code{pixbuf} property of type @class{gdk-pixbuf} (Read / Write) @br{}
  The pixbuf to display.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-pixbuf 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-pixbuf object) => pixbuf}
  @syntax[]{(setf (gtk-status-icon-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{pixbuf} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-pixbuf} gets the pixbuf being
  displayed by the status icon. The slot access function
  @sym{(setf gtk-status-icon-pixbuf)} sets the pixbuf.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:pixbuf} of the @symbol{gtk-image-type} enumeration. See the function
  @fun{gtk-status-icon-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-pixbuf} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-storage-type}")

;;; --- gtk-status-icon-screen -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-status-icon) 't)
 "The @code{screen} property of type @class{gdk-screen} (Read / Write) @br{}
  The screen where this status icon will be displayed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-screen 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-screen object) => screen}
  @syntax[]{(setf (gtk-status-icon-screen object) screen)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{screen} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-screen} returns the screen
  associated with the status icon. The slot access function
  @sym{(setf gtk-status-icon-screen)} sets the screen. If the icon is already
  mapped, it will be unmapped, and then remapped on the new screen.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-screen} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-class{gdk-screen}")

;;; --- gtk-status-icon-size ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-status-icon) 't)
 "The @code{size} property of type @code{:int} (Read) @br{}
  The size of the icon. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-size 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-size object) => size}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[size]{an integer with the size of the icon}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{size} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-size} gets the size in pixels
  that is available for the image.

  Stock icons and named icons adapt their size automatically if the size of the
  notification area changes. For other storage types, the \"size-changed\"
  signal can be used to react to size changes. Note that the returned size is
  only meaningful while the status icon is embedded. See the function
  @fun{gtk-status-icon-is-embedded}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-size} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-is-embedded}")

;;; --- gtk-status-icon-stock --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock" 'gtk-status-icon) 't)
 "The @code{stock} property of type @code{:string} (Read / Write) @br{}
  Stock ID for a stock image to display. @br{}
  @em{Warning:} The @code{stock} property has been deprecated since version
  3.10 and should not be used in newly written code. Use the
  @code{icon-name} property instead. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-stock 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-stock object) => stock-id}
  @syntax[]{(setf (gtk-status-icon-stock object) stock-id)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{stock} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-stock} gets the ID of the stock
  icon being displayed by the status icon. The slot access function
  @sym{(setf gtk-status-icon-stock)} sets the stock ID.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:stock} of the @symbol{gtk-image-type} enumeration. See the function
  @fun{gtk-status-icon-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-stock} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-storage-type}")

;;; --- gtk-status-icon-storage-type -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "storage-type"
                                               'gtk-status-icon) 't)
 "The @code{storage-type} property of type @symbol{gtk-image-type} (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-storage-type 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-storage-type object) => storage-type}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[storage-type]{a value of the @symbol{gtk-image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{storage-type} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-storage-type} gets the type of
  representation being used by the status icon to store image data. If the
  status icon has no image data, the return value will be the value
  @code{:empty}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-storage-type} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-symbol{gtk-image-type}")

;;; --- gtk-status-icon-title --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-status-icon) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of this tray icon. This should be a short, human readable,
  localized string describing the tray icon. It may be used by tools like
  screen readers to render the tray icon. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-title 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-title object) => title}
  @syntax[]{(setf (gtk-status-icon-title object) title)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[title]{a string with the title}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{title} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-title} gets the title of the
  status icon. The slot access function @sym{(setf gtk-status-icon-title)}
  sets the title.

  This should be a short, human readable, localized string describing the status
  icon. It may be used by tools like screen readers to render the status icon.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-title} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}")

;;; --- gtk-status-icon-tooltip-markup -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-markup"
                                               'gtk-status-icon) 't)
 "The @code{tooltip-markup} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string, which is marked up with
  the Pango text markup language. Also see the function
  @fun{gtk-tooltip-set-markup}. This is a convenience property which will take
  care of getting the tooltip shown if the given string is not @code{nil}. The
  @code{has-tooltip} property will automatically be set to @em{true} and the
  default handler for the \"query-tooltip\" signal will take care of displaying
  the tooltip. On some platforms, embedded markup will be ignored. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-tooltip-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-tooltip-markup 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-tooltip-markup object) => markup}
  @syntax[]{(setf (gtk-status-icon-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[markup]{a string with the contents of the tooltip for the status
    icon, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{tooltip-markup} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-tooltip-markup} gets the
  contents of the tooltip for the status icon. The slot access function
  @sym{(setf gtk-status-icon-tooltip-markup)} sets @arg{markup} as the contents
  of the tooltip, which is marked up with the Pango text markup language.

  This function will take care of setting the
  @slot[gtk-status-icon]{has-tooltip} property to @em{true} and of the default
  handler for the \"query-tooltip\" signal. See also the function
  @fun{gtk-tooltip-markup}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-tooltip-markup} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-tooltip-markup}
  @see-function{gtk-status-icon-has-tooltip}")

;;; --- gtk-status-icon-tooltip-text -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-text"
                                               'gtk-status-icon) 't)
 "The @code{tooltip-text} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string. Also see the function
  @fun{gtk-tooltip-set-text}. This is a convenience property which will take
  care of getting the tooltip shown if the given string is not @code{nil}. The
  @code{has-tooltip} property will automatically be set to @em{true} and the
  default handler for the \"query-tooltip\" signal will take care of displaying
  the tooltip. Note that some platforms have limitations on the length of
  tooltips that they allow on status icons, e.g. Windows only shows the first
  64 characters. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-tooltip-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-tooltip-text 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-tooltip-text object) => text}
  @syntax[]{(setf (gtk-status-icon-tooltip-text object) text)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[text]{a string with the contents of the tooltip for the status icon}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{tooltip-text} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-tooltip-text} gets the contents
  of the tooltip for the status icon. The slot access function
  @sym{(setf gtk-status-icon-tooltip-text)} sets the text of the tooltip.

  This function will take care of setting the
  @slot[gtk-status-icon]{has-tooltip} property to @em{true} and of the default
  handler for the \"query-tooltip\" signal.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-tooltip-text} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-has-tooltip}")

;;; --- gtk-status-icon-visible ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-status-icon) 't)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the status icon is visible. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-visible 'function)
 "@version{2021-7-22}
  @syntax[]{(gtk-status-icon-visible object) => visible}
  @syntax[]{(setf (gtk-status-icon-visible object) visible)}
  @argument[object]{a @class{gtk-status-icon} widget}
  @argument[visible]{@em{true} to show the status icon, @em{false} to hide it}
  @begin{short}
    Accessor of the @slot[gtk-status-icon]{visible} slot of the
    @class{gtk-status-icon} class.
  @end{short}

  The slot access function @sym{gtk-status-icon-visible} returns whether the
  status icon is visible or not. The slot access function
  @sym{(setf gtk-status-icon-visible)} sets the visibility.

  Note that being visible does not guarantee that the user can actually see the
  icon, see also the function @fun{gtk-status-icon-is-embedded}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-visible} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-is-embedded}")

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-new))

(defun gtk-status-icon-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @return{A new @class{gtk-status-icon} object.}
  @short{Creates an empty status icon.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-new} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}"
  (make-instance 'gtk-status-icon))

(export 'gtk-status-icon-new)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_pixbuf" gtk-status-icon-new-from-pixbuf)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @return{A new @class{gtk-status-icon} object.}
  @begin{short}
    Creates a status icon displaying @arg{pixbuf}.
  @end{short}

  The image will be scaled down to fit in the available space in the
  notification area, if necessary.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-new-from-pixbuf} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-class{gdk-pixbuf}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-status-icon-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_file" gtk-status-icon-new-from-file)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[filename]{a string with the filename}
  @return{A new @class{gtk-status-icon} object.}
  @begin{short}
    Creates a status icon displaying the icon from a file.
  @end{short}

  The image will be scaled down to fit in the available space in the
  notification area, if necessary.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-new-from-file} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}"
  (filename :string))

(export 'gtk-status-icon-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_stock" gtk-status-icon-new-from-stock)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[stock-id]{a string with a stock icon ID}
  @return{A new @class{gtk-status-icon} object.}
  @begin{short}
    Creates a status icon displaying a stock icon.
  @end{short}
  Sample stock icon names are \"gtk-open\", \"gtk-quit\".
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-new-from-stock} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}"
  (stock-id :string))

(export 'gtk-status-icon-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_icon_name"
           gtk-status-icon-new-from-icon-name) (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[name]{a string with an icon name}
  @return{A new @class{gtk-status-icon} object.}
  @begin{short}
    Creates a status icon displaying an icon from the current icon theme.
  @end{short}
  If the current icon theme is changed, the icon will be updated appropriately.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-new-from-icon-name} has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}"
  (name :string))

(export 'gtk-status-icon-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_gicon" gtk-status-icon-new-from-gicon)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{g-icon} object}
  @return{A new @class{gtk-status-icon} object.}
  @begin{short}
    Creates a status icon displaying a @class{g-icon} object.
  @end{short}
  If the icon is a themed icon, it will be updated when the theme changes.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-new-from-gicon} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-class{g-icon}"
  (icon (g-object g-icon)))

(export 'gtk-status-icon-new-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_pixbuf" gtk-status-icon-set-from-pixbuf)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @argument[pixbuf]{a @class{gdk-pixbuf} object or @code{nil}}
  @begin{short}
    Makes the status icon display @arg{pixbuf}.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-pixbuf} for details.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-set-from-pixbuf} has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-class{gdk-pixbuf}
  @see-function{gtk-status-icon-new-from-pixbuf}"
  (icon (g-object gtk-status-icon))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-status-icon-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_file" gtk-status-icon-set-from-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @argument[filename]{a string with a filename}
  @begin{short}
    Makes  the status icon display the icon from the file @arg{filename}.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-file} for details.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-set-from-file} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-file}"
  (icon (g-object gtk-status-icon))
  (filename :string))

(export 'gtk-status-icon-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_stock" gtk-status-icon-set-from-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Makes the status icon display the stock icon with the ID @arg{stock-id.}
  @end{short}
  See the function @fun{gtk-status-icon-new-from-stock} for details.
  @begin[Warning]{dictionary}
    The @sym{gtk-status-icon-set-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-stock}"
  (icon (g-object gtk-status-icon))
  (stock-id :string))

(export 'gtk-status-icon-set-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_icon_name"
           gtk-status-icon-set-from-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @argument[name]{a string with an icon name}
  @begin{short}
    Makes the status icon display the icon named @arg{name} from the current
    icon theme.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-icon-name} for details.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-set-from-icon-name} has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-icon-name}"
  (icon (g-object gtk-status-icon))
  (name :string))

(export 'gtk-status-icon-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_gicon" gtk-status-icon-set-from-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @argument[gicon]{a @class{g-icon} object}
  @begin{short}
    Makes the status icon display the @class{g-icon} object.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-gicon} for details.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-set-from-gicon} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-gicon}"
  (icon (g-object gtk-status-icon))
  (gicon (g-object g-icon)))

(export 'gtk-status-icon-set-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_name" gtk-status-icon-set-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @argument[name]{a string with the name}
  @begin{short}
    Sets the name of the tray icon.
  @end{short}
  This should be a string identifying this icon. It may be used for sorting the
  icons in the tray and will not be shown to the user.
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-set-name} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}"
  (icon (g-object gtk-status-icon))
  (name :string))

(export 'gtk-status-icon-set-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_is_embedded ()
;;; ----------------------------------------------------------------------------

;; TODO: Replace this code with a call to the accessor gtk-status-icon-embedded

(defcfun ("gtk_status_icon_is_embedded" gtk-status-icon-is-embedded) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-22}
  @argument[icon]{a @class{gtk-status-icon} object}
  @return{@em{True} if the status icon is embedded in a notification area.}
  @begin{short}
    Returns whether the status icon is embedded in a notification area.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-status-icon-is-embedded} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-status-icon}"
  (icon (g-object gtk-status-icon)))

(export 'gtk-status-icon-is-embedded)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_position_menu ()
;;;
;;; void gtk_status_icon_position_menu (GtkMenu *menu,
;;;                                     gint *x,
;;;                                     gint *y,
;;;                                     gboolean *push_in,
;;;                                     gpointer user_data);
;;;
;;; gtk_status_icon_position_menu has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;; Use GNotification and GtkApplication to provide status notifications;
;;; notifications do not have menus, but can have buttons, and actions
;;; associated with each button.
;;;
;;; Menu positioning function to use with gtk_menu_popup() to position menu
;;; aligned to the status icon user_data.
;;;
;;; menu :
;;;     the GtkMenu
;;;
;;; x :
;;;     return location for the x position
;;;
;;; y :
;;;     return location for the y position
;;;
;;; push_in :
;;;     whether the first menu item should be offset (pushed in) to be aligned
;;;     with the menu popup position (only useful for GtkOptionMenu)
;;;
;;; user_data :
;;;     the status icon to position the menu on
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_geometry ()
;;;
;;; gboolean gtk_status_icon_get_geometry (GtkStatusIcon *status_icon,
;;;                                        GdkScreen **screen,
;;;                                        GdkRectangle *area,
;;;                                        GtkOrientation *orientation);
;;;
;;; gtk_status_icon_get_geometry has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;; Use GNotification and GtkApplication to provide status notifications; there
;;; is no direct replacement for this function, as the platform is responsible
;;; for the presentation of notifications
;;;
;;; Obtains information about the location of the status icon on screen. This
;;; information can be used to e.g. position popups like notification bubbles.
;;;
;;; See gtk_status_icon_position_menu() for a more convenient alternative for
;;; positioning menus.
;;;
;;; Note that some platforms do not allow GTK to provide this information, and
;;; even on platforms that do allow it, the information is not reliable unless
;;; the status icon is embedded in a notification area, see
;;; gtk_status_icon_is_embedded().
;;;
;;; status_icon :
;;;     a GtkStatusIcon
;;;
;;; screen :
;;;     return location for the screen, or NULL if the information is not
;;;     needed
;;;
;;; area :
;;;     return location for the area occupied by the status icon, or NULL
;;;
;;; orientation :
;;;     return location for the orientation of the panel in which the status
;;;     icon is embedded, or NULL. A panel at the top or bottom of the screen is
;;;     horizontal, a panel at the left or right is vertical
;;;
;;; Returns :
;;;     TRUE if the location information has been filled in
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_x11_window_id ()
;;;
;;; guint32 gtk_status_icon_get_x11_window_id (GtkStatusIcon *status_icon);
;;;
;;; gtk_status_icon_get_x11_window_id has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;; Use GNotification and GtkApplication to provide status notifications; there
;;; is no direct replacement for this function
;;;
;;; This function is only useful on the X11/freedesktop.org platform. It returns
;;; a window ID for the widget in the underlying status icon implementation.
;;; This is useful for the Galago notification service, which can send a window
;;; ID in the protocol in order for the server to position notification windows
;;; pointing to a status icon reliably.
;;;
;;; This function is not intended for other use cases which are more likely to
;;; be met by one of the non-X11 specific methods, such as
;;; gtk_status_icon_position_menu().
;;;
;;; status_icon :
;;;     a GtkStatusIcon
;;;
;;; Returns :
;;;     An 32 bit unsigned integer identifier for the underlying X11 Window
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.status-icon.lisp ---------------------------------------
