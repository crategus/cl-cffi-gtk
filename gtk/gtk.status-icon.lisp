;;; ----------------------------------------------------------------------------
;;; gtk.status-icon.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkStatusIcon
;;;
;;; Display an icon in the system tray
;;;
;;; Synopsis
;;;
;;;     GtkStatusIcon
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
    "title" "gchar" t t)
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
 "@version{2013-4-24}
  @begin{short}
    The \"system tray\" or notification area is normally used for transient
    icons that indicate some special state. For example, a system tray icon
    might appear to tell the user that they have new mail, or have an incoming
    instant message, or something along those lines. The basic idea is that
    creating an icon in the notification area is less annoying than popping up a
    dialog.
  @end{short}

  A @sym{gtk-status-icon} object can be used to display an icon in a
  \"system tray\". The icon can have a tooltip, and the user can interact with
  it by activating it or popping up a context menu. Critical information should
  not solely be displayed in a @sym{gtk-status-icon}, since it may not be
  visible, e. g. when the user does not have a notification area on his panel.
  This can be checked with the function @fun{gtk-status-icon-is-embedded}.

  On X11, the implementation follows the freedesktop.org \"System Tray\"
  specification. Implementations of the \"tray\" side of this specification can
  be found e. g. in the GNOME 2 and KDE panel applications.

  Note that a @sym{gtk-status-icon} is not a widget, but just a
  @class{g-object}. Making it a widget would be impractical, since the system
  tray on Win32 does not allow to embed arbitrary widgets.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (status-icon)   : Action
      @end{pre}
      Gets emitted when the user activates the status icon. If and how status
      icons can activated is platform-dependent.
      Unlike most @code{G_SIGNAL_ACTION} signals, this signal is meant to be
      used by applications and should be wrapped by language bindings.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
      @end{table}
      Since 2.10

    @subheading{The \"button-press-event\" signal}
      @begin{pre}
 lambda (status-icon event)
      @end{pre}
      The \"button-press-event\" signal will be emitted when a button (typically
      from a mouse) is pressed.
      Whether this event is emitted is platform-dependent. Use the \"activate\"
      and \"popup-menu\" signals in preference.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
        @entry[event]{The @class{gdk-event-button} which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @code{Nil} to propagate the event further.}
      @end{table}
      Since 2.14

    @subheading{The \"button-release-event\" signal}
      @begin{pre}
 lambda (status-icon event)
      @end{pre}
      The \"button-release-event\" signal will be emitted when a button
      (typically from a mouse) is released.
      Whether this event is emitted is platform-dependent. Use the \"activate\"
      and \"popup-menu\" signals in preference.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
        @entry[event]{The @class{gdk-event-button} which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @code{Nil} to propagate the event further.}
      @end{table}
      Since 2.14

    @subheading{The \"popup-menu\" signal}
      @begin{pre}
 lambda (status-icon button activate-time)   : Action
      @end{pre}
      Gets emitted when the user brings up the context menu of the status icon.
      Whether status icons can have context menus and how these are activated is
      platform-dependent.
      The button and @arg{activate-time} parameters should be passed as the last
      to arguments to the function @fun{gtk-menu-popup}.
      Unlike most @code{G_SIGNAL_ACTION} signals, this signal is meant to be
      used by applications and should be wrapped by language bindings.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
        @entry[button]{The button that was pressed, or 0 if the signal is not
          emitted in response to a button press event.}
        @entry[activate-time]{The timestamp of the event that triggered the
          signal emission.}
      @end{table}
      Since 2.10

    @subheading{The \"query-tooltip\" signal}
      @begin{pre}
 lambda (status-icon x y keyboard-mode tooltip)   : Run Last
      @end{pre}
      Emitted when the \"gtk-tooltip-timeout\" has expired with the cursor
      hovering above @arg{status-icon}; or emitted when @arg{status-icon} got
      focus in keyboard mode.
      Using the given coordinates, the signal handler should determine whether a
      tooltip should be shown for @arg{status-icon}. If this is the case
      @em{true} should be returned, @code{nil} otherwise. Note that if
      @arg{keyboard-mode} is @em{true}, the values of @arg{x} and @arg{y} are
      undefined and should not be used.
      The signal handler is free to manipulate tooltip with the therefore
      destined function calls.
      Whether this signal is emitted is platform-dependent. For plain text
      tooltips, use \"tooltip-text\" in preference.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
        @entry[x]{The x coordinate of the cursor position where the request has
          been emitted, relative to @arg{status-icon}.}
        @entry[y]{The y coordinate of the cursor position where the request has
          been emitted, relative to @arg{status-icon}.}
        @entry[keyboard-mode]{@em{True} if the tooltip was trigged using the
          keyboard.}
        @entry[tooltip]{A @class{gtk-tooltip} object.}
        @entry[Returns]{@em{True} if tooltip should be shown right now,
          @code{nil} otherwise.}
      @end{table}
      Since 2.16

    @subheading{The \"scroll-event\" signal}
      @begin{pre}
 lambda (status-icon event)   : Run Last
      @end{pre}
      The \"scroll-event\" signal is emitted when a button in the 4 to 7 range
      is pressed. Wheel mice are usually configured to generate button press
      events for buttons 4 and 5 when the wheel is turned.
      Whether this event is emitted is platform-dependent.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
        @entry[event]{The @class{gdk-event-scroll} which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @code{Nil} to propagate the event further.}
      @end{table}
      Since 2.16

    @subheading{The \"size-changed\" signal}
      @begin{pre}
 lambda (status-icon size)   : Run Last
      @end{pre}
      Gets emitted when the size available for the image changes, e. g. because
      the notification area got resized.
      @begin[code]{table}
        @entry[status-icon]{The object which received the signal.}
        @entry[size]{The new size.}
        @entry[Returns]{@em{True} if the icon was updated for the new size.
          Otherwise, GTK+ will scale the icon as necessary.}
      @end{table}
      Since 2.10
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
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embedded" 'gtk-status-icon) 't)
 "The @code{\"embedded\"} property of type @code{:boolean} (Read) @br{}
  @em{True} if the status icon is embedded in a notification area. @br{}
  Default value: @code{nil} @br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "file" 'gtk-status-icon) 't)
 "The @code{\"file\"} property of type @code{:string} (Write) @br{}
  Filename to load and display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-status-icon) 't)
 "The @code{\"gicon\"} property of type @code{GIcon} (Read / Write) @br{}
  The @class{g-icon} displayed in the @sym{gtk-status-icon}. For themed icons,
  the image will be updated automatically if the theme changes. @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-tooltip"
                                               'gtk-status-icon) 't)
 "The @code{\"has-tooltip\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Enables or disables the emission of \"query-tooltip\" signals on status icon.
  A value of @em{true} indicates that status icon can have a tooltip, in this
  case the status icon will be queried using the \"query-tooltip\" signal to
  determine whether it will provide a tooltip or not.
  Note that setting this property to @em{true} for the first time will change
  the event masks of the windows of this status icon to include \"leave-notify\"
  and \"motion-notify\" events. This will not be undone when the property is set
  to @code{nil} again.
  Whether this property is respected is platform dependent. For plain text
  tooltips, use the @code{\"tooltip-text\"} property in preference. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-status-icon) 't)
 "The @code{\"icon-name\"} property of type @code{:string} (Read / Write) @br{}
  The name of the icon from the icon theme. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "orientation"
                                               'gtk-status-icon) 't)
 "The @code{\"orientation\"} property of type @symbol{gtk-orientation}
  (Read) @br{}
  The orientation of the tray in which the status icon is embedded. @br{}
  Default value: @code{:horizontal} @br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf" 'gtk-status-icon) 't)
 "The @code{\"pixbuf\"} property of type @class{gdk-pixbuf} (Read / Write) @br{}
  A @class{gdk-pixbuf} object to display.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-status-icon) 't)
 "The @code{\"screen\"} property of type @class{gdk-screen} (Read / Write) @br{}
  The screen where this status icon will be displayed.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-status-icon) 't)
 "The @code{\"size\"} property of type @code{:int} (Read) @br{}
  The size of the icon. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock" 'gtk-status-icon) 't)
 "The @code{\"stock\"} property of type @code{:string} (Read / Write) @br{}
  Stock ID for a stock image to display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "storage-type"
                                               'gtk-status-icon) 't)
 "The @code{\"storage-type\"} property of type @symbol{gtk-image-type}
  (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-status-icon) 't)
 "The @code{\"title\"} property of type @code{:string} @code{Read / Write} @br{}
  The title of this tray icon. This should be a short, human-readable,
  localized string describing the tray icon. It may be used by tools like
  screen readers to render the tray icon. @br{}
  Default value: @code{nil} @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-markup"
                                               'gtk-status-icon) 't)
 "The @code{\"tooltip-markup\"} property of type @code{:string}
  (Read / Write) @br{}
  Sets the text of tooltip to be the given string, which is marked up with the
  Pango text markup language. Also see the function
  @fun{gtk-tooltip-set-markup}.
  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not @code{nil}. The @code{\"has-tooltip\"}
  property will automatically be set to @em{true} and the default handler for
  the \"query-tooltip\" signal will take care of displaying the tooltip.
  On some platforms, embedded markup will be ignored. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-text"
                                               'gtk-status-icon) 't)
 "The @code{\"tooltip-text\"} property of type @code{:string}
  (Read / Write) @br{}
  Sets the text of tooltip to be the given string. Also see the function
  @fun{gtk-tooltip-set-text}.
  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not @code{nil}. The @code{\"has-tooltip\"}
  property will automatically be set to @em{true} and the default handler for
  the \"query-tooltip\" signal will take care of displaying the tooltip.
  Note that some platforms have limitations on the length of tooltips that
  they allow on status icons, e. g. Windows only shows the first 64
  characters. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-status-icon) 't)
 "The @code{\"visible\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether the status icon is visible. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-embedded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-embedded 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"embedded\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-is-embedded}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-file atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-file 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"file\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-gicon 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"gicon\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-gicon}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-has-tooltip atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-has-tooltip 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"has-tooltip\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-has-tooltip}
  @see-function{gtk-status-icon-set-has-tooltip}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-icon-name 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"icon-name\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-icon-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-orientation 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"orientation\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-pixbuf 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"pixbuf\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-pixbuf}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-screen 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"screen\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-screen}
  @see-function{gtk-status-icon-set-sceen}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-size 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"size\"} of the @class{gtk-status-icon} class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-size}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-stock 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"stock\"} of the @class{gtk-status-icon} class.
  @see-class{gtk-status-icon}
  @see-class{gtk-status-icon-get-stock}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-storage-type 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"storage-type\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-storage-type}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-title 'function)
 "@version{2013-11-10}
  Accessor of the slot @code{\"title\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-title}
  @see-function{gtk-status-icon-set-title}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-tooltip-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-tooltip-markup 'function)
 "@version{2013-11-10}
  Accessor of the slot @code{\"tooltip-markup\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-tooltip-markup}
  @see-function{gtk-status-icon-set-tooltip-markup}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-tooltip-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-tooltip-text 'function)
 "@version{2013-11-10}
  Accessor of the slot @code{\"tooltip-text\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-tooltip-text}
  @see-function{gtk-status-icon-set-tooltip-text}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-status-icon-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-status-icon-visible 'function)
 "@version{2013-11-10}
  Accessor of the slot @code{\"visible\"} of the @class{gtk-status-icon}
  class.
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-visible}
  @see-function{gtk-status-icon-set-visible}")

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-new))

(defun gtk-status-icon-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @return{A new @class{gtk-status-icon} widget.}
  @short{Creates an empty status icon object.}

  Since 2.10
  @see-class{gtk-status-icon}"
  (make-instance 'gtk-status-icon))

(export 'gtk-status-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_pixbuf" gtk-status-icon-new-from-pixbuf)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @return{A new @class{gtk-status-icon} widget.}
  @begin{short}
    Creates a status icon displaying @arg{pixbuf}.
  @end{short}

  The image will be scaled down to fit in the available space in the
  notification area, if necessary.

  Since 2.10
  @see-class{gtk-status-icon}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-status-icon-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_file" gtk-status-icon-new-from-file)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[filename]{a filename}
  @return{A new @class{gtk-status-icon} widget.}
  @begin{short}
    Creates a status icon displaying the file @arg{filename}.
  @end{short}

  The image will be scaled down to fit in the available space in the
  notification area, if necessary.

  Since 2.10
  @see-class{gtk-status-icon}"
  (filename :string))

(export 'gtk-status-icon-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_stock" gtk-status-icon-new-from-stock)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[stock-id]{a stock icon ID}
  @return{A new @class{gtk-status-icon} widget.}
  @begin{short}
    Creates a status icon displaying a stock icon.
  @end{short}
  Sample stock icon names are \"gtk-open\", \"gtk-quit\". You can register your
  own stock icon names, see the functions @fun{gtk-icon-factory-add-default}
  and @fun{gtk-icon-factory-add}.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-icon-factory-add-default}
  @see-function{gtk-icon-factory-add}"
  (stock-id :string))

(export 'gtk-status-icon-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_icon_name"
           gtk-status-icon-new-from-icon-name) (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[icon-name]{an icon name}
  @return{A new @class{gtk-status-icon} widget}
  @begin{short}
    Creates a status icon displaying an icon from the current icon theme.
  @end{short}
  If the current icon theme is changed, the icon will be updated appropriately.

  Since 2.10
  @see-class{gtk-status-icon}"
  (icon-name :string))

(export 'gtk-status-icon-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_new_from_gicon" gtk-status-icon-new-from-gicon)
    (g-object gtk-status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[icon]{a @class{g-icon} object}
  @return{A new @class{gtk-status-icon} widget.}
  @begin{short}
    Creates a status icon displaying a @class{g-icon} object.
  @end{short}
  If the icon is a themed icon, it will be updated when the theme changes.

  Since 2.14
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
 "@version{2013-11-24}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf} or @code{nil}}
  @begin{short}
    Makes @arg{status-icon} display @arg{pixbuf}.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-pixbuf} for details.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-class{gdk-pixbuf}
  @see-function{gtk-status-icon-new-from-pixbuf}"
  (status-icon (g-object gtk-status-icon))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-status-icon-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_file" gtk-status-icon-set-from-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[filename]{a filename}
  @begin{short}
    Makes @arg{status-icon} display the file filename.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-file} for details.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-file}"
  (status-icon (g-object gtk-status-icon))
  (filename :string))

(export 'gtk-status-icon-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_stock" gtk-status-icon-set-from-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[stock-id]{a stock icon ID}
  @begin{short}
    Makes @arg{status-icon} display the stock icon with the ID @arg{stock-id.}
  @end{short}
  See the function @fun{gtk-status-icon-new-from-stock} for details.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-stock}"
  (status-icon (g-object gtk-status-icon))
  (stock-id :string))

(export 'gtk-status-icon-set-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_icon_name"
           gtk-status-icon-set-from-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[icon-name]{an icon name}
  @begin{short}
    Makes @arg{status-icon} display the icon named @arg{icon-name} from the
    current icon theme.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-icon-name} for details.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-icon-name}"
  (status-icon (g-object gtk-status-icon))
  (icon-name :string))

(export 'gtk-status-icon-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_from_gicon" gtk-status-icon-set-from-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Makes @arg{status-icon} display the @class{g-icon}.
  @end{short}
  See the function @fun{gtk-status-icon-new-from-gicon} for details.

  Since 2.14
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-new-from-gicon}"
  (status-icon (g-object gtk-status-icon))
  (icon (g-object g-icon)))

(export 'gtk-status-icon-set-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_storage_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-storage-type))

(defun gtk-status-icon-get-storage-type (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{The image representation being used.}
  @begin{short}
    Gets the type of representation being used by the @class{gtk-status-icon}
    to store image data.
  @end{short}
  If the @class{gtk-status-icon} has no image data, the return value will be the
  value @code{:empty} of the @symbol{gtk-image-type} enumeration.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-symbol{gtk-image-type}"
  (gtk-status-icon-storage-type status-icon))

(export 'gtk-status-icon-get-storage-type)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_pixbuf ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-pixbuf))

(defun gtk-status-icon-get-pixbuf (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @begin{return}
    The displayed pixbuf, or @code{nil} if the image is empty.
  @end{return}
  @begin{short}
    Gets the @class{gdk-pixbuf} being displayed by the @class{gtk-status-icon}.
  @end{short}
  The storage type of the status icon must be the value @code{:empty} or
  @code{:pixbuf}. See the function @fun{gtk-status-icon-get-storage-type}. The
  caller of this function does not own a reference to the returned pixbuf.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-get-storage-type}"
  (gtk-status-icon-pixbuf status-icon))

(export 'gtk-status-icon-get-pixubf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-stock))

(defun gtk-status-icon-get-stock (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @begin{return}
    Stock ID of the displayed stock icon, or @code{nil} if the image is empty.
  @end{return}
  @begin{short}
    Gets the ID of the stock icon being displayed by the
    @class{gtk-status-icon}.
  @end{short}
  The storage type of the status icon must be the value @code{:empty} or
  @code{:stock} of the @symbol{gtk-image-type} enumeration. See the function
  @fun{gtk-status-icon-get-storage-type}. The returned string is owned by
  the @class{gtk-status-icon} and should not be freed or modified.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-get-storage-type}"
  (gtk-status-icon-stock status-icon))

(export 'gtk-status-icon-get-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-icon-name))

(defun gtk-status-icon-get-icon-name (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @begin{return}
    Name of the displayed icon, or @code{nil} if the image is empty.
  @end{return}
  @begin{short}
    Gets the name of the icon being displayed by the @class{gtk-status-icon}.
  @end{short}
  The storage type of the status icon must be the value @code{:empty} or
  @code{:icon-name} of the @symbol{gtk-image-type} enumeration. See the function
  @fun{gtk-status-icon-get-storage-type}. The returned string is owned by the
  @class{gtk-status-icon} and should not be freed or modified.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-get-storage-type}"
  (gtk-status-icon-icon-name status-icon))

(export 'gtk-status-icon-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-gicon))

(defun gtk-status-icon-get-gicon (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{The displayed icon, or @code{nil} if the image is empty.}
  @begin{short}
    Retrieves the @class{g-icon} being displayed by the @class{gtk-status-icon}.
  @end{short}
  The storage type of the status icon must be the value @code{:empty} or
  @code{:gicon} of the @symbol{gtk-image-type} enumeration. See the function
  @fun{gtk-status-icon-get-storage-type}. The caller of this function does not
  own a reference to the returned @class{g-icon}.

  If this function fails, icon is left unchanged.

  Since 2.14
  @see-class{gtk-status-icon}
  @see-class{g-icon}
  @see-symbol{gtk-image-type}
  @see-function{gtk-status-icon-get-storage-type}"
  (gtk-status-icon-get-gicon status-icon))

(export 'gtk-status-icon-get-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-size))

(defun gtk-status-icon-get-size (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{The size that is available for the image.}
  @begin{short}
    Gets the size in pixels that is available for the image.
  @end{short}
  Stock icons and named icons adapt their size automatically if the size of the
  notification area changes. For other storage types, the \"size-changed\"
  signal can be used to react to size changes.

  Note that the returned size is only meaningful while the status icon is
  embedded. See the function @fun{gtk-status-icon-is-embedded}.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-is-embedded}"
  (gtk-status-icon-size status-icon))

(export 'gtk-status-icon-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-set-screen))

(defun gtk-status-icon-set-screen (status-icon screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Sets the @class{gdk-screen} object where @arg{status-icon} is displayed; if
    the icon is already mapped, it will be unmapped, and then remapped on the
    new screen.
  @end{short}

  Since 2.12
  @see-class{gtk-status-icon}
  @see-class{gdk-screen}
  @see-function{gtk-status-icon-get-screen}"
  (setf (gtk-status-icon-screen status-icon) screen))

(export 'gtk-status-icon-set-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-screen))

(defun gtk-status-icon-get-screen (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{a @class{gdk-screen} object}
  @begin{short}
    Returns the @class{gdk-screen} object associated with @arg{status-icon}.
  @end{short}

  Since 2.12
  @see-class{gtk-status-icon}
  @see-class{gdk-screen}
  @see-function{gtk-status-icon-set-screen}"
  (gtk-status-icon-screen status-icon))

(export 'gtk-status-icon-get-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_tooltip_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-set-tooltip-text))

(defun gtk-status-icon-set-tooltip-text (status-icon text)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[text]{the contents of the tooltip for @arg{status-icon}}
  @begin{short}
    Sets text as the contents of the tooltip.
  @end{short}

  This function will take care of setting the @code{\"has-tooltip\"} property
  to @em{true} and of the default handler for the \"query-tooltip\" signal.

  See also the @code{\"tooltip-text\"} property and the function
  @fun{gtk-tooltip-set-text}.

  Since 2.16
  @see-class{gtk-status-icon}
  @see-function{gtk-tooltip-set-text}
  @see-function{gtk-status-icon-get-tooltip-text}"
  (setf (gtk-status-icon-tooltip-text status-icon) text))

(export 'gtk-status-icon-set-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_tooltip_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-tooltip-text))

(defun gtk-status-icon-get-tooltip-text (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @begin{return}
    The tooltip text, or @code{nil}.
  @end{return}
  @begin{short}
    Gets the contents of the tooltip for @arg{status-icon}.
  @end{short}

  Since 2.16
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-set-tooltip-text}"
  (gtk-status-icon-tooltip-text status-icon))

(export 'gtk-status-icon-get-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-set-tooltip-markup))

(defun gtk-status-icon-set-tooltip-markup (status-icon markup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[markup]{the contents of the tooltip for @arg{status-icon},
    or @code{nil}}
  @begin{short}
    Sets markup as the contents of the tooltip, which is marked up with the
    Pango text markup language.
  @end{short}

  This function will take care of setting the @code{\"has-tooltip\"} property
  to @em{true} and of the default handler for the \"query-tooltip\" signal.

  See also the @code{\"tooltip-markup\"} property and the function
  @fun{gtk-tooltip-set-markup}.

  Since 2.16
  @see-class{gtk-status-icon}
  @see-function{gtk-tooltip-set-markup}
  @see-function{gtk-status-icon-get-tooltip-markup}"
  (setf (gtk-status-icon-tooltip-markup status-icon) markup))

(export 'gtk-status-icon-set-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-tooltip-markup))

(defun gtk-status-icon-get-tooltip-markup (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{The tooltip text, or @code{nil}.}
  @begin{short}
    Gets the contents of the tooltip for @arg{status-icon}.
  @end{short}

  Since 2.16
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-set-tooltip-markup}"
  (gtk-status-icon-tooltip-markup status-icon))

(export 'gtk-status-icon-get-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_has_tooltip ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-set-has-tooltip))

(defun gtk-status-icon-set-has-tooltip (status-icon has-tooltip)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[has-tooltip]{whether or not @arg{status-icon} has a tooltip}
  @begin{short}
    Sets the @code{\"has-tooltip\"} property on @arg{status-icon} to
    @arg{has-tooltip}.
  @end{short}
  See the @code{\"has-tooltip\"} for more information.

  Since 2.16
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-has-tooltip}"
  (setf (gtk-status-icon-has-tooltip status-icon) has-tooltip))

(export 'gtk-status-icon-set-has-tooltip)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_has_tooltip ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-has-tooltip))

(defun gtk-status-icon-get-has-tooltip (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @begin{return}
    Current value of the @code{\"has-tooltip\"} property on @arg{status-icon}.
  @end{return}
  @begin{short}
    Returns the current value of the @code{\"has-tooltip\"} property.
  @end{short}
  See the @code{\"has-tooltip\"} property for more information.

  Since 2.16
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-set-has-tooltip}"
  (gtk-status-icon-has-tooltip status-icon))

(export 'gtk-status-icon-get-has-tooltip)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-set-title))

(defun gtk-status-icon-set-title (status-icon title)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[title]{the title}
  @begin{short}
    Sets the title of this tray icon.
  @end{short}
  This should be a short, human-readable, localized string describing the tray
  icon. It may be used by tools like screen readers to render the tray icon.

  Since 2.18
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-title}"
  (setf (gtk-status-icon-title status-icon) title))

(export 'gtk-status-icon-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-title))

(defun gtk-status-icon-get-title (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{The title of the status icon.}
  @begin{short}
    Gets the title of this tray icon.
  @end{short}
  See the function @fun{gtk-status-icon-set-title}.

  Since 2.18
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-set-title}"
  (gtk-status-icon-title status-icon))

(export 'gtk-status-icon-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_name" gtk-status-icon-set-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[name]{the name}
  @begin{short}
    Sets the name of this tray icon.
  @end{short}
  This should be a string identifying this icon. It may be used for sorting the
  icons in the tray and will not be shown to the user.

  Since 2.20
  @see-class{gtk-status-icon}"
  (status-icon (g-object gtk-status-icon))
  (name :string))

(export 'gtk-status-icon-set-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-set-visible))

(defun gtk-status-icon-set-visible (status-icon visible)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @argument[visible]{@em{true} to show the status icon, @code{nil} to hide it}
  @begin{short}
    Shows or hides a status icon.
  @end{short}

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-get-visible}"
  (setf (gtk-status-icon-visible status-icon) visible))

(export 'gtk-status-icon-set-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-status-icon-get-visible))

(defun gtk-status-icon-get-visible (status-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{@em{true} if the status icon is visible}
  @begin{short}
    Returns whether the status icon is visible or not.
  @end{short}
  Note that being visible does not guarantee that the user can actually see the
  icon, see also the function @fun{gtk-status-icon-is-embedded}.

  Since 2.10
  @see-class{gtk-status-icon}
  @see-function{gtk-status-icon-is-embedded}
  @see-function{gtk-status-icon-set-visible}"
  (gtk-status-icon-visible status-icon))

(export 'gtk-status-icon-get-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_is_embedded ()
;;; ----------------------------------------------------------------------------

;; TODO: Replace this code with a call the accessor gtk-status-icon-embedded

(defcfun ("gtk_status_icon_is_embedded" gtk-status-icon-is-embedded) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[status-icon]{a @class{gtk-status-icon} widget}
  @return{@em{True} if the status icon is embedded in a notification area.}
  @begin{short}
    Returns whether the status icon is embedded in a notification area.
  @end{short}

  Since 2.10
  @see-class{gtk-status-icon}"
  (status-icon (g-object gtk-status-icon)))

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
;;; Obtains information about the location of the status icon on screen. This
;;; information can be used to e.g. position popups like notification bubbles.
;;;
;;; See gtk_status_icon_position_menu() for a more convenient alternative for
;;; positioning menus.
;;;
;;; Note that some platforms do not allow GTK+ to provide this information, and
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
