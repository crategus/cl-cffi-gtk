;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.status-icon.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; 
;;; 
;;; 
;;; Signals
;;; 
;;;   "activate"                                       : Action
;;;   "button-press-event"                             : Run Last
;;;   "button-release-event"                           : Run Last
;;;   "popup-menu"                                     : Action
;;;   "query-tooltip"                                  : Run Last
;;;   "scroll-event"                                   : Run Last
;;;   "size-changed"                                   : Run Last
;;; 
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;; 
;;; void user_function (GtkStatusIcon *status_icon,
;;;                     gpointer       user_data)        : Action
;;; 
;;; Gets emitted when the user activates the status icon. If and how status
;;; icons can activated is platform-dependent.
;;; 
;;; Unlike most G_SIGNAL_ACTION signals, this signal is meant to be used by
;;; applications and should be wrapped by language bindings.
;;; 
;;; status_icon :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-press-event" signal
;;; 
;;; gboolean user_function (GtkStatusIcon *status_icon,
;;;                         GdkEvent      *event,
;;;                         gpointer       user_data)        : Run Last
;;; 
;;; The ::button-press-event signal will be emitted when a button (typically
;;; from a mouse) is pressed.
;;; 
;;; Whether this event is emitted is platform-dependent. Use the ::activate and
;;; ::popup-menu signals in preference.
;;; 
;;; status_icon :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventButton which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-release-event" signal
;;; 
;;; gboolean user_function (GtkStatusIcon *status_icon,
;;;                         GdkEvent      *event,
;;;                         gpointer       user_data)        : Run Last
;;; 
;;; The ::button-release-event signal will be emitted when a button (typically
;;; from a mouse) is released.
;;; 
;;; Whether this event is emitted is platform-dependent. Use the ::activate and
;;; ::popup-menu signals in preference.
;;; 
;;; status_icon :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventButton which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-menu" signal
;;; 
;;; void user_function (GtkStatusIcon *status_icon,
;;;                     guint          button,
;;;                     guint          activate_time,
;;;                     gpointer       user_data)          : Action
;;; 
;;; Gets emitted when the user brings up the context menu of the status icon.
;;; Whether status icons can have context menus and how these are activated is
;;; platform-dependent.
;;; 
;;; The button and activate_time parameters should be passed as the last to
;;; arguments to gtk_menu_popup().
;;; 
;;; Unlike most G_SIGNAL_ACTION signals, this signal is meant to be used by
;;; applications and should be wrapped by language bindings.
;;; 
;;; status_icon :
;;;     the object which received the signal
;;; 
;;; button :
;;;     the button that was pressed, or 0 if the signal is not emitted in
;;;     response to a button press event
;;; 
;;; activate_time :
;;;     the timestamp of the event that triggered the signal emission
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "query-tooltip" signal
;;; 
;;; gboolean user_function (GtkStatusIcon *status_icon,
;;;                         gint           x,
;;;                         gint           y,
;;;                         gboolean       keyboard_mode,
;;;                         GtkTooltip    *tooltip,
;;;                         gpointer       user_data)          : Run Last
;;; 
;;; Emitted when the "gtk-tooltip-timeout" has expired with the cursor hovering
;;; above status_icon; or emitted when status_icon got focus in keyboard mode.
;;; 
;;; Using the given coordinates, the signal handler should determine whether a
;;; tooltip should be shown for status_icon. If this is the case TRUE should be
;;; returned, FALSE otherwise. Note that if keyboard_mode is TRUE, the values of
;;; x and y are undefined and should not be used.
;;; 
;;; The signal handler is free to manipulate tooltip with the therefore destined
;;; function calls.
;;; 
;;; Whether this signal is emitted is platform-dependent. For plain text
;;; tooltips, use "tooltip-text" in preference.
;;; 
;;; status_icon :
;;;     the object which received the signal
;;; 
;;; x :
;;;     the x coordinate of the cursor position where the request has been
;;;     emitted, relative to status_icon
;;; 
;;; y :
;;;     the y coordinate of the cursor position where the request has been
;;;     emitted, relative to status_icon
;;; 
;;; keyboard_mode :
;;;     TRUE if the tooltip was trigged using the keyboard
;;; 
;;; tooltip :
;;;     a GtkTooltip
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if tooltip should be shown right now, FALSE otherwise.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "scroll-event" signal
;;; 
;;; gboolean user_function (GtkStatusIcon *status_icon,
;;;                         GdkEvent      *event,
;;;                         gpointer       user_data)        : Run Last
;;; 
;;; The ::scroll-event signal is emitted when a button in the 4 to 7 range is
;;; pressed. Wheel mice are usually configured to generate button press events
;;; for buttons 4 and 5 when the wheel is turned.
;;; 
;;; Whether this event is emitted is platform-dependent.
;;; 
;;; status_icon :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEventScroll which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-changed" signal
;;; 
;;; gboolean user_function (GtkStatusIcon *status_icon,
;;;                         gint           size,
;;;                         gpointer       user_data)        : Run Last
;;; 
;;; Gets emitted when the size available for the image changes, e.g. because the
;;; notification area got resized.
;;; 
;;; status_icon :
;;;     the object which received the signal
;;; 
;;; size :
;;;     the new size
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the icon was updated for the new size. Otherwise, GTK+ will
;;;     scale the icon as necessary.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; --- gtk-status-icon --------------------------------------------------------

(setf (documentation 'gtk-status-icon 'type)
 "@version{2013-2-3}
  @begin{short}
    The \"system tray\" or notification area is normally used for transient
    icons that indicate some special state. For example, a system tray icon
    might appear to tell the user that they have new mail, or have an incoming
    instant message, or something along those lines. The basic idea is that
    creating an icon in the notification area is less annoying than popping up a
    dialog.
  @end{short}

  A GtkStatusIcon object can be used to display an icon in a \"system tray\".
  The icon can have a tooltip, and the user can interact with it by activating
  it or popping up a context menu. Critical information should not solely be
  displayed in a GtkStatusIcon, since it may not be visible (e.g. when the
  user doesn't have a notification area on his panel). This can be checked
  with gtk_status_icon_is_embedded().

  On X11, the implementation follows the freedesktop.org \"System Tray\"
  specification. Implementations of the \"tray\" side of this specification can
  be found e.g. in the GNOME 2 and KDE panel applications.

  Note that a GtkStatusIcon is not a widget, but just a GObject. Making it a
  widget would be impractical, since the system tray on Win32 doesn't allow to
  embed arbitrary widgets.

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
  @see-slot{gtk-status-icon-visible}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "embedded" 'gtk-status-icon) 't)
 "The @code{\"embedded\"} property of type @code{gboolean} (Read)@br{}
  @arg{true} if the statusicon is embedded in a notification area.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "file" 'gtk-status-icon) 't)
 "The @code{\"file\"} property of type @code{gchar*} (Write)@br{}
  Filename to load and display.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-status-icon) 't)
 "The @code{\"gicon\"} property of type @code{GIcon*} (Read / Write)@br{}
  The @code{GIcon} displayed in the @sym{gtk-status-icon}. For themed icons, the
  image will be updated automatically if the theme changes.@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-tooltip" 'gtk-status-icon) 't)
 "The @code{\"has-tooltip\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Enables or disables the emission of \"query-tooltip\" on status_icon. A value
  of @arg{true} indicates that status_icon can have a tooltip, in this case the
  status icon will be queried using \"query-tooltip\" to determine whether it
  will provide a tooltip or not.
  Note that setting this property to @arg{true} for the first time will change
  the event masks of the windows of this status icon to include leave-notify and
  motion-notify events. This will not be undone when the property is set to
  @code{nil} again.
  Whether this property is respected is platform dependent. For plain text
  tooltips, use @code{\"tooltip-text\"} in preference.@br{}
  Default value: @code{nil}@br{}
  Since 2.16")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-status-icon) 't)
 "The @code{\"icon-name\"} property of type @code{gchar*} (Read / Write)@br{}
  The name of the icon from the icon theme.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "orientation" 'gtk-status-icon) 't)
 "The @code{\"orientation\"} property of type @symbol{gtk-orientation}
  (Read)@br{}
  The orientation of the tray in which the status icon is embedded.@br{}
  Default value: @code{:horizontal}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "pixbuf" 'gtk-status-icon) 't)
 "The @code{\"pixbuf\"} property of type @class{gdk-pixbuf} (Read / Write)@br{}
  A @class{gdk-pixbuf} to display.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-status-icon) 't)
 "The @code{\"screen\"} property of type @class{gdk-screen} (Read / Write)@br{}
  The screen where this status icon will be displayed.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-status-icon) 't)
 "The @code{\"size\"} property of type @code{gint} (Read)@br{}
  The size of the icon.@br{}
  Allowed values: @code{>= 0}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "stock" 'gtk-status-icon) 't)
 "The @code{\"stock\"} property of type @code{gchar*} (Read / Write)@br{}
  Stock ID for a stock image to display.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "storage-type" 'gtk-status-icon) 't)
 "The @code{\"storage-type\"} property of type @symbol{gtk-image-type}
  (Read)@br{}
  The representation being used for image data.@br{}
  Default value: @code{:empty}")
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-status-icon) 't)
 "The @code{\"title\"} property of type @code{gchar*} @code{Read / Write}@br{}
  The title of this tray icon. This should be a short, human-readable,
  localized string describing the tray icon. It may be used by tools like
  screen readers to render the tray icon.@br{}
  Default value: @code{nil}@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "tooltip-markup" 'gtk-status-icon) 't)
 "The @code{\"tooltip-markup\"} property of type @code{gchar*}
  (Read / Write)@br{}
  Sets the text of tooltip to be the given string, which is marked up with the
  Pango text markup language. Also see @fun{gtk-tooltip-set-markup}.
  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not @code{nil}. @code{\"has-tooltip\"} will
  automatically be set to @arg{true} and the default handler for the
  \"query-tooltip\" signal will take care of displaying the tooltip.
  On some platforms, embedded markup will be ignored.@br{}
  Default value: @code{nil}@br{}
  Since 2.16")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "tooltip-text" 'gtk-status-icon) 't)
 "The @code{\"tooltip-text\"} property of type @code{gchar*} (Read / Write)@br{}
  Sets the text of tooltip to be the given string.
  Also see @fun{gtk-tooltip-set-text}.
  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not @code{nil}. @code{\"has-tooltip\"} will
  automatically be set to @arg{true} and the default handler for the
  \"query-tooltip\" signal will take care of displaying the tooltip.
  Note that some platforms have limitations on the length of tooltips that
  they allow on status icons, e.g. Windows only shows the first 64
  characters.@br{}
  Default value: @code{nil}
  Since 2.16")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-status-icon) 't)
 "The @code{\"visible\"} property of type @code{gboolean} (Read / Write)@br{}
  Whether the status icon is visible.@br{}
  Default value: @arg{true}")

;;; --- End of file atdoc-gtk.status-icon.lisp ---------------------------------
