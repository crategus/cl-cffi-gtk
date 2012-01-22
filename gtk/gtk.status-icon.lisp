;;; ----------------------------------------------------------------------------
;;; gtk.status-icon.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkStatusIcon
;;; 
;;; Properties
;;; 
;;;   "embedded"                 gboolean              : Read
;;;   "file"                     gchar*                : Write
;;;   "gicon"                    GIcon*                : Read / Write
;;;   "has-tooltip"              gboolean              : Read / Write
;;;   "icon-name"                gchar*                : Read / Write
;;;   "orientation"              GtkOrientation        : Read
;;;   "pixbuf"                   GdkPixbuf*            : Read / Write
;;;   "screen"                   GdkScreen*            : Read / Write
;;;   "size"                     gint                  : Read
;;;   "stock"                    gchar*                : Read / Write
;;;   "storage-type"             GtkImageType          : Read
;;;   "title"                    gchar*                : Read / Write
;;;   "tooltip-markup"           gchar*                : Read / Write
;;;   "tooltip-text"             gchar*                : Read / Write
;;;   "visible"                  gboolean              : Read / Write
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
;;; Description
;;; 
;;; The "system tray" or notification area is normally used for transient icons
;;; that indicate some special state. For example, a system tray icon might
;;; appear to tell the user that they have new mail, or have an incoming instant
;;; message, or something along those lines. The basic idea is that creating an
;;; icon in the notification area is less annoying than popping up a dialog.
;;; 
;;; A GtkStatusIcon object can be used to display an icon in a "system tray".
;;; The icon can have a tooltip, and the user can interact with it by activating
;;; it or popping up a context menu. Critical information should not solely be
;;; displayed in a GtkStatusIcon, since it may not be visible (e.g. when the
;;; user doesn't have a notification area on his panel). This can be checked
;;; with gtk_status_icon_is_embedded().
;;; 
;;; On X11, the implementation follows the freedesktop.org "System Tray"
;;; specification. Implementations of the "tray" side of this specification can
;;; be found e.g. in the GNOME 2 and KDE panel applications.
;;; 
;;; Note that a GtkStatusIcon is not a widget, but just a GObject. Making it a
;;; widget would be impractical, since the system tray on Win32 doesn't allow
;;; to embed arbitrary widgets.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "embedded" property
;;; 
;;;   "embedded"                 gboolean              : Read
;;; 
;;; TRUE if the statusicon is embedded in a notification area.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "file" property
;;; 
;;;   "file"                     gchar*                : Write
;;; 
;;; Filename to load and display.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gicon" property
;;; 
;;;   "gicon"                    GIcon*                : Read / Write
;;; 
;;; The GIcon displayed in the GtkStatusIcon. For themed icons, the image will
;;; be updated automatically if the theme changes.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-tooltip" property
;;; 
;;;   "has-tooltip"              gboolean              : Read / Write
;;; 
;;; Enables or disables the emission of "query-tooltip" on status_icon. A value
;;; of TRUE indicates that status_icon can have a tooltip, in this case the
;;; status icon will be queried using "query-tooltip" to determine whether it
;;; will provide a tooltip or not.
;;; 
;;; Note that setting this property to TRUE for the first time will change the
;;; event masks of the windows of this status icon to include leave-notify and
;;; motion-notify events. This will not be undone when the property is set to
;;; FALSE again.
;;; 
;;; Whether this property is respected is platform dependent. For plain text
;;; tooltips, use "tooltip-text" in preference.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-name" property
;;; 
;;;   "icon-name"                gchar*                : Read / Write
;;; 
;;; The name of the icon from the icon theme.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "orientation" property
;;; 
;;;   "orientation"              GtkOrientation        : Read
;;; 
;;; The orientation of the tray in which the statusicon is embedded.
;;; 
;;; Default value: GTK_ORIENTATION_HORIZONTAL
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf" property
;;; 
;;;   "pixbuf"                   GdkPixbuf*            : Read / Write
;;; 
;;; A GdkPixbuf to display.
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen" property
;;; 
;;;   "screen"                   GdkScreen*            : Read / Write
;;; 
;;; The screen where this status icon will be displayed.
;;;
;;; ----------------------------------------------------------------------------
;;; The "size" property
;;; 
;;;   "size"                     gint                  : Read
;;; 
;;; The size of the icon.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "stock" property
;;; 
;;;   "stock"                    gchar*                : Read / Write
;;; 
;;; Stock ID for a stock image to display.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "storage-type" property
;;; 
;;;   "storage-type"             GtkImageType          : Read
;;; 
;;; The representation being used for image data.
;;; 
;;; Default value: GTK_IMAGE_EMPTY
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" property
;;; 
;;;   "title"                    gchar*                : Read / Write
;;; 
;;; The title of this tray icon. This should be a short, human-readable,
;;; localized string describing the tray icon. It may be used by tools like
;;; screen readers to render the tray icon.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip-markup" property
;;; 
;;;   "tooltip-markup"           gchar*                : Read / Write
;;; 
;;; Sets the text of tooltip to be the given string, which is marked up with
;;; the Pango text markup language. Also see gtk_tooltip_set_markup().
;;; 
;;; This is a convenience property which will take care of getting the tooltip
;;; shown if the given string is not NULL. "has-tooltip" will automatically be
;;; set to TRUE and the default handler for the "query-tooltip" signal will
;;; take care of displaying the tooltip.
;;; 
;;; On some platforms, embedded markup will be ignored.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip-text" property
;;; 
;;;   "tooltip-text"             gchar*                : Read / Write
;;; 
;;; Sets the text of tooltip to be the given string.
;;; 
;;; Also see gtk_tooltip_set_text().
;;; 
;;; This is a convenience property which will take care of getting the tooltip
;;; shown if the given string is not NULL. "has-tooltip" will automatically be
;;; set to TRUE and the default handler for the "query-tooltip" signal will
;;; take care of displaying the tooltip.
;;; 
;;; Note that some platforms have limitations on the length of tooltips that
;;; they allow on status icons, e.g. Windows only shows the first 64 characters.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible" property
;;; 
;;;   "visible"                  gboolean              : Read / Write
;;; 
;;; Whether the status icon is visible.
;;; 
;;; Default value: TRUE
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
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
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
;;; Whether this event is emitted is platform-dependent. Use the ::activate
;;; and ::popup-menu signals in preference.
;;; 
;;; status_icon :
;;; 	the object which received the signal
;;; 
;;; event :
;;; 	the GdkEventButton which triggered this signal. [type Gdk.EventButton]
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
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
;;; Whether this event is emitted is platform-dependent. Use the ::activate
;;; and ::popup-menu signals in preference.
;;; 
;;; status_icon :
;;; 	the object which received the signal
;;; 
;;; event :
;;; 	the GdkEventButton which triggered this signal. [type Gdk.EventButton]
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
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
;;; 	the object which received the signal
;;; 
;;; button :
;;; 	the button that was pressed, or 0 if the signal is not emitted in
;;;     response to a button press event
;;; 
;;; activate_time :
;;; 	the timestamp of the event that triggered the signal emission
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
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
;;; returned, FALSE otherwise. Note that if keyboard_mode is TRUE, the values
;;; of x and y are undefined and should not be used.
;;; 
;;; The signal handler is free to manipulate tooltip with the therefore
;;; destined function calls.
;;; 
;;; Whether this signal is emitted is platform-dependent. For plain text
;;; tooltips, use "tooltip-text" in preference.
;;; 
;;; status_icon :
;;; 	the object which received the signal
;;; 
;;; x :
;;; 	the x coordinate of the cursor position where the request has been
;;;     emitted, relative to status_icon
;;; 
;;; y :
;;; 	the y coordinate of the cursor position where the request has been
;;;     emitted, relative to status_icon
;;; 
;;; keyboard_mode :
;;; 	TRUE if the tooltip was trigged using the keyboard
;;; 
;;; tooltip :
;;; 	a GtkTooltip
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE if tooltip should be shown right now, FALSE otherwise.
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
;;; 	the object which received the signal.
;;; 
;;; event :
;;; 	the GdkEventScroll which triggered this signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
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
;;; Gets emitted when the size available for the image changes, e.g. because
;;; the notification area got resized.
;;; 
;;; status_icon :
;;; 	the object which received the signal
;;; 
;;; size :
;;; 	the new size
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE if the icon was updated for the new size. Otherwise, GTK+ will
;;;     scale the icon as necessary.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStatusIcon
;;; 
;;; struct GtkStatusIcon;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStatusIcon" gtk-status-icon
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_status_icon_get_type")
  ((blinking gtk-status-icon-blinking "blinking" "gboolean" t t)
   (embedded gtk-status-icon-embedded "embedded" "gboolean" t nil)
   (file gtk-status-icon-file "file" "gchararray" nil t)
   (gicon gtk-status-icon-gicon "gicon" "GIcon" t t)
   (has-tooltip gtk-status-icon-has-tooltip "has-tooltip" "gboolean" t t)
   (icon-name gtk-status-icon-icon-name "icon-name" "gchararray" t t)
   (orientation gtk-status-icon-orientation
    "orientation" "GtkOrientation" t nil)
   (pixbuf gtk-status-icon-pixbuf "pixbuf" "GdkPixbuf" t t)
   (screen gtk-status-icon-screen "screen" "GdkScreen" t t)
   (size gtk-status-icon-size "size" "gint" t nil)
   (stock gtk-status-icon-stock "stock" "gchararray" t t)
   (storage-type gtk-status-icon-storage-type
    "storage-type" "GtkImageType" t nil)
   (tooltip-markup gtk-status-icon-tooltip-markup
    "tooltip-markup" "gchararray" t t)
   (tooltip-text gtk-status-icon-tooltip-text "tooltip-text" "gchararray" t t)
   (visible gtk-status-icon-visible "visible" "gboolean" t t)))

;;; ---------------------------------------------------------------------------- 
;;; gtk_status_icon_new ()
;;; 
;;; GtkStatusIcon * gtk_status_icon_new (void);
;;; 
;;; Creates an empty status icon object.
;;; 
;;; Returns :
;;; 	a new GtkStatusIcon
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_pixbuf ()
;;; 
;;; GtkStatusIcon * gtk_status_icon_new_from_pixbuf (GdkPixbuf *pixbuf);
;;; 
;;; Creates a status icon displaying pixbuf.
;;; 
;;; The image will be scaled down to fit in the available space in the
;;; notification area, if necessary.
;;; 
;;; pixbuf :
;;; 	a GdkPixbuf
;;; 
;;; Returns :
;;; 	a new GtkStatusIcon
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_file ()
;;; 
;;; GtkStatusIcon * gtk_status_icon_new_from_file (const gchar *filename);
;;; 
;;; Creates a status icon displaying the file filename.
;;; 
;;; The image will be scaled down to fit in the available space in the
;;; notification area, if necessary.
;;; 
;;; filename :
;;; 	a filename. [type filename]
;;; 
;;; Returns :
;;; 	a new GtkStatusIcon
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_stock ()
;;; 
;;; GtkStatusIcon * gtk_status_icon_new_from_stock (const gchar *stock_id);
;;; 
;;; Creates a status icon displaying a stock icon. Sample stock icon names are
;;; GTK_STOCK_OPEN, GTK_STOCK_QUIT. You can register your own stock icon names,
;;; see gtk_icon_factory_add_default() and gtk_icon_factory_add().
;;; 
;;; stock_id :
;;; 	a stock icon id
;;; 
;;; Returns :
;;; 	a new GtkStatusIcon
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_icon_name ()
;;; 
;;; GtkStatusIcon * gtk_status_icon_new_from_icon_name (const gchar *icon_name)
;;; 
;;; Creates a status icon displaying an icon from the current icon theme. If
;;; the current icon theme is changed, the icon will be updated appropriately.
;;; 
;;; icon_name :
;;; 	an icon name
;;; 
;;; Returns :
;;; 	a new GtkStatusIcon
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_gicon ()
;;; 
;;; GtkStatusIcon * gtk_status_icon_new_from_gicon (GIcon *icon);
;;; 
;;; Creates a status icon displaying a GIcon. If the icon is a themed icon,
;;; it will be updated when the theme changes.
;;; 
;;; icon :
;;; 	a GIcon
;;; 
;;; Returns :
;;; 	a new GtkStatusIcon
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_pixbuf ()
;;; 
;;; void gtk_status_icon_set_from_pixbuf (GtkStatusIcon *status_icon,
;;;                                       GdkPixbuf *pixbuf);
;;; 
;;; Makes status_icon display pixbuf. See gtk_status_icon_new_from_pixbuf() for
;;; details.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; pixbuf :
;;; 	a GdkPixbuf or NULL.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_file ()
;;; 
;;; void gtk_status_icon_set_from_file (GtkStatusIcon *status_icon,
;;;                                     const gchar *filename);
;;; 
;;; Makes status_icon display the file filename. See
;;; gtk_status_icon_new_from_file() for details.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; filename :
;;; 	a filename.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_stock ()
;;; 
;;; void gtk_status_icon_set_from_stock (GtkStatusIcon *status_icon,
;;;                                      const gchar *stock_id);
;;; 
;;; Makes status_icon display the stock icon with the id stock_id. See
;;; gtk_status_icon_new_from_stock() for details.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; stock_id :
;;; 	a stock icon id
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_icon_name ()
;;; 
;;; void gtk_status_icon_set_from_icon_name (GtkStatusIcon *status_icon,
;;;                                          const gchar *icon_name);
;;; 
;;; Makes status_icon display the icon named icon_name from the current icon
;;; theme. See gtk_status_icon_new_from_icon_name() for details.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; icon_name :
;;; 	an icon name
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_gicon ()
;;; 
;;; void gtk_status_icon_set_from_gicon (GtkStatusIcon *status_icon,
;;;                                      GIcon *icon);
;;; 
;;; Makes status_icon display the GIcon. See gtk_status_icon_new_from_gicon()
;;; for details.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; icon :
;;; 	a GIcon
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_storage_type ()
;;; 
;;; GtkImageType gtk_status_icon_get_storage_type (GtkStatusIcon *status_icon);
;;; 
;;; Gets the type of representation being used by the GtkStatusIcon to store
;;; image data. If the GtkStatusIcon has no image data, the return value will
;;; be GTK_IMAGE_EMPTY.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the image representation being used
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_pixbuf ()
;;; 
;;; GdkPixbuf * gtk_status_icon_get_pixbuf (GtkStatusIcon *status_icon);
;;; 
;;; Gets the GdkPixbuf being displayed by the GtkStatusIcon. The storage type
;;; of the status icon must be GTK_IMAGE_EMPTY or GTK_IMAGE_PIXBUF (see
;;; gtk_status_icon_get_storage_type()). The caller of this function does not
;;; own a reference to the returned pixbuf.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the displayed pixbuf, or NULL if the image is empty.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_stock ()
;;; 
;;; const gchar * gtk_status_icon_get_stock (GtkStatusIcon *status_icon);
;;; 
;;; Gets the id of the stock icon being displayed by the GtkStatusIcon. The
;;; storage type of the status icon must be GTK_IMAGE_EMPTY or GTK_IMAGE_STOCK
;;; (see gtk_status_icon_get_storage_type()). The returned string is owned by
;;; the GtkStatusIcon and should not be freed or modified.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	stock id of the displayed stock icon, or NULL if the image is empty.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_icon_name ()
;;; 
;;; const gchar * gtk_status_icon_get_icon_name (GtkStatusIcon *status_icon);
;;; 
;;; Gets the name of the icon being displayed by the GtkStatusIcon. The storage
;;; type of the status icon must be GTK_IMAGE_EMPTY or GTK_IMAGE_ICON_NAME (see
;;; gtk_status_icon_get_storage_type()). The returned string is owned by the
;;; GtkStatusIcon and should not be freed or modified.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	name of the displayed icon, or NULL if the image is empty.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_gicon ()
;;; 
;;; GIcon * gtk_status_icon_get_gicon (GtkStatusIcon *status_icon);
;;; 
;;; Retrieves the GIcon being displayed by the GtkStatusIcon. The storage type
;;; of the status icon must be GTK_IMAGE_EMPTY or GTK_IMAGE_GICON (see
;;; gtk_status_icon_get_storage_type()). The caller of this function does not
;;; own a reference to the returned GIcon.
;;; 
;;; If this function fails, icon is left unchanged;
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the displayed icon, or NULL if the image is empty.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_size ()
;;; 
;;; gint gtk_status_icon_get_size (GtkStatusIcon *status_icon);
;;; 
;;; Gets the size in pixels that is available for the image. Stock icons and
;;; named icons adapt their size automatically if the size of the notification
;;; area changes. For other storage types, the size-changed signal can be used
;;; to react to size changes.
;;; 
;;; Note that the returned size is only meaningful while the status icon is
;;; embedded (see gtk_status_icon_is_embedded()).
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the size that is available for the image
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_screen ()
;;; 
;;; void gtk_status_icon_set_screen (GtkStatusIcon *status_icon,
;;;                                  GdkScreen *screen);
;;; 
;;; Sets the GdkScreen where status_icon is displayed; if the icon is already
;;; mapped, it will be unmapped, and then remapped on the new screen.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; screen :
;;; 	a GdkScreen
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_screen ()
;;; 
;;; GdkScreen * gtk_status_icon_get_screen (GtkStatusIcon *status_icon);
;;; 
;;; Returns the GdkScreen associated with status_icon.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	a GdkScreen.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_tooltip_text ()
;;; 
;;; void gtk_status_icon_set_tooltip_text (GtkStatusIcon *status_icon,
;;;                                        const gchar *text);
;;; 
;;; Sets text as the contents of the tooltip.
;;; 
;;; This function will take care of setting "has-tooltip" to TRUE and of the
;;; default handler for the "query-tooltip" signal.
;;; 
;;; See also the "tooltip-text" property and gtk_tooltip_set_text().
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; text :
;;; 	the contents of the tooltip for status_icon
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_status_icon_set_tooltip_text"
          gtk-status-icon-set-tooltip-text) :void
  (status-icon (g-object gtk-status-icon))
  (tooltip-text :string))

(export 'gtk-status-icon-set-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_tooltip_text ()
;;; 
;;; gchar * gtk_status_icon_get_tooltip_text (GtkStatusIcon *status_icon);
;;; 
;;; Gets the contents of the tooltip for status_icon.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the tooltip text, or NULL. You should free the returned string with
;;;     g_free() when done.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_tooltip_markup ()
;;; 
;;; void gtk_status_icon_set_tooltip_markup (GtkStatusIcon *status_icon,
;;;                                          const gchar *markup);
;;; 
;;; Sets markup as the contents of the tooltip, which is marked up with the
;;; Pango text markup language.
;;; 
;;; This function will take care of setting "has-tooltip" to TRUE and of the
;;; default handler for the "query-tooltip" signal.
;;; 
;;; See also the "tooltip-markup" property and gtk_tooltip_set_markup().
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; markup :
;;; 	the contents of the tooltip for status_icon, or NULL.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_tooltip_markup ()
;;; 
;;; gchar * gtk_status_icon_get_tooltip_markup  (GtkStatusIcon *status_icon);
;;; 
;;; Gets the contents of the tooltip for status_icon.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the tooltip text, or NULL. You should free the returned string with
;;;     g_free() when done.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_has_tooltip ()
;;; 
;;; void gtk_status_icon_set_has_tooltip (GtkStatusIcon *status_icon,
;;;                                       gboolean has_tooltip);
;;; 
;;; Sets the has-tooltip property on status_icon to has_tooltip. See
;;; "has-tooltip" for more information.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; has_tooltip :
;;; 	whether or not status_icon has a tooltip
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_has_tooltip ()
;;; 
;;; gboolean gtk_status_icon_get_has_tooltip (GtkStatusIcon *status_icon);
;;; 
;;; Returns the current value of the has-tooltip property. See "has-tooltip"
;;; for more information.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	current value of has-tooltip on status_icon.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_title ()
;;; 
;;; void gtk_status_icon_set_title (GtkStatusIcon *status_icon,
;;;                                 const gchar *title);
;;; 
;;; Sets the title of this tray icon. This should be a short, human-readable,
;;; localized string describing the tray icon. It may be used by tools like
;;; screen readers to render the tray icon.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; title :
;;; 	the title
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_title ()
;;; 
;;; const gchar * gtk_status_icon_get_title (GtkStatusIcon *status_icon);
;;; 
;;; Gets the title of this tray icon. See gtk_status_icon_set_title().
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	the title of the status icon
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_name ()
;;; 
;;; void gtk_status_icon_set_name (GtkStatusIcon *status_icon,
;;;                                const gchar *name);
;;; 
;;; Sets the name of this tray icon. This should be a string identifying this
;;; icon. It is may be used for sorting the icons in the tray and will not be
;;; shown to the user.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; name :
;;; 	the name
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_visible ()
;;; 
;;; void gtk_status_icon_set_visible (GtkStatusIcon *status_icon,
;;;                                   gboolean visible);
;;; 
;;; Shows or hides a status icon.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; visible :
;;; 	TRUE to show the status icon, FALSE to hide it
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_visible ()
;;; 
;;; gboolean gtk_status_icon_get_visible (GtkStatusIcon *status_icon);
;;; 
;;; Returns whether the status icon is visible or not. Note that being visible
;;; does not guarantee that the user can actually see the icon, see
;;; also gtk_status_icon_is_embedded().
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	TRUE if the status icon is visible
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_is_embedded ()
;;; 
;;; gboolean gtk_status_icon_is_embedded (GtkStatusIcon *status_icon);
;;; 
;;; Returns whether the status icon is embedded in a notification area.
;;; 
;;; status_icon :
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	TRUE if the status icon is embedded in a notification area.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

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
;;; 	the GtkMenu
;;; 
;;; x :
;;; 	return location for the x position.
;;; 
;;; y :
;;; 	return location for the y position.
;;; 
;;; push_in :
;;; 	whether the first menu item should be offset (pushed in) to be aligned
;;;     with the menu popup position (only useful for GtkOptionMenu).
;;; 
;;; user_data :
;;; 	the status icon to position the menu on.
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
;;; 	a GtkStatusIcon
;;; 
;;; screen :
;;; 	return location for the screen, or NULL if the information is not
;;;     needed.
;;; 
;;; area :
;;; 	return location for the area occupied by the status icon, or NULL.
;;; 
;;; orientation :
;;; 	return location for the orientation of the panel in which the status
;;;     icon is embedded, or NULL. A panel at the top or bottom of the screen
;;;     is horizontal, a panel at the left or right is vertical.
;;; 
;;; Returns :
;;; 	TRUE if the location information has been filled in
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
;;; 	a GtkStatusIcon
;;; 
;;; Returns :
;;; 	An 32 bit unsigned integer identifier for the underlying X11 Window
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.status-icon.lisp ---------------------------------------
