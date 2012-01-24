;;; ----------------------------------------------------------------------------
;;; gtk.window.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
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
;;; GtkWindow
;;; 
;;; Toplevel which can contain other widgets
;;; 	
;;; Synopsis
;;; 
;;;     gtk-window
;;;
;;;     gtk-window-new
;;;     gtk-window-set-title
;;;
;;;     gtk_window_set_wmclass
;;;
;;;     gtk-window-set-resizable
;;;     gtk-window-get-resizable
;;;     gtk-window-add-accel-group
;;;     gtk-window-remove-accel-group
;;;     gtk-window-activate-focus
;;;     gtk-window-activate-default
;;;     gtk-window-set-modal
;;;     gtk-window-set-default-size
;;;
;;;     gtk_window_set_default_geometry
;;;     gtk_window_set_geometry_hints
;;;     gtk_window_set_gravity
;;;     gtk_window_get_gravity
;;;     gtk_window_set_position
;;;     gtk_window_set_transient_for
;;;     gtk_window_set_destroy_with_parent
;;;     gtk_window_set_screen
;;;     gtk_window_get_screen
;;;     gtk_window_is_active
;;;     gtk_window_has_toplevel_focus
;;;     gtk_window_list_toplevels
;;;     gtk_window_add_mnemonic
;;;     gtk_window_remove_mnemonic
;;;     gtk_window_mnemonic_activate
;;;     gtk_window_activate_key
;;;     gtk_window_propagate_key_event
;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_get_default_widget
;;;     gtk_window_set_default
;;;     gtk_window_present
;;;     gtk_window_present_with_time
;;;     gtk_window_iconify
;;;     gtk_window_deiconify
;;;     gtk_window_stick
;;;     gtk_window_unstick
;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_fullscreen
;;;     gtk_window_unfullscreen
;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below
;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag
;;;     gtk_window_set_decorated
;;;     gtk_window_set_deletable
;;;     gtk_window_set_mnemonic_modifier
;;;     gtk_window_set_type_hint
;;;     gtk_window_set_skip_taskbar_hint
;;;     gtk_window_set_skip_pager_hint
;;;     gtk_window_set_urgency_hint
;;;     gtk_window_set_accept_focus
;;;     gtk_window_set_focus_on_map
;;;     gtk_window_set_startup_id
;;;     gtk_window_set_role
;;;     gtk_window_get_decorated
;;;     gtk_window_get_deletable
;;;     gtk_window_get_default_icon_list
;;;     gtk_window_get_default_icon_name
;;;     gtk_window_get_default_size
;;;
;;;     gtk_window_get_destroy_with_parent
;;;     gtk_window_get_icon
;;;     gtk_window_get_icon_list
;;;     gtk_window_get_icon_name
;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_get_modal
;;;     gtk_window_get_position
;;;     gtk_window_get_role
;;;     gtk_window_get_size
;;;     gtk_window_get_title
;;;     gtk_window_get_transient_for
;;;     gtk_window_get_type_hint
;;;     gtk_window_get_skip_taskbar_hint
;;;     gtk_window_get_skip_pager_hint
;;;     gtk_window_get_urgency_hint
;;;     gtk_window_get_accept_focus
;;;     gtk_window_get_focus_on_map
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_get_window_type
;;;     gtk_window_move
;;;     gtk_window_parse_geometry
;;;     gtk_window_reshow_with_initial_size
;;;     gtk_window_resize
;;;     gtk_window_resize_to_geometry
;;;     gtk_window_set_default_icon_list
;;;     gtk_window_set_default_icon
;;;     gtk_window_set_default_icon_from_file
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_set_icon
;;;     gtk_window_set_icon_list
;;;     gtk_window_set_icon_from_file
;;;     gtk_window_set_icon_name
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity
;;;     gtk_window_get_mnemonics_visible
;;;     gtk_window_set_mnemonics_visible
;;;     gtk_window_get_focus_visible
;;;     gtk_window_set_focus_visible
;;;     gtk_window_set_has_resize_grip
;;;     gtk_window_get_has_resize_grip
;;;     gtk_window_resize_grip_is_visible
;;;     gtk_window_get_resize_grip_area
;;;     gtk_window_get_application
;;;     gtk_window_set_application
;;;     gtk_window_set_has_user_ref_count
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                  +----GtkAssistant
;;;                                  +----GtkOffscreenWindow
;;;                                  +----GtkPlug
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkWindow implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "accept-focus"          gboolean           : Read / Write
;;;   "application"           GtkApplication*    : Read / Write
;;;   "decorated"             gboolean           : Read / Write
;;;   "default-height"        gint               : Read / Write
;;;   "default-width"         gint               : Read / Write
;;;   "deletable"             gboolean           : Read / Write
;;;   "destroy-with-parent"   gboolean           : Read / Write
;;;   "focus-on-map"          gboolean           : Read / Write
;;;   "focus-visible"         gboolean           : Read / Write
;;;   "gravity"               GdkGravity         : Read / Write
;;;   "has-resize-grip"       gboolean           : Read / Write
;;;   "has-toplevel-focus"    gboolean           : Read
;;;   "icon"                  GdkPixbuf*         : Read / Write
;;;   "icon-name"             gchar*             : Read / Write
;;;   "is-active"             gboolean           : Read
;;;   "mnemonics-visible"     gboolean           : Read / Write
;;;   "modal"                 gboolean           : Read / Write
;;;   "opacity"               gdouble            : Read / Write
;;;   "resizable"             gboolean           : Read / Write
;;;   "resize-grip-visible"   gboolean           : Read
;;;   "role"                  gchar*             : Read / Write
;;;   "screen"                GdkScreen*         : Read / Write
;;;   "skip-pager-hint"       gboolean           : Read / Write
;;;   "skip-taskbar-hint"     gboolean           : Read / Write
;;;   "startup-id"            gchar*             : Write
;;;   "title"                 gchar*             : Read / Write
;;;   "transient-for"         GtkWindow*         : Read / Write / Construct
;;;   "type"                  GtkWindowType      : Read / Write / Construct Only
;;;   "type-hint"             GdkWindowTypeHint  : Read / Write
;;;   "urgency-hint"          gboolean           : Read / Write
;;;   "window-position"       GtkWindowPosition  : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "resize-grip-height"    gint               : Read / Write
;;;   "resize-grip-width"     gint               : Read / Write
;;; 
;;; Signals
;;; 
;;;   "activate-default"                         : Action
;;;   "activate-focus"                           : Action
;;;   "keys-changed"                             : Run First
;;;   "set-focus"                                : Run Last
;;; 
;;; Description
;;; 
;;; A GtkWindow is a toplevel window which can contain other widgets. Windows
;;; normally have decorations that are under the control of the windowing
;;; system and allow the user to manipulate the window (resize it, move it,
;;; close it, ...).
;;; 
;;; GTK+ also allows windows to have a resize grip (a small area in the lower
;;; right or left corner) which can be clicked to reszie the window. To control
;;; whether a window has a resize grip, use gtk_window_set_has_resize_grip().
;;; 
;;; GtkWindow as GtkBuildable
;;; 
;;; The GtkWindow implementation of the GtkBuildable interface supports a
;;; custom <accel-groups> element, which supports any number of <group>
;;; elements representing the GtkAccelGroup objects you want to add to your
;;; window (synonymous with gtk_window_add_accel_group().
;;; 
;;; Example 48. A UI definition fragment with accel groups
;;; 
;;;  1 <object class="GtkWindow">
;;;  2   <accel-groups>
;;;  3     <group name="accelgroup1"/>
;;;  4   </accel-groups>
;;;  5 </object>
;;;  6 <!-- -->
;;;  7 ...
;;;  8 <!-- -->
;;;  9 <object class="GtkAccelGroup" id="accelgroup1"/>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accept-focus" property
;;; 
;;;   "accept-focus" gboolean              : Read / Write
;;; 
;;; Whether the window should receive the input focus.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "application" property
;;; 
;;;   "application" GtkApplication*       : Read / Write
;;; 
;;; The GtkApplication associated with the window.
;;; 
;;; The application will be kept alive for at least as long as it has any
;;; windows associated with it (see g_application_hold() for a way to keep it
;;; alive without windows).
;;; 
;;; Normally, the connection between the application and the window will remain
;;; until the window is destroyed, but you can explicitly remove it by setting
;;; the ::application property to NULL.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "decorated" property
;;; 
;;;   "decorated" gboolean              : Read / Write
;;; 
;;; Whether the window should be decorated by the window manager.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "default-height" property
;;; 
;;;   "default-height"           gint                  : Read / Write
;;; 
;;; The default height of the window, used when initially showing the window.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "default-width" property
;;; 
;;;   "default-width"            gint                  : Read / Write
;;; 
;;; The default width of the window, used when initially showing the window.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "deletable" property
;;; 
;;;   "deletable"                gboolean              : Read / Write
;;; 
;;; Whether the window frame should have a close button.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "destroy-with-parent" property
;;; 
;;;   "destroy-with-parent"      gboolean              : Read / Write
;;; 
;;; If this window should be destroyed when the parent is destroyed.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-on-map" property
;;; 
;;;   "focus-on-map"             gboolean              : Read / Write
;;; 
;;; Whether the window should receive the input focus when mapped.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-visible" property
;;; 
;;;   "focus-visible"            gboolean              : Read / Write
;;; 
;;; Whether 'focus rectangles' are currently visible in this window.
;;; 
;;; This property is maintained by GTK+ based on the "gtk-visible-focus" setting
;;; and user input and should not be set by applications.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;; The "gravity" property
;;; 
;;;   "gravity"                  GdkGravity            : Read / Write
;;; 
;;; The window gravity of the window. See gtk_window_move() and GdkGravity for
;;; more details about window gravity.
;;; 
;;; Default value: GDK_GRAVITY_NORTH_WEST
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-resize-grip" property
;;; 
;;;   "has-resize-grip"          gboolean              : Read / Write
;;; 
;;; Whether the window has a corner resize grip.
;;; 
;;; Note that the resize grip is only shown if the window is actually resizable
;;; and not maximized. Use "resize-grip-visible" to find out if the resize grip
;;; is currently shown.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-toplevel-focus" property
;;; 
;;;   "has-toplevel-focus"       gboolean              : Read
;;; 
;;; Whether the input focus is within this GtkWindow.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon" property
;;; 
;;;   "icon"                     GdkPixbuf*            : Read / Write
;;; 
;;; Icon for this window.
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-name" property
;;; 
;;;   "icon-name"                gchar*                : Read / Write
;;; 
;;; The :icon-name property specifies the name of the themed icon to use as
;;; the window icon. See GtkIconTheme for more details.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-active" property
;;; 
;;;   "is-active"                gboolean              : Read
;;; 
;;; Whether the toplevel is the current active window.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "mnemonics-visible" property
;;; 
;;;   "mnemonics-visible"        gboolean              : Read / Write
;;; 
;;; Whether mnemonics are currently visible in this window.
;;; 
;;; This property is maintained by GTK+ based on the "gtk-auto-mnemonics"
;;; setting and user input, and should not be set by applications.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;; The "modal" property
;;; 
;;;   "modal"                    gboolean              : Read / Write
;;; 
;;; If TRUE, the window is modal (other windows are not usable while this one
;;; is up).
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "opacity" property
;;; 
;;;   "opacity"                  gdouble               : Read / Write
;;; 
;;; The requested opacity of the window. See gtk_window_set_opacity() for more
;;; details about window opacity.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 1
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "resizable" property
;;; 
;;;   "resizable"                gboolean              : Read / Write
;;; 
;;; If TRUE, users can resize the window.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "resize-grip-visible" property
;;; 
;;;   "resize-grip-visible"      gboolean              : Read
;;; 
;;; Whether a corner resize grip is currently shown.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "role" property
;;; 
;;;   "role"                     gchar*                : Read / Write
;;; 
;;; Unique identifier for the window to be used when restoring a session.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen" property
;;; 
;;;   "screen"                   GdkScreen*            : Read / Write
;;; 
;;; The screen where this window will be displayed.
;;; The "skip-pager-hint" property
;;; 
;;;   "skip-pager-hint"          gboolean              : Read / Write
;;; 
;;; TRUE if the window should not be in the pager.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "skip-taskbar-hint" property
;;; 
;;;   "skip-taskbar-hint"        gboolean              : Read / Write
;;; 
;;; TRUE if the window should not be in the task bar.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "startup-id" property
;;; 
;;;   "startup-id"               gchar*                : Write
;;; 
;;; The :startup-id is a write-only property for setting window's startup
;;; notification identifier. See gtk_window_set_startup_id() for more details.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" property
;;; 
;;;   "title"                    gchar*                : Read / Write
;;; 
;;; The title of the window.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "transient-for" property
;;; 
;;;   "transient-for"            GtkWindow*           : Read / Write / Construct
;;; 
;;; The transient parent of the window. See gtk_window_set_transient_for() for
;;; more details about transient windows.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "type" property
;;; 
;;;   "type"                     GtkWindowType   : Read / Write / Construct Only
;;; 
;;; The type of the window.
;;; 
;;; Default value: GTK_WINDOW_TOPLEVEL
;;;
;;; ----------------------------------------------------------------------------
;;; The "type-hint" property
;;; 
;;;   "type-hint"                GdkWindowTypeHint     : Read / Write
;;; 
;;; Hint to help the desktop environment understand what kind of window this is
;;; and how to treat it.
;;; 
;;; Default value: GDK_WINDOW_TYPE_HINT_NORMAL
;;;
;;; ----------------------------------------------------------------------------
;;; The "urgency-hint" property
;;; 
;;;   "urgency-hint" gboolean              : Read / Write
;;; 
;;; TRUE if the window should be brought to the user's attention.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-position" property
;;; 
;;;   "window-position" GtkWindowPosition     : Read / Write
;;; 
;;; The initial position of the window.
;;; 
;;; Default value: GTK_WIN_POS_NONE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "resize-grip-height" style property
;;; 
;;;   "resize-grip-height"       gint                  : Read / Write
;;; 
;;; Height of resize grip.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;;
;;; ----------------------------------------------------------------------------
;;; The "resize-grip-width" style property
;;; 
;;;   "resize-grip-width" gint                  : Read / Write
;;; 
;;; Width of resize grip.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-default" signal
;;; 
;;; void user_function (GtkWindow *window, gpointer user_data)      : Action
;;; 
;;; The ::activate-default signal is a keybinding signal which gets emitted
;;; when the user activates the default widget of window.
;;; 
;;; window :
;;; 	the window which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-focus" signal
;;; 
;;; void user_function (GtkWindow *window, gpointer user_data)      : Action
;;; 
;;; The ::activate-focus signal is a keybinding signal which gets emitted when
;;; the user activates the currently focused widget of window.
;;; 
;;; window :
;;; 	the window which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "keys-changed" signal
;;; 
;;; void user_function (GtkWindow *window, gpointer user_data)      : Run First
;;; 
;;; The ::keys-changed signal gets emitted when the set of accelerators or
;;; mnemonics that are associated with window changes.
;;; 
;;; window :
;;; 	the window which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "set-focus" signal
;;; 
;;; void user_function (GtkWindow *window,
;;;                     GtkWidget *widget,
;;;                     gpointer  user_data)      : Run Last
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindow
;;; 
;;; typedef struct _GtkWindow GtkWindow;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkWindow" 'gtk-window))

(define-g-object-class "GtkWindow" gtk-window
  (:superclass gtk-bin
    :export t
    :interfaces ("AtkImplementorIface" "GtkBuildable")
    :type-initializer "gtk_window_get_type")
  ((accept-focus        gtk-window-accept-focus "accept-focus" "gboolean" t t)
   (allow-grow          gtk-window-allow-grow "allow-grow" "gboolean" t t)
   (allow-shrink        gtk-window-allow-shrink "allow-shrink" "gboolean" t t)
   (decorated           gtk-window-decorated "decorated" "gboolean" t t)
   (default-height      gtk-window-default-height "default-height" "gint" t t)
   (default-width       gtk-window-default-width "default-width" "gint" t t)
   (deletable           gtk-window-deletable "deletable" "gboolean" t t)
   (destroy-with-parent gtk-window-destroy-with-parent
                        "destroy-with-parent" "gboolean" t t)
   (focus-on-map        gtk-window-focus-on-map "focus-on-map" "gboolean" t t)
   (gravity             gtk-window-gravity "gravity" "GdkGravity" t t)
   (has-toplevel-focus  gtk-window-has-toplevel-focus
                        "has-toplevel-focus" "gboolean" t nil)
   (icon                gtk-window-icon "icon" "GdkPixbuf" t t)
   (icon-name           gtk-window-icon-name "icon-name" "gchararray" t t)
   (is-active           gtk-window-is-active "is-active" "gboolean" t nil)
   (mnemonics-visible   gtk-window-mnemonics-visible
                        "mnemonis-visible" "gboolean" t t)
   (modal               gtk-window-modal "modal" "gboolean" t t)
   (opacity             gtk-window-opacity "opacity" "gdouble" t t)
   (resizable           gtk-window-resizable "resizable" "gboolean" t t)
   (role                gtk-window-role "role" "gchararray" t t)
   (screen              gtk-window-screen "screen" "GdkScreen" t t)
   (skip-pager-hint     gtk-window-skip-pager-hint
                        "skip-pager-hint" "gboolean" t t)
   (skip-taskbar-hint   gtk-window-skip-taskbar-hint
                        "skip-taskbar-hint" "gboolean" t t)
   (startup-id          gtk-window-startup-id "startup-id" "gchararray" nil t)
   (title               gtk-window-title "title" :string  t t)
   (transient-for       gtk-window-transient-for
                        "transient-for" "GtkWindow" t t)
   (type                gtk-window-type "type" "GtkWindowType" t nil)
   (type-hint           gtk-window-type-hint
                        "type-hint" "GdkWindowTypeHint" t t)
   (urgency-hint        gtk-window-urgency-hint "urgency-hint" "gboolean" t t)
   (window-position     gtk-window-window-position
                        "window-position" "GtkWindowPosition" t t)
   (:cffi focus
          gtk-window-focus (g-object gtk-widget)
          "gtk_window_get_focus" "gtk_window_set_focus")
   (:cffi default-widget
          gtk-window-default-widget (g-object gtk-widget)
          "gtk_window_get_default_widget" "gtk_window_set_default")
   (:cffi has-frame
          gtk-window-has-frame :boolean
          "gtk_window_get_has_frame" "gtk_window_set_has_frame")
   (:cffi mnemonic-modifier
          gtk-window-mnemonic-modifier (g-object gdk-modifier-type)
          "gtk_window_get_mnemonic_modifier" "gtk_window_set_mnemonic_modifier")
   (:cffi icon-list
          gtk-window-icon-list
          (g-list gdk-pixbuf :free-from-foreign t :free-to-foreign t)
          "gtk_window_get_icon_list" "gtk_window_set_icon_list")
   (:cffi group gtk-window-group
          (g-object gtk-window-group)
          "gtk_window_get_group" nil)
   (:cffi keep-above
          gtk-window-keep-above :boolean
          nil "gtk_window_set_keep_above")
   (:cffi keep-below
          gtk-window-keep-below :boolean
          nil "gtk_window_set_keep_below")))

;;; ----------------------------------------------------------------------------
;;; gtk_window_new ()
;;; 
;;; GtkWidget * gtk_window_new (GtkWindowType type)
;;; 
;;; Creates a new GtkWindow, which is a toplevel window that can contain other
;;; widgets. Nearly always, the type of the window should be
;;; GTK_WINDOW_TOPLEVEL. If you're implementing something like a popup menu
;;; from scratch (which is a bad idea, just use GtkMenu), you might use
;;; GTK_WINDOW_POPUP. GTK_WINDOW_POPUP is not for dialogs, though in some other
;;; toolkits dialogs are called "popups". In GTK+, GTK_WINDOW_POPUP means a
;;; pop-up menu or pop-up tooltip. On X11, popup windows are not controlled by
;;; the window manager.
;;; 
;;; If you simply want an undecorated window (no window borders), use
;;; gtk_window_set_decorated(), don't use GTK_WINDOW_POPUP.
;;; 
;;; type :
;;; 	type of window
;;; 
;;; Returns :
;;; 	a new GtkWindow.
;;; ----------------------------------------------------------------------------

(defun gtk-window-new (type)
  (make-instance 'gtk-window :type type))

(export 'gtk-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_title ()
;;; 
;;; void gtk_window_set_title (GtkWindow *window, const gchar *title)
;;; 
;;; Sets the title of the GtkWindow. The title of a window will be displayed in
;;; its title bar; on the X Window System, the title bar is rendered by the
;;; window manager, so exactly how the title appears to users may vary according
;;; to a user's exact configuration. The title should help a user distinguish
;;; this window from other windows they may have open. A good title might
;;; include the application name and current document filename, for example.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; title :
;;; 	title of the window
;;; ----------------------------------------------------------------------------

(defun gtk-window-set-title (window title)
  (setf (gtk-window-title window) title))

(export 'gtk-window-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_wmclass ()
;;; 
;;; void gtk_window_set_wmclass (GtkWindow *window,
;;;                              const gchar *wmclass_name,
;;;                              const gchar *wmclass_class)
;;; 
;;; Don't use this function. It sets the X Window System "class" and "name"
;;; hints for a window. According to the ICCCM, you should always set these to
;;; the same value for all windows in an application, and GTK+ sets them to
;;; that value by default, so calling this function is sort of pointless.
;;; However, you may want to call gtk_window_set_role() on each window in your
;;; application, for the benefit of the session manager. Setting the role allows
;;; the window manager to restore window positions when loading a saved session.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; wmclass_name :
;;; 	window name hint
;;; 
;;; wmclass_class :
;;; 	window class hint
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_resizable ()
;;; 
;;; void gtk_window_set_resizable (GtkWindow *window, gboolean resizable)
;;; 
;;; Sets whether the user can resize a window. Windows are user resizable by
;;; default.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; resizable :
;;; 	TRUE if the user can resize this window
;;; ----------------------------------------------------------------------------

(defun gtk-window-set-resizable (window resizable)
  (setf (gtk-window-resizable window) resizable))

(export 'gtk-window-set-resizable)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_resizable ()
;;; 
;;; gboolean gtk_window_get_resizable (GtkWindow *window)
;;; 
;;; Gets the value set by gtk_window_set_resizable().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the user can resize the window
;;; ----------------------------------------------------------------------------

(defun gtk-window-get-resizable (window)
  (gtk-window-resizable window))

(export 'gtk-window-get-resizable)

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_accel_group ()
;;; 
;;; void gtk_window_add_accel_group (GtkWindow *window,
;;;                                  GtkAccelGroup *accel_group)
;;; 
;;; Associate accel_group with window, such that calling
;;; gtk_accel_groups_activate() on window will activate accelerators in
;;; accel_group.
;;; 
;;; window :
;;; 	window to attach accelerator group to
;;; 
;;; accel_group :
;;; 	a GtkAccelGroup
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_add_accel_group" gtk-window-add-accel-group) :void
  (window (g-object gtk-window))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-window-add-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_accel_group ()
;;; 
;;; void gtk_window_remove_accel_group (GtkWindow *window,
;;;                                     GtkAccelGroup *accel_group)
;;; 
;;; Reverses the effects of gtk_window_add_accel_group().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; accel-group :
;;; 	a GtkAccelGroup
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_remove_accel_group" gtk-window-remove-accel-group) :void
  (window (g-object gtk-window))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-window-remove-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_focus ()
;;; 
;;; gboolean gtk_window_activate_focus (GtkWindow *window)
;;; 
;;; Activates the current focused widget within the window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if a widget got activated.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_focus" gtk-window-activate-focus) :boolean
  (window (g-object gtk-window)))

(export 'gtk-window-activate-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_default ()
;;; 
;;; gboolean gtk_window_activate_default (GtkWindow *window)
;;; 
;;; Activates the default widget for the window, unless the current focused
;;; widget has been configured to receive the default action (see
;;; gtk_widget_set_receives_default()), in which case the focused widget is
;;; activated.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if a widget got activated.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_default" gtk-window-activate-default) :boolean
  (window (g-object gtk-window)))

(export 'gtk-window-activate-default)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_modal ()
;;; 
;;; void gtk_window_set_modal (GtkWindow *window, gboolean modal)
;;; 
;;; Sets a window modal or non-modal. Modal windows prevent interaction with
;;; other windows in the same application. To keep modal dialogs on top of main
;;; application windows, use gtk_window_set_transient_for() to make the dialog
;;; transient for the parent; most window managers will then disallow lowering
;;; the dialog below the parent.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; modal :
;;; 	whether the window is modal
;;; ----------------------------------------------------------------------------

(defun gtk-window-set-modal (window modal)
  (setf (gtk-window-modal window) modal))

(export 'gtk-window-set-modal)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_size ()
;;; 
;;; void gtk_window_set_default_size (GtkWindow *window,
;;;                                   gint width, gint height)
;;; 
;;; Sets the default size of a window. If the window's "natural" size (its size
;;; request) is larger than the default, the default will be ignored. More
;;; generally, if the default size does not obey the geometry hints for the
;;; window (gtk_window_set_geometry_hints() can be used to set these
;;; explicitly), the default size will be clamped to the nearest permitted size.
;;; 
;;; Unlike gtk_widget_set_size_request(), which sets a size request for a widget
;;; and thus would keep users from shrinking the window, this function only sets
;;; the initial size, just as if the user had resized the window themselves.
;;; Users can still shrink the window again as they normally would. Setting a
;;; default size of -1 means to use the "natural" default size (the size request
;;; of the window).
;;; 
;;; For more control over a window's initial size and how resizing works,
;;; investigate gtk_window_set_geometry_hints().
;;; 
;;; For some uses, gtk_window_resize() is a more appropriate function.
;;; gtk_window_resize() changes the current size of the window, rather than the
;;; size to be used on initial display. gtk_window_resize() always affects the
;;; window itself, not the geometry widget.
;;; 
;;; The default size of a window only affects the first time a window is shown;
;;; if a window is hidden and re-shown, it will remember the size it had prior
;;; to hiding, rather than using the default size.
;;; 
;;; Windows can't actually be 0x0 in size, they must be at least 1x1, but
;;; passing 0 for width and height is OK, resulting in a 1x1 default size.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; width :
;;; 	width in pixels, or -1 to unset the default width
;;; 
;;; height :
;;; 	height in pixels, or -1 to unset the default height
;;; ----------------------------------------------------------------------------

(defun gtk-window-set-default-size (window width height)
  (setf (gtk-window-default-width window) width
        (gtk-window-default-height window) height))

(export 'gtk-window-set-default-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_geometry ()
;;; 
;;; void gtk_window_set_default_geometry (GtkWindow *window,
;;;                                       gint width, gint height)
;;; 
;;; Like gtk_window_set_default_size(), but width and height are interpreted in
;;; terms of the base size and increment set with gtk_window_set_geometry_hints.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; width :
;;; 	width in resize increments, or -1 to unset the default width
;;; 
;;; height :
;;; 	height in resize increments, or -1 to unset the default height
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_geometry_hints ()
;;; 
;;; void gtk_window_set_geometry_hints (GtkWindow *window,
;;;                                     GtkWidget *geometry_widget,
;;;                                     GdkGeometry *geometry,
;;;                                     GdkWindowHints geom_mask)
;;; 
;;; This function sets up hints about how a window can be resized by the user.
;;; You can set a minimum and maximum size; allowed resize increments (e.g. for
;;; xterm, you can only resize by the size of a character); aspect ratios; and
;;; more. See the GdkGeometry struct.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; geometry_widget :
;;; 	widget the geometry hints will be applied to or NULL.
;;; 
;;; geometry :
;;; 	struct containing geometry information or NULL.
;;; 
;;; geom_mask :
;;; 	mask indicating which struct fields should be paid attention to
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_geometry_hints" gtk-window-set-geometry-hints) :void
  (window (g-object gtk-window))
  (geometry-widget (g-object gtk-widget))
  (geometry (g-boxed-foreign geometry))
  (geometry-mask gdk-window-hints))

(export 'gtk-window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_gravity ()
;;; 
;;; void gtk_window_set_gravity (GtkWindow *window, GdkGravity gravity)
;;; 
;;; Window gravity defines the meaning of coordinates passed to
;;; gtk_window_move(). See gtk_window_move() and GdkGravity for more details.
;;; 
;;; The default window gravity is GDK_GRAVITY_NORTH_WEST which will typically
;;; "do what you mean."
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; gravity :
;;; 	window gravity
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_gravity ()
;;; 
;;; GdkGravity gtk_window_get_gravity (GtkWindow *window)
;;; 
;;; Gets the value set by gtk_window_set_gravity().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	window gravity.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_position ()
;;; 
;;; void gtk_window_set_position (GtkWindow *window, GtkWindowPosition position)
;;; 
;;; Sets a position constraint for this window. If the old or new constraint is
;;; GTK_WIN_POS_CENTER_ALWAYS, this will also cause the window to be
;;; repositioned to satisfy the new constraint.
;;; 
;;; window :
;;; 	a GtkWindow.
;;; 
;;; position :
;;; 	a position constraint.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_transient_for ()
;;; 
;;; void gtk_window_set_transient_for (GtkWindow *window, GtkWindow *parent);
;;; 
;;; Dialog windows should be set transient for the main application window they
;;; were spawned from. This allows window managers to e.g. keep the dialog on
;;; top of the main window, or center the dialog over the main window.
;;; gtk_dialog_new_with_buttons() and other convenience functions in GTK+ will
;;; sometimes call gtk_window_set_transient_for() on your behalf.
;;; 
;;; Passing NULL for parent unsets the current transient window.
;;; 
;;; On Windows, this function puts the child window on top of the parent, much
;;; as the window manager would have done on X.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; parent :
;;; 	parent window, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_destroy_with_parent ()
;;; 
;;; void gtk_window_set_destroy_with_parent (GtkWindow *window,
;;;                                          gboolean setting);
;;; 
;;; If setting is TRUE, then destroying the transient parent of window will
;;; also destroy window itself. This is useful for dialogs that shouldn't
;;; persist beyond the lifetime of the main window they're associated with,
;;; for example.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	whether to destroy window with its transient parent
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_screen ()
;;; 
;;; void gtk_window_set_screen (GtkWindow *window, GdkScreen *screen);
;;; 
;;; Sets the GdkScreen where the window is displayed; if the window is already
;;; mapped, it will be unmapped, and then remapped on the new screen.
;;; 
;;; window :
;;; 	a GtkWindow.
;;; 
;;; screen :
;;; 	a GdkScreen.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_screen ()
;;; 
;;; GdkScreen * gtk_window_get_screen (GtkWindow *window);
;;; 
;;; Returns the GdkScreen associated with window.
;;; 
;;; window :
;;; 	a GtkWindow.
;;; 
;;; Returns :
;;; 	a GdkScreen.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_is_active ()
;;; 
;;; gboolean gtk_window_is_active (GtkWindow *window);
;;; 
;;; Returns whether the window is part of the current active toplevel.
;;; (That is, the toplevel window receiving keystrokes.) The return value is
;;; TRUE if the window is active toplevel itself, but also if it is, say, a
;;; GtkPlug embedded in the active toplevel. You might use this function if you
;;; wanted to draw a widget differently in an active window from a widget in an
;;; inactive window. See gtk_window_has_toplevel_focus()
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the window part of the current active window.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_has_toplevel_focus ()
;;; 
;;; gboolean gtk_window_has_toplevel_focus (GtkWindow *window);
;;; 
;;; Returns whether the input focus is within this GtkWindow. For real toplevel
;;; windows, this is identical to gtk_window_is_active(), but for embedded
;;; windows, like GtkPlug, the results will differ.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the input focus is within this GtkWindow
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_list_toplevels ()
;;; 
;;; GList * gtk_window_list_toplevels (void);
;;; 
;;; Returns a list of all existing toplevel windows. The widgets in the list
;;; are not individually referenced. If you want to iterate through the list
;;; and perform actions involving callbacks that might destroy the widgets, you
;;; must call g_list_foreach (result, (GFunc)g_object_ref, NULL) first, and
;;; then unref all the widgets afterwards.
;;; 
;;; Returns :
;;; 	list of toplevel widgets.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_list_toplevels" gtk-window-list-toplevels)
    (g-list (g-object gtk-window) :free-from-foreign t))

(export 'gtk-window-list-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_mnemonic ()
;;; 
;;; void gtk_window_add_mnemonic (GtkWindow *window,
;;;                               guint keyval,
;;;                               GtkWidget *target);
;;; 
;;; Adds a mnemonic to this window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; keyval :
;;; 	the mnemonic
;;; 
;;; target :
;;; 	the widget that gets activated by the mnemonic
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_add_mnemonic" gtk-window-add-mnemonic) :void
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object gtk-widget)))

(export 'gtk-window-add-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_mnemonic ()
;;; 
;;; void gtk_window_remove_mnemonic (GtkWindow *window,
;;;                                  guint keyval,
;;;                                  GtkWidget *target);
;;; 
;;; Removes a mnemonic from this window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; keyval :
;;; 	the mnemonic
;;; 
;;; target :
;;; 	the widget that gets activated by the mnemonic
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_remove_mnemonic" gtk-window-remove-mnemonic) :void
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object gtk-widget)))

(export 'gtk-window-remove-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_mnemonic_activate ()
;;; 
;;; gboolean gtk_window_mnemonic_activate (GtkWindow *window,
;;;                                        guint keyval,
;;;                                        GdkModifierType modifier)
;;; 
;;; Activates the targets associated with the mnemonic.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; keyval :
;;; 	the mnemonic
;;; 
;;; modifier :
;;; 	the modifiers
;;; 
;;; Returns :
;;; 	TRUE if the activation is done.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_mnemonic_activate" gtk-window-mnemonic-activate) :boolean
  (window (g-object gtk-window))
  (keyval :uint)
  (modifier gdk-modifier-type))

(export 'gtk-window-mnemonic-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_key ()
;;; 
;;; gboolean gtk_window_activate_key (GtkWindow *window, GdkEventKey *event);
;;; 
;;; Activates mnemonics and accelerators for this GtkWindow. This is normally
;;; called by the default ::key_press_event handler for toplevel windows,
;;; however in some cases it may be useful to call this directly when overriding
;;; the standard key handling for a toplevel window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; event :
;;; 	a GdkEventKey
;;; 
;;; Returns :
;;; 	TRUE if a mnemonic or accelerator was found and activated.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_key" gtk-window-activate-key) :boolean
  (window (g-object gtk-window))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-window-activate-key)

;;; ----------------------------------------------------------------------------
;;; gtk_window_propagate_key_event ()
;;; 
;;; gboolean gtk_window_propagate_key_event (GtkWindow *window,
;;;                                          GdkEventKey *event);
;;; 
;;; Propagate a key press or release event to the focus widget and up the focus
;;; container chain until a widget handles event. This is normally called by
;;; the default ::key_press_event and ::key_release_event handlers for toplevel
;;; windows, however in some cases it may be useful to call this directly when
;;; overriding the standard key handling for a toplevel window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; event :
;;; 	a GdkEventKey
;;; 
;;; Returns :
;;; 	TRUE if a widget in the focus chain handled the event.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_propagate_key_event" gtk-window-propagate-key-event)
    :boolean
  (window (g-object gtk-window))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-window-propagate-key-event)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus ()
;;; 
;;; GtkWidget * gtk_window_get_focus (GtkWindow *window);
;;; 
;;; Retrieves the current focused widget within the window. Note that this is
;;; the widget that would have the focus if the toplevel window focused; if the
;;; toplevel window is not focused then gtk_widget_has_focus (widget) will not
;;; be TRUE for the widget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the currently focused widget, or NULL if there is none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_focus ()
;;; 
;;; void gtk_window_set_focus (GtkWindow *window, GtkWidget *focus);
;;; 
;;; If focus is not the current focus widget, and is focusable, sets it as the
;;; focus widget for the window. If focus is NULL, unsets the focus widget for
;;; this window. To set the focus to a particular widget in the toplevel, it is
;;; usually more convenient to use gtk_widget_grab_focus() instead of this
;;; function.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; focus :
;;; 	widget to be the new focus widget, or NULL to unset any focus widget
;;;     for the toplevel window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_widget ()
;;; 
;;; GtkWidget * gtk_window_get_default_widget (GtkWindow *window);
;;; 
;;; Returns the default widget for window. See gtk_window_set_default() for
;;; more details.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the default widget, or NULL if there is none.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default ()
;;; 
;;; void gtk_window_set_default (GtkWindow *window, GtkWidget *default_widget);
;;; 
;;; The default widget is the widget that's activated when the user presses
;;; Enter in a dialog (for example). This function sets or unsets the default
;;; widget for a GtkWindow about. When setting (rather than unsetting) the
;;; default widget it's generally easier to call gtk_widget_grab_focus() on the
;;; widget. Before making a widget the default widget, you must set the
;;; GTK_CAN_DEFAULT flag on the widget you'd like to make the default using
;;; GTK_WIDGET_SET_FLAGS().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; default_widget :
;;; 	widget to be the default, or NULL to unset the default widget for the
;;;     toplevel.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_present ()
;;; 
;;; void gtk_window_present (GtkWindow *window);
;;; 
;;; Presents a window to the user. This may mean raising the window in the
;;; stacking order, deiconifying it, moving it to the current desktop, and/or
;;; giving it the keyboard focus, possibly dependent on the user's platform,
;;; window manager, and preferences.
;;; 
;;; If window is hidden, this function calls gtk_widget_show() as well.
;;; 
;;; This function should be used when the user tries to open a window that's
;;; already open. Say for example the preferences dialog is currently open, and
;;; the user chooses Preferences from the menu a second time; use
;;; gtk_window_present() to move the already-open dialog where the user can see
;;; it.
;;; 
;;; If you are calling this function in response to a user interaction, it is
;;; preferable to use gtk_window_present_with_time().
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_present" gtk-window-present) :void
  (window (g-object gtk-window)))

(export 'gtk-window-present)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present_with_time ()
;;; 
;;; void gtk_window_present_with_time (GtkWindow *window, guint32 timestamp);
;;; 
;;; Presents a window to the user in response to a user interaction. If you
;;; need to present a window without a timestamp, use gtk_window_present().
;;; See gtk_window_present() for details.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; timestamp :
;;; 	the timestamp of the user interaction (typically a button or key press
;;;     event) which triggered this call
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_present_with_time" gtk-window-present-with-time) :void
  (window (g-object gtk-window))
  (timestamp :uint32))

(export 'gtk-window-present-with-time)

;;; ----------------------------------------------------------------------------
;;; gtk_window_iconify ()
;;; 
;;; void gtk_window_iconify (GtkWindow *window);
;;; 
;;; Asks to iconify (i.e. minimize) the specified window. Note that you
;;; shouldn't assume the window is definitely iconified afterward, because
;;; other entities (e.g. the user or window manager) could deiconify it again,
;;; or there may not be a window manager in which case iconification isn't
;;; possible, etc. But normally the window will end up iconified. Just don't
;;; write code that crashes if not.
;;; 
;;; It's permitted to call this function before showing a window, in which case
;;; the window will be iconified before it ever appears onscreen.
;;; 
;;; You can track iconification via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_iconify" gtk-window-iconify) :void
  (window (g-object gtk-window)))

(export 'gtk-window-iconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_deiconify ()
;;; 
;;; void gtk_window_deiconify (GtkWindow *window);
;;; 
;;; Asks to deiconify (i.e. unminimize) the specified window. Note that you
;;; shouldn't assume the window is definitely deiconified afterward, because
;;; other entities (e.g. the user or window manager) could iconify it again
;;; before your code which assumes deiconification gets to run.
;;; 
;;; You can track iconification via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_deiconify" gtk-window-deiconify) :void
  (window (g-object gtk-window)))

(export 'gtk-window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_stick ()
;;; 
;;; void gtk_window_stick (GtkWindow *window);
;;; 
;;; Asks to stick window, which means that it will appear on all user desktops.
;;; Note that you shouldn't assume the window is definitely stuck afterward,
;;; because other entities (e.g. the user or window manager) could unstick it
;;; again, and some window managers do not support sticking windows. But
;;; normally the window will end up stuck. Just don't write code that crashes
;;; if not.
;;; 
;;; It's permitted to call this function before showing a window.
;;; 
;;; You can track stickiness via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_stick" gtk-window-stick) :void
  (window (g-object gtk-window)))

(export 'gtk-window-stick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unstick ()
;;; 
;;; void gtk_window_unstick (GtkWindow *window);
;;; 
;;; Asks to unstick window, which means that it will appear on only one of the
;;; user's desktops. Note that you shouldn't assume the window is definitely
;;; unstuck afterward, because other entities (e.g. the user or window manager)
;;; could stick it again. But normally the window will end up stuck. Just don't
;;; write code that crashes if not.
;;; 
;;; You can track stickiness via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unstick" gtk-window-unstick) :void
  (window (g-object gtk-window)))

(export 'gtk-window-unstick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_maximize ()
;;; 
;;; void gtk_window_maximize (GtkWindow *window);
;;; 
;;; Asks to maximize window, so that it becomes full-screen. Note that you
;;; shouldn't assume the window is definitely maximized afterward, because other
;;; entities (e.g. the user or window manager) could unmaximize it again, and
;;; not all window managers support maximization. But normally the window will
;;; end up maximized. Just don't write code that crashes if not.
;;; 
;;; It's permitted to call this function before showing a window, in which case
;;; the window will be maximized when it appears onscreen initially.
;;; 
;;; You can track maximization via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_maximize" gtk-window-maximize) :void
  (window (g-object gtk-window)))

(export 'gtk-window-maximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unmaximize ()
;;; 
;;; void gtk_window_unmaximize (GtkWindow *window);
;;; 
;;; Asks to unmaximize window. Note that you shouldn't assume the window is
;;; definitely unmaximized afterward, because other entities (e.g. the user or
;;; window manager) could maximize it again, and not all window managers honor
;;; requests to unmaximize. But normally the window will end up unmaximized.
;;; Just don't write code that crashes if not.
;;; 
;;; You can track maximization via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unmaximize" gtk-window-unmaximize) :void
  (window (g-object gtk-window)))

(export 'gtk-window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen ()
;;; 
;;; void gtk_window_fullscreen (GtkWindow *window);
;;; 
;;; Asks to place window in the fullscreen state. Note that you shouldn't
;;; assume the window is definitely full screen afterward, because other
;;; entities (e.g. the user or window manager) could unfullscreen it again, and
;;; not all window managers honor requests to fullscreen windows. But normally
;;; the window will end up fullscreen. Just don't write code that crashes if
;;; not.
;;; 
;;; You can track the fullscreen state via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_fullscreen" gtk-window-fullscreen) :void
  (window (g-object gtk-window)))

(export 'gtk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unfullscreen ()
;;; 
;;; void gtk_window_unfullscreen (GtkWindow *window);
;;; 
;;; Asks to toggle off the fullscreen state for window. Note that you shouldn't
;;; assume the window is definitely not full screen afterward, because other
;;; entities (e.g. the user or window manager) could fullscreen it again, and
;;; not all window managers honor requests to unfullscreen windows. But normally
;;; the window will end up restored to its normal state. Just don't write code
;;; that crashes if not.
;;; 
;;; You can track the fullscreen state via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unfullscreen" gtk-window-unfullscreen) :void
  (window (g-object gtk-window)))

(export 'gtk-window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_keep_above ()
;;; 
;;; void gtk_window_set_keep_above (GtkWindow *window, gboolean setting);
;;; 
;;; Asks to keep window above, so that it stays on top. Note that you shouldn't
;;; assume the window is definitely above afterward, because other entities
;;; (e.g. the user or window manager) could not keep it above, and not all
;;; window managers support keeping windows above. But normally the window will
;;; end kept above. Just don't write code that crashes if not.
;;; 
;;; It's permitted to call this function before showing a window, in which case
;;; the window will be kept above when it appears onscreen initially.
;;; 
;;; You can track the above state via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; Note that, according to the Extended Window Manager Hints specification,
;;; the above state is mainly meant for user preferences and should not be used
;;; by applications e.g. for drawing attention to their dialogs.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	whether to keep window above other windows
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_keep_below ()
;;; 
;;; void gtk_window_set_keep_below (GtkWindow *window, gboolean setting);
;;; 
;;; Asks to keep window below, so that it stays in bottom. Note that you
;;; shouldn't assume the window is definitely below afterward, because other
;;; entities (e.g. the user or window manager) could not keep it below, and not
;;; all window managers support putting windows below. But normally the window
;;; will be kept below. Just don't write code that crashes if not.
;;; 
;;; It's permitted to call this function before showing a window, in which case
;;; the window will be kept below when it appears onscreen initially.
;;; 
;;; You can track the below state via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; Note that, according to the Extended Window Manager Hints specification,
;;; the above state is mainly meant for user preferences and should not be used
;;; by applications e.g. for drawing attention to their dialogs.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	whether to keep window below other windows
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_resize_drag ()
;;; 
;;; void gtk_window_begin_resize_drag (GtkWindow *window,
;;;                                    GdkWindowEdge edge,
;;;                                    gint button,
;;;                                    gint root_x,
;;;                                    gint root_y,
;;;                                    guint32 timestamp);
;;; 
;;; Starts resizing a window. This function is used if an application has
;;; window resizing controls. When GDK can support it, the resize will be done
;;; using the standard mechanism for the window manager or windowing system.
;;; Otherwise, GDK will try to emulate window resizing, potentially not all
;;; that well, depending on the windowing system.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; button :
;;; 	mouse button that initiated the drag
;;; 
;;; edge :
;;; 	position of the resize control
;;; 
;;; root_x :
;;; 	X position where the user clicked to initiate the drag, in root window
;;;     coordinates
;;; 
;;; root_y :
;;; 	Y position where the user clicked to initiate the drag
;;; 
;;; timestamp :
;;; 	timestamp from the click event that initiated the drag
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_begin_resize_drag" gtk-window-begin-resize-drag) :void
  (window (g-object gtk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_move_drag ()
;;; 
;;; void gtk_window_begin_move_drag (GtkWindow *window,
;;;                                  gint button,
;;;                                  gint root_x,
;;;                                  gint root_y,
;;;                                  guint32 timestamp);
;;; 
;;; Starts moving a window. This function is used if an application has window
;;; movement grips. When GDK can support it, the window movement will be done
;;; using the standard mechanism for the window manager or windowing system.
;;; Otherwise, GDK will try to emulate window movement, potentially not all
;;; that well, depending on the windowing system.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; button :
;;; 	mouse button that initiated the drag
;;; 
;;; root_x :
;;; 	X position where the user clicked to initiate the drag, in root window
;;;     coordinates
;;; 
;;; root_y :
;;; 	Y position where the user clicked to initiate the drag
;;; 
;;; timestamp :
;;; 	timestamp from the click event that initiated the drag
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_begin_move_drag" gtk-window-begin-move-drag) :void
  (window (g-object gtk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_decorated ()
;;; 
;;; void gtk_window_set_decorated (GtkWindow *window, gboolean setting);
;;; 
;;; By default, windows are decorated with a title bar, resize controls, etc.
;;; Some window managers allow GTK+ to disable these decorations, creating a
;;; borderless window. If you set the decorated property to FALSE using this
;;; function, GTK+ will do its best to convince the window manager not to
;;; decorate the window. Depending on the system, this function may not have
;;; any effect when called on a window that is already visible, so you should
;;; call it before calling gtk_widget_show().
;;; 
;;; On Windows, this function always works, since there's no window manager
;;; policy involved.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to decorate the window
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_deletable ()
;;; 
;;; void gtk_window_set_deletable (GtkWindow *window, gboolean setting);
;;; 
;;; By default, windows have a close button in the window frame. Some window
;;; managers allow GTK+ to disable this button. If you set the deletable
;;; property to FALSE using this function, GTK+ will do its best to convince
;;; the window manager not to show a close button. Depending on the system,
;;; this function may not have any effect when called on a window that is
;;; already visible, so you should call it before calling gtk_window_show().
;;; 
;;; On Windows, this function always works, since there's no window manager
;;; policy involved.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to decorate the window as deletable
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_mnemonic_modifier ()
;;; 
;;; void gtk_window_set_mnemonic_modifier (GtkWindow *window,
;;;                                        GdkModifierType modifier);
;;; 
;;; Sets the mnemonic modifier for this window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; modifier :
;;; 	the modifier mask used to activate mnemonics on this window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_type_hint ()
;;; 
;;; void gtk_window_set_type_hint (GtkWindow *window, GdkWindowTypeHint hint);
;;; 
;;; By setting the type hint for the window, you allow the window manager to
;;; decorate and handle the window in a way which is suitable to the function
;;; of the window in your application.
;;; 
;;; This function should be called before the window becomes visible.
;;; 
;;; gtk_dialog_new_with_buttons() and other convenience functions in GTK+ will
;;; sometimes call gtk_window_set_type_hint() on your behalf.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; hint :
;;; 	the window type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_skip_taskbar_hint ()
;;; 
;;; void gtk_window_set_skip_taskbar_hint (GtkWindow *window, gboolean setting);
;;; 
;;; Windows may set a hint asking the desktop environment not to display the
;;; window in the task bar. This function sets this hint.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to keep this window from appearing in the task bar
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_skip_pager_hint ()
;;; 
;;; void gtk_window_set_skip_pager_hint (GtkWindow *window, gboolean setting);
;;; 
;;; Windows may set a hint asking the desktop environment not to display the
;;; window in the pager. This function sets this hint. (A "pager" is any desktop
;;; navigation tool such as a workspace switcher that displays a thumbnail
;;; representation of the windows on the screen.)
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to keep this window from appearing in the pager
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_urgency_hint ()
;;; 
;;; void gtk_window_set_urgency_hint (GtkWindow *window, gboolean setting);
;;; 
;;; Windows may set a hint asking the desktop environment to draw the users
;;; attention to the window. This function sets this hint.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to mark this window as urgent
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_accept_focus ()
;;; 
;;; void gtk_window_set_accept_focus (GtkWindow *window, gboolean setting);
;;; 
;;; Windows may set a hint asking the desktop environment not to receive the
;;; input focus. This function sets this hint.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to let this window receive input focus
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_focus_on_map ()
;;; 
;;; void gtk_window_set_focus_on_map (GtkWindow *window, gboolean setting);
;;; 
;;; Windows may set a hint asking the desktop environment not to receive the
;;; input focus when the window is mapped. This function sets this hint.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	TRUE to let this window receive input focus on map
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_startup_id ()
;;; 
;;; void gtk_window_set_startup_id (GtkWindow *window, const gchar *startup_id)
;;; 
;;; Startup notification identifiers are used by desktop environment to track
;;; application startup, to provide user feedback and other features. This
;;; function changes the corresponding property on the underlying GdkWindow.
;;; Normally, startup identifier is managed automatically and you should only
;;; use this function in special cases like transferring focus from other
;;; processes. You should use this function before calling gtk_window_present()
;;; or any equivalent function generating a window map event.
;;; 
;;; This function is only useful on X11, not with other GTK+ targets.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; startup_id :
;;; 	a string with startup-notification identifier
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_role ()
;;; 
;;; void gtk_window_set_role (GtkWindow *window, const gchar *role);
;;; 
;;; This function is only useful on X11, not with other GTK+ targets.
;;; 
;;; In combination with the window title, the window role allows a window
;;; manager to identify "the same" window when an application is restarted. So
;;; for example you might set the "toolbox" role on your app's toolbox window,
;;; so that when the user restarts their session, the window manager can put
;;; the toolbox back in the same place.
;;; 
;;; If a window already has a unique title, you don't need to set the role,
;;; since the WM can use the title to identify the window when restoring the
;;; session.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; role :
;;; 	unique identifier for the window to be used when restoring a session
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_decorated ()
;;; 
;;; gboolean gtk_window_get_decorated (GtkWindow *window)
;;; 
;;; Returns whether the window has been set to have decorations such as a title
;;; bar via gtk_window_set_decorated().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the window has been set to have decorations
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_deletable ()
;;; 
;;; gboolean gtk_window_get_deletable (GtkWindow *window)
;;; 
;;; Returns whether the window has been set to have a close button via
;;; gtk_window_set_deletable().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the window has been set to have a close button
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_list ()
;;; 
;;; GList * gtk_window_get_default_icon_list (void)
;;; 
;;; Gets the value set by gtk_window_set_default_icon_list(). The list is a
;;; copy and should be freed with g_list_free(), but the pixbufs in the list
;;; have not had their reference count incremented.
;;; 
;;; Returns :
;;; 	copy of default icon list.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_list" gtk-window-get-default-icon-list)
  (g-list (g-object gdk-pixbuf)))

(export 'gtk-window-get-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_name ()
;;; 
;;; const gchar * gtk_window_get_default_icon_name (void)
;;; 
;;; Returns the fallback icon name for windows that has been set with
;;; gtk_window_set_default_icon_name(). The returned string is owned by GTK+
;;; and should not be modified. It is only valid until the next call to
;;; gtk_window_set_default_icon_name().
;;; 
;;; Returns :
;;; 	the fallback icon name for windows
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_name" gtk-window-get-default-icon-name)
  (:string :free-from-foreign nil))

(export 'gtk-window-get-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_size ()
;;; 
;;; void gtk_window_get_default_size (GtkWindow *window,
;;;                                   gint *width, gint *height)
;;; 
;;; Gets the default size of the window. A value of -1 for the width or height
;;; indicates that a default size has not been explicitly set for that
;;; dimension, so the "natural" size of the window will be used.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; width :
;;; 	location to store the default width, or NULL. [out][allow-none]
;;; 
;;; height :
;;; 	location to store the default height, or NULL. [out][allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_destroy_with_parent ()
;;; 
;;; gboolean gtk_window_get_destroy_with_parent (GtkWindow *window);
;;; 
;;; Returns whether the window will be destroyed with its transient parent.
;;; See gtk_window_set_destroy_with_parent().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the window will be destroyed with its transient parent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon ()
;;; 
;;; GdkPixbuf * gtk_window_get_icon (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_icon() (or if you've called
;;; gtk_window_set_icon_list(), gets the first icon in the icon list).
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	icon for window. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon_list ()
;;; 
;;; GList * gtk_window_get_icon_list (GtkWindow *window);
;;; 
;;; Retrieves the list of icons set by gtk_window_set_icon_list(). The list is
;;; copied, but the reference count on each member won't be incremented.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	copy of window's icon list.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon_name ()
;;; 
;;; const gchar * gtk_window_get_icon_name (GtkWindow *window);
;;; 
;;; Returns the name of the themed icon for the window,
;;; see gtk_window_set_icon_name().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the icon name or NULL if the window has no themed icon
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_mnemonic_modifier ()
;;; 
;;; GdkModifierType gtk_window_get_mnemonic_modifier (GtkWindow *window);
;;; 
;;; Returns the mnemonic modifier for this window.
;;; See gtk_window_set_mnemonic_modifier().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the modifier mask used to activate mnemonics on this window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_modal ()
;;; 
;;; gboolean gtk_window_get_modal (GtkWindow *window);
;;; 
;;; Returns whether the window is modal. See gtk_window_set_modal().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the window is set to be modal and establishes a grab when shown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_position ()
;;; 
;;; void gtk_window_get_position (GtkWindow *window,
;;;                               gint *root_x,
;;;                               gint *root_y);
;;; 
;;; This function returns the position you need to pass to gtk_window_move()
;;; to keep window in its current position. This means that the meaning of the
;;; returned value varies with window gravity. See gtk_window_move() for more
;;; details.
;;; 
;;; If you haven't changed the window gravity, its gravity will be
;;; GDK_GRAVITY_NORTH_WEST. This means that gtk_window_get_position() gets the
;;; position of the top-left corner of the window manager frame for the window.
;;; gtk_window_move() sets the position of this same top-left corner.
;;; 
;;; gtk_window_get_position() is not 100% reliable because the X Window System
;;; does not specify a way to obtain the geometry of the decorations placed on
;;; a window by the window manager. Thus GTK+ is using a "best guess" that
;;; works with most window managers.
;;; 
;;; Moreover, nearly all window managers are historically broken with respect
;;; to their handling of window gravity. So moving a window to its current
;;; position as returned by gtk_window_get_position() tends to result in moving
;;; the window slightly. Window managers are slowly getting better over time.
;;; 
;;; If a window has gravity GDK_GRAVITY_STATIC the window manager frame is not
;;; relevant, and thus gtk_window_get_position() will always produce accurate
;;; results. However you can't use static gravity to do things like place a
;;; window in a corner of the screen, because static gravity ignores the window
;;; manager decorations.
;;; 
;;; If you are saving and restoring your application's window positions, you
;;; should know that it's impossible for applications to do this without getting
;;; it somewhat wrong because applications do not have sufficient knowledge of
;;; window manager state. The Correct Mechanism is to support the session
;;; management protocol (see the "GnomeClient" object in the GNOME libraries
;;; for example) and allow the window manager to save your window sizes and
;;; positions.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; root_x :
;;; 	return location for X coordinate of gravity-determined reference point,
;;;     or NULL.
;;; 
;;; root_y :
;;; 	return location for Y coordinate of gravity-determined reference point,
;;;     or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_position" %gtk-window-get-position) :void
  (window (g-object gtk-window))
  (root-x (:pointer :int))
  (root-y (:pointer :int)))

;; The Lisp implementation returns the position as a value list.

(defun gtk-window-get-position (window)
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-window-get-position window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gtk-window-get-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_role ()
;;; 
;;; const gchar * gtk_window_get_role (GtkWindow *window);
;;; 
;;; Returns the role of the window. See gtk_window_set_role() for further
;;; explanation.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the role of the window if set, or NULL. The returned is owned by the
;;;     widget and must not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_size ()
;;; 
;;; void gtk_window_get_size (GtkWindow *window, gint *width, gint *height);
;;; 
;;; Obtains the current size of window. If window is not onscreen, it returns
;;; the size GTK+ will suggest to the window manager for the initial window
;;; size (but this is not reliably the same as the size the window manager will
;;; actually select). The size obtained by gtk_window_get_size() is the last
;;; size received in a GdkEventConfigure, that is, GTK+ uses its locally-stored
;;; size, rather than querying the X server for the size. As a result, if you
;;; call gtk_window_resize() then immediately call gtk_window_get_size(), the
;;; size won't have taken effect yet. After the window manager processes the
;;; resize request, GTK+ receives notification that the size has changed via a
;;; configure event, and the size of the window gets updated.
;;; 
;;; Note 1: Nearly any use of this function creates a race condition, because
;;; the size of the window may change between the time that you get the size
;;; and the time that you perform some action assuming that size is the current
;;; size. To avoid race conditions, connect to "configure-event" on the window
;;; and adjust your size-dependent state to match the size delivered in the
;;; GdkEventConfigure.
;;; 
;;; Note 2: The returned size does not include the size of the window manager
;;; decorations (aka the window frame or border). Those are not drawn by GTK+
;;; and GTK+ has no reliable method of determining their size.
;;; 
;;; Note 3: If you are getting a window size in order to position the window
;;; onscreen, there may be a better way. The preferred way is to simply set the
;;; window's semantic type with gtk_window_set_type_hint(), which allows the
;;; window manager to e.g. center dialogs. Also, if you set the transient
;;; parent of dialogs with gtk_window_set_transient_for() window managers will
;;; often center the dialog over its parent window. It's much preferred to let
;;; the window manager handle these things rather than doing it yourself,
;;; because all apps will behave consistently and according to user prefs if
;;; the window manager handles it. Also, the window manager can take the size
;;; of the window decorations/border into account, while your application
;;; cannot.
;;; 
;;; In any case, if you insist on application-specified window positioning,
;;; there's still a better way than doing it yourself -
;;; gtk_window_set_position() will frequently handle the details for you.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; width :
;;; 	return location for width, or NULL.
;;; 
;;; height :
;;; 	return location for height, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_size" %gtk-window-get-size) :void
  (window (g-object gtk-window))
  (width (:pointer :int))
  (height (:pointer :int)))

;; The Lisp implemenation returns the size as a value list.

(defun gtk-window-get-size (window)
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-window-get-size window width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(export 'gtk-window-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_title ()
;;; 
;;; const gchar * gtk_window_get_title (GtkWindow *window);
;;; 
;;; Retrieves the title of the window. See gtk_window_set_title().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the title of the window, or NULL if none has been set explicitely.
;;;     The returned string is owned by the widget and must not be modified or
;;;     freed.
;;; ----------------------------------------------------------------------------

;; gtk-window-title is the accessor of the slot title of the class gtk-window.

(defun gtk-window-get-title (window)
  (gtk-window-title window))

(export 'gtk-window-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_transient_for ()
;;; 
;;; GtkWindow * gtk_window_get_transient_for (GtkWindow *window);
;;; 
;;; Fetches the transient parent for this window.
;;; See gtk_window_set_transient_for().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the transient parent for this window, or NULL if no transient parent
;;;     has been set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_type_hint ()
;;; 
;;; GdkWindowTypeHint gtk_window_get_type_hint (GtkWindow *window);
;;; 
;;; Gets the type hint for this window. See gtk_window_set_type_hint().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the type hint for window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_skip_taskbar_hint ()
;;; 
;;; gboolean gtk_window_get_skip_taskbar_hint (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_skip_taskbar_hint()
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if window shouldn't be in taskbar
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_skip_pager_hint ()
;;; 
;;; gboolean gtk_window_get_skip_pager_hint (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_skip_pager_hint().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if window shouldn't be in pager
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_urgency_hint ()
;;; 
;;; gboolean gtk_window_get_urgency_hint (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_urgency_hint()
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if window is urgent
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_accept_focus ()
;;; 
;;; gboolean gtk_window_get_accept_focus (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_accept_focus().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if window should receive the input focus
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus_on_map ()
;;; 
;;; gboolean gtk_window_get_focus_on_map (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_focus_on_map().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if window should receive the input focus when mapped.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_group ()
;;; 
;;; GtkWindowGroup * gtk_window_get_group (GtkWindow *window);
;;; 
;;; Returns the group for window or the default group, if window is NULL or
;;; if window does not have an explicit window group.
;;; 
;;; window :
;;; 	a GtkWindow, or NULL.
;;; 
;;; Returns :
;;; 	the GtkWindowGroup for a window or the default group.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_has_group ()
;;; 
;;; gboolean gtk_window_has_group (GtkWindow *window);
;;; 
;;; Returns whether window has an explicit window group.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if window has an explicit window group. Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_window_type ()
;;; 
;;; GtkWindowType gtk_window_get_window_type (GtkWindow *window);
;;; 
;;; Gets the type of the window. See GtkWindowType.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the type of the window
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_move ()
;;; 
;;; void gtk_window_move (GtkWindow *window, gint x, gint y);
;;; 
;;; Asks the window manager to move window to the given position. Window
;;; managers are free to ignore this; most window managers ignore requests for
;;; initial window positions (instead using a user-defined placement algorithm)
;;; and honor requests after the window has already been shown.
;;; 
;;; Note: the position is the position of the gravity-determined reference
;;; point for the window. The gravity determines two things: first, the
;;; location of the reference point in root window coordinates; and second,
;;; which point on the window is positioned at the reference point.
;;; 
;;; By default the gravity is GDK_GRAVITY_NORTH_WEST, so the reference point is
;;; simply the x, y supplied to gtk_window_move(). The top-left corner of the
;;; window decorations (aka window frame or border) will be placed at x, y.
;;; Therefore, to position a window at the top left of the screen, you want to
;;; use the default gravity (which is GDK_GRAVITY_NORTH_WEST) and move the
;;; window to 0,0.
;;; 
;;; To position a window at the bottom right corner of the screen, you would
;;; set GDK_GRAVITY_SOUTH_EAST, which means that the reference point is at
;;; x + the window width and y + the window height, and the bottom-right corner
;;; of the window border will be placed at that reference point. So, to place a
;;; window in the bottom right corner you would first set gravity to south
;;; east, then write: gtk_window_move (window, gdk_screen_width() -
;;; window_width, gdk_screen_height() - window_height) (note that this example
;;; does not take multi-head scenarios into account).
;;; 
;;; The Extended Window Manager Hints specification at
;;; http://www.freedesktop.org/Standards/wm-spec has a nice table of gravities
;;; in the "implementation notes" section.
;;; 
;;; The gtk_window_get_position() documentation may also be relevant.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; x :
;;; 	X coordinate to move window to
;;; 
;;; y :
;;; 	Y coordinate to move window to
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_move" gtk-window-move) :void
  (window (g-object gtk-window))
  (x :int)
  (y :int))

(export 'gtk-window-move)

;;; ----------------------------------------------------------------------------
;;; gtk_window_parse_geometry ()
;;; 
;;; gboolean gtk_window_parse_geometry (GtkWindow *window,
;;;                                     const gchar *geometry);
;;; 
;;; Parses a standard X Window System geometry string - see the manual page
;;; for X (type 'man X') for details on this. gtk_window_parse_geometry() does
;;; work on all GTK+ ports including Win32 but is primarily intended for an X
;;; environment.
;;; 
;;; If either a size or a position can be extracted from the geometry string,
;;; gtk_window_parse_geometry() returns TRUE and calls
;;; gtk_window_set_default_size() and/or gtk_window_move() to resize/move the
;;; window.
;;; 
;;; If gtk_window_parse_geometry() returns TRUE, it will also set the
;;; GDK_HINT_USER_POS and/or GDK_HINT_USER_SIZE hints indicating to the window
;;; manager that the size/position of the window was user-specified. This causes
;;; most window managers to honor the geometry.
;;; 
;;; Note that for gtk_window_parse_geometry() to work as expected, it has to be
;;; called when the window has its "final" size, i.e. after calling
;;; gtk_widget_show_all() on the contents and gtk_window_set_geometry_hints()
;;; on the window.
;;; 
;;;  1 #include <gtk/gtk.h>
;;;  2    
;;;  3 static void
;;;  4 fill_with_content (GtkWidget *vbox)
;;;  5 {
;;;  6   /* fill with content... */
;;;  7 }
;;;  8    
;;;  9 int
;;; 10 main (int argc, char *argv[])
;;; 11 {
;;; 12   GtkWidget *window, *vbox;
;;; 13   GdkGeometry size_hints = {
;;; 14     100, 50, 0, 0, 100, 50, 10, 10, 0.0, 0.0, GDK_GRAVITY_NORTH_WEST  
;;; 15   };
;;; 16    
;;; 17   gtk_init (&argc, &argv);
;;; 18   
;;; 19   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;; 20   vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, FALSE, 0);
;;; 21    
;;; 22   gtk_container_add (GTK_CONTAINER (window), vbox);
;;; 23   fill_with_content (vbox);
;;; 24   gtk_widget_show_all (vbox);
;;; 25   
;;; 26   gtk_window_set_geometry_hints (GTK_WINDOW (window),
;;; 27                     window,
;;; 28                     &size_hints,
;;; 29                     GDK_HINT_MIN_SIZE | 
;;; 30                     GDK_HINT_BASE_SIZE | 
;;; 31                     GDK_HINT_RESIZE_INC);
;;; 32   
;;; 33   if (argc > 1)
;;; 34     {
;;; 35       if (!gtk_window_parse_geometry (GTK_WINDOW (window), argv[1]))
;;; 36         fprintf (stderr, "Failed to parse '%s'\n", argv[1]);
;;; 37     }
;;; 38   
;;; 39   gtk_widget_show_all (window);
;;; 40   gtk_main ();
;;; 41    
;;; 42   return 0;
;;; 43 }
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; geometry :
;;; 	geometry string
;;; 
;;; Returns :
;;; 	TRUE if string was parsed successfully
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_parse_geometry" gtk-window-parse-geometry) :boolean
  (window (g-object gtk-window))
  (geometry :string))

(export 'gtk-window-parse-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_reshow_with_initial_size ()
;;; 
;;; void gtk_window_reshow_with_initial_size (GtkWindow *window);
;;; 
;;; Hides window, then reshows it, resetting the default size and position of
;;; the window. Used by GUI builders only.
;;; 
;;; window :
;;; 	a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_reshow_with_initial_size"
          gtk-window-reshow-with-initial-size) :void
  (window (g-object gtk-window)))

(export 'gtk-window-reshow-with-initial-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize ()
;;; 
;;; void gtk_window_resize (GtkWindow *window, gint width, gint height)
;;; 
;;; Resizes the window as if the user had done so, obeying geometry
;;; constraints. The default geometry constraint is that windows may not be
;;; smaller than their size request; to override this constraint, call
;;; gtk_widget_set_size_request() to set the window's request to a smaller
;;; value.
;;; 
;;; If gtk_window_resize() is called before showing a window for the first
;;; time, it overrides any default size set with gtk_window_set_default_size().
;;; 
;;; Windows may not be resized smaller than 1 by 1 pixels.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; width :
;;; 	width in pixels to resize the window to
;;; 
;;; height :
;;; 	height in pixels to resize the window to
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_resize" gtk-window-resize) :void
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_to_geometry ()
;;; 
;;; void gtk_window_resize_to_geometry (GtkWindow *window,
;;;                                     gint width, gint height);
;;; 
;;; Like gtk_window_resize(), but width and height are interpreted in terms of
;;; the base size and increment set with gtk_window_set_geometry_hints.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; width :
;;; 	width in resize increments to resize the window to
;;; 
;;; height :
;;; 	height in resize increments to resize the window to
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_list ()
;;; 
;;; void gtk_window_set_default_icon_list (GList *list);
;;; 
;;; Sets an icon list to be used as fallback for windows that haven't had
;;; gtk_window_set_icon_list() called on them to set up a window-specific icon
;;; list. This function allows you to set up the icon for all windows in your
;;; app at once.
;;; 
;;; See gtk_window_set_icon_list() for more details.
;;; 
;;; icon-list :
;;; 	a list of GdkPixbuf.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_list" gtk-window-set-default-icon-list)
    :boolean
  (icon-list (g-list (g-object gdk-pixbuf))))

(export 'gtk-window-set-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon ()
;;; 
;;; void gtk_window_set_default_icon (GdkPixbuf *icon)
;;; 
;;; Sets an icon to be used as fallback for windows that haven't had
;;; gtk_window_set_icon() called on them from a pixbuf.
;;; 
;;; icon :
;;; 	the icon
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon" gtk-window-set-default-icon) :void
  (icon (g-object gdk-pixbuf)))

(export 'gtk-window-set-default-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_from_file ()
;;; 
;;; gboolean gtk_window_set_default_icon_from_file (const gchar *filename,
;;;                                                 GError **err)
;;; 
;;; Sets an icon to be used as fallback for windows that haven't had
;;; gtk_window_set_icon_list() called on them from a file on disk. Warns on
;;; failure if err is NULL.
;;; 
;;; filename :
;;; 	location of icon file.
;;; 
;;; err :
;;; 	location to store error, or NULL.
;;; 
;;; Returns :
;;; 	TRUE if setting the icon succeeded.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_name ()
;;; 
;;; void gtk_window_set_default_icon_name (const gchar *name)
;;; 
;;; Sets an icon to be used as fallback for windows that haven't had
;;; gtk_window_set_icon_list() called on them from a named themed icon, see
;;; gtk_window_set_icon_name().
;;; 
;;; name :
;;; 	the name of the themed icon
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_name" gtk-window-set-default-icon-name)
    :void
  (name :string))

(export 'gtk-window-set-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon ()
;;; 
;;; void gtk_window_set_icon (GtkWindow *window, GdkPixbuf *icon);
;;; 
;;; Sets up the icon representing a GtkWindow. This icon is used when the window
;;; is minimized (also known as iconified). Some window managers or desktop
;;; environments may also place it in the window frame, or display it in other
;;; contexts.
;;; 
;;; The icon should be provided in whatever size it was naturally drawn; that
;;; is, don't scale the image before passing it to GTK+. Scaling is postponed
;;; until the last minute, when the desired final size is known, to allow best
;;; quality.
;;; 
;;; If you have your icon hand-drawn in multiple sizes, use
;;; gtk_window_set_icon_list(). Then the best size will be used.
;;; 
;;; This function is equivalent to calling gtk_window_set_icon_list() with a
;;; 1-element list.
;;; 
;;; See also gtk_window_set_default_icon_list() to set the icon for all windows
;;; in your application in one go.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; icon :
;;; 	icon image, or NULL. [allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_list ()
;;; 
;;; void gtk_window_set_icon_list (GtkWindow *window, GList *list);
;;; 
;;; Sets up the icon representing a GtkWindow. The icon is used when the window
;;; is minimized (also known as iconified). Some window managers or desktop
;;; environments may also place it in the window frame, or display it in other
;;; contexts.
;;; 
;;; gtk_window_set_icon_list() allows you to pass in the same icon in several
;;; hand-drawn sizes. The list should contain the natural sizes your icon is
;;; available in; that is, don't scale the image before passing it to GTK+.
;;; Scaling is postponed until the last minute, when the desired final size is
;;; known, to allow best quality.
;;; 
;;; By passing several sizes, you may improve the final image quality of the
;;; icon, by reducing or eliminating automatic image scaling.
;;; 
;;; Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and larger
;;; images (64x64, 128x128) if you have them.
;;; 
;;; See also gtk_window_set_default_icon_list() to set the icon for all windows
;;; in your application in one go.
;;; 
;;; Note that transient windows (those who have been set transient for another
;;; window using gtk_window_set_transient_for()) will inherit their icon from
;;; their transient parent. So there's no need to explicitly set the icon on
;;; transient windows.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; list :
;;; 	list of GdkPixbuf. [element-type GdkPixbuf]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_from_file ()
;;; 
;;; gboolean gtk_window_set_icon_from_file (GtkWindow *window,
;;;                                         const gchar *filename,
;;;                                         GError **err);
;;; 
;;; Sets the icon for window. Warns on failure if err is NULL.
;;; 
;;; This function is equivalent to calling gtk_window_set_icon() with a pixbuf
;;; created by loading the image from filename.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; filename :
;;; 	location of icon file. [type filename]
;;; 
;;; err :
;;; 	location to store error, or NULL.
;;; 
;;; Returns :
;;; 	TRUE if setting the icon succeeded.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_name ()
;;; 
;;; void gtk_window_set_icon_name (GtkWindow *window, const gchar *name)
;;; 
;;; Sets the icon for the window from a named themed icon. See the docs for
;;; GtkIconTheme for more details.
;;; 
;;; Note that this has nothing to do with the WM_ICON_NAME property which is
;;; mentioned in the ICCCM.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; name :
;;; 	the name of the themed icon.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_auto_startup_notification ()
;;; 
;;; void gtk_window_set_auto_startup_notification (gboolean setting)
;;; 
;;; By default, after showing the first GtkWindow, GTK+ calls
;;; gdk_notify_startup_complete(). Call this function to disable the automatic
;;; startup notification. You might do this if your first window is a splash
;;; screen, and you want to delay notification until after your real main
;;; window has been shown, for example.
;;; 
;;; In that example, you would disable startup notification temporarily, show
;;; your splash screen, then re-enable it so that showing the main window would
;;; automatically result in notification.
;;; 
;;; setting :
;;; 	TRUE to automatically do startup notification
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_auto_startup_notification"
          gtk-set-window-auto-startup-notification) :void
  (setting :boolean))

(export 'gtk-set-window-auto-startup-notification)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_opacity ()
;;; 
;;; gdouble gtk_window_get_opacity (GtkWindow *window);
;;; 
;;; Fetches the requested opacity for this window. See gtk_window_set_opacity().
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	the requested opacity for this window.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_opacity ()
;;; 
;;; void gtk_window_set_opacity (GtkWindow *window, gdouble opacity);
;;; 
;;; Request the windowing system to make window partially transparent, with
;;; opacity 0 being fully transparent and 1 fully opaque. (Values of the
;;; opacity parameter are clamped to the [0,1] range.) On X11 this has any
;;; effect only on X screens with a compositing manager running.
;;; See gtk_widget_is_composited(). On Windows it should work always.
;;; 
;;; Note that setting a window's opacity after the window has been shown causes
;;; it to flicker once on Windows.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; opacity :
;;; 	desired opacity, between 0 and 1
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_mnemonics_visible ()
;;; 
;;; gboolean gtk_window_get_mnemonics_visible (GtkWindow *window)
;;; 
;;; Gets the value of the "mnemonics-visible" property.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if mnemonics are supposed to be visible in this window.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_mnemonics_visible ()
;;; 
;;; void gtk_window_set_mnemonics_visible (GtkWindow *window, gboolean setting)
;;; 
;;; Sets the "mnemonics-visible" property.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	the new value
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus_visible ()
;;; 
;;; gboolean gtk_window_get_focus_visible (GtkWindow *window);
;;; 
;;; Gets the value of the "focus-visible" property.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if 'focus rectangles' are supposed to be visible in this window.
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_focus_visible ()
;;; 
;;; void gtk_window_set_focus_visible (GtkWindow *window, gboolean setting);
;;; 
;;; Sets the "focus-visible" property.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	the new value
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_has_resize_grip ()
;;; 
;;; void gtk_window_set_has_resize_grip (GtkWindow *window, gboolean value);
;;; 
;;; Sets whether window has a corner resize grip.
;;; 
;;; Note that the resize grip is only shown if the window is actually resizable
;;; and not maximized. Use gtk_window_resize_grip_is_visible() to find out if
;;; the resize grip is currently shown.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; value :
;;; 	TRUE to allow a resize grip
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_has_resize_grip ()
;;; 
;;; gboolean gtk_window_get_has_resize_grip (GtkWindow *window);
;;; 
;;; Determines whether the window may have a resize grip.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if the window has a resize grip
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_grip_is_visible ()
;;; 
;;; gboolean gtk_window_resize_grip_is_visible (GtkWindow *window);
;;; 
;;; Determines whether a resize grip is visible for the specified window.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	TRUE if a resize grip exists and is visible
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_resize_grip_area ()
;;; 
;;; gboolean gtk_window_get_resize_grip_area (GtkWindow *window,
;;;                                           GdkRectangle *rect);
;;; 
;;; If a window has a resize grip, this will retrieve the grip position, width
;;; and height into the specified GdkRectangle.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; rect :
;;; 	a pointer to a GdkRectangle which we should store the resize grip area.
;;; 
;;; Returns :
;;; 	TRUE if the resize grip's area was retrieved
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_application ()
;;; 
;;; GtkApplication * gtk_window_get_application (GtkWindow *window);
;;; 
;;; Gets the GtkApplication associated with the window (if any).
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; Returns :
;;; 	a GtkApplication, or NULL.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_application ()
;;; 
;;; void gtk_window_set_application (GtkWindow *window,
;;;                                  GtkApplication *application);
;;; 
;;; Sets or unsets the GtkApplication associated with the window.
;;; 
;;; The application will be kept alive for at least as long as the window
;;; is open.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; application :
;;; 	a GtkApplication, or NULL.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_has_user_ref_count ()
;;; 
;;; void gtk_window_set_has_user_ref_count (GtkWindow *window,
;;;                                         gboolean setting);
;;; 
;;; Tells GTK+ whether to drop its extra reference to the window when
;;; gtk_window_destroy() is called.
;;; 
;;; This function is only exported for the benefit of language bindings which
;;; may need to keep the window alive until their wrapper object is garbage
;;; collected. There is no justification for ever calling this function in an
;;; application.
;;; 
;;; window :
;;; 	a GtkWindow
;;; 
;;; setting :
;;; 	the new value
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.window.lisp --------------------------------------------
