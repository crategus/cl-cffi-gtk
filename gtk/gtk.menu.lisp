;;; ----------------------------------------------------------------------------
;;; gtk.menu.lisp
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
;;; GtkMenu
;;; 
;;; A menu widget
;;; 
;;; Synopsis
;;; 
;;;     GtkMenu
;;;
;;;     gtk_menu_new
;;;     gtk_menu_set_screen
;;;     gtk_menu_reorder_child
;;;     gtk_menu_attach
;;;     gtk_menu_popup_for_device
;;;     gtk_menu_popup
;;;     gtk_menu_set_accel_group
;;;     gtk_menu_get_accel_group
;;;     gtk_menu_set_accel_path
;;;     gtk_menu_get_accel_path
;;;     gtk_menu_set_title
;;;     gtk_menu_get_title
;;;     gtk_menu_set_monitor
;;;     gtk_menu_get_monitor
;;;     gtk_menu_get_tearoff_state
;;;     gtk_menu_set_reserve_toggle_size
;;;     gtk_menu_get_reserve_toggle_size
;;;     gtk_menu_popdown
;;;     gtk_menu_reposition
;;;     gtk_menu_get_active
;;;     gtk_menu_set_active
;;;     gtk_menu_set_tearoff_state
;;;     gtk_menu_attach_to_widget
;;;     gtk_menu_detach
;;;     gtk_menu_get_attach_widget
;;;     gtk_menu_get_for_attach_widget
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkMenuShell
;;;                            +----GtkMenu
;;;                                  +----GtkRecentChooserMenu
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMenu implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "accel-group"              GtkAccelGroup*        : Read / Write
;;;   "accel-path"               gchar*                : Read / Write
;;;   "active"                   gint                  : Read / Write
;;;   "attach-widget"            GtkWidget*            : Read / Write
;;;   "monitor"                  gint                  : Read / Write
;;;   "reserve-toggle-size"      gboolean              : Read / Write
;;;   "tearoff-state"            gboolean              : Read / Write
;;;   "tearoff-title"            gchar*                : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "bottom-attach"            gint                  : Read / Write
;;;   "left-attach"              gint                  : Read / Write
;;;   "right-attach"             gint                  : Read / Write
;;;   "top-attach"               gint                  : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "arrow-placement"          GtkArrowPlacement     : Read
;;;   "arrow-scaling"            gfloat                : Read
;;;   "double-arrows"            gboolean              : Read
;;;   "horizontal-offset"        gint                  : Read
;;;   "horizontal-padding"       gint                  : Read
;;;   "vertical-offset"          gint                  : Read
;;;   "vertical-padding"         gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "move-scroll"                                    : Action
;;; 
;;; Description
;;; 
;;; A GtkMenu is a GtkMenuShell that implements a drop down menu consisting of 
;;; a list of GtkMenuItem objects which can be navigated and activated by the
;;; user to perform application functions.
;;; 
;;; A GtkMenu is most commonly dropped down by activating a GtkMenuItem in a 
;;; GtkMenuBar or popped up by activating a GtkMenuItem in another GtkMenu.
;;; 
;;; A GtkMenu can also be popped up by activating a GtkOptionMenu. Other 
;;; composite widgets such as the GtkNotebook can pop up a GtkMenu as well.
;;; 
;;; Applications can display a GtkMenu as a popup menu by calling the 
;;; gtk_menu_popup() function. The example below shows how an application can
;;; pop up a menu when the 3rd mouse button is pressed.
;;; 
;;; Example 75. Connecting the popup signal handler.
;;; 
;;; /* connect our handler which will popup the menu */
;;; g_signal_connect_swapped (window, "button_press_event",
;;; G_CALLBACK (my_popup_handler), menu);
;;; 
;;; Example 76. Signal handler which displays a popup menu.
;;; 
;;; static gint
;;; my_popup_handler (GtkWidget *widget, GdkEvent *event)
;;; {
;;;   GtkMenu *menu;
;;;   GdkEventButton *event_button;
;;; 
;;;   g_return_val_if_fail (widget != NULL, FALSE);
;;;   g_return_val_if_fail (GTK_IS_MENU (widget), FALSE);
;;;   g_return_val_if_fail (event != NULL, FALSE);
;;; 
;;;   /* The "widget" is the menu that was supplied when 
;;;    * g_signal_connect_swapped() was called.
;;;    */
;;;   menu = GTK_MENU (widget);
;;; 
;;;   if (event->type == GDK_BUTTON_PRESS)
;;;     {
;;;       event_button = (GdkEventButton *) event;
;;;       if (event_button->button == 3)
;;;         {
;;;           gtk_menu_popup (menu, NULL, NULL, NULL, NULL, 
;;;                           event_button->button, event_button->time);
;;;           return TRUE;
;;;         }
;;;     }
;;; 
;;;   return FALSE;
;;; }
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-group" property
;;; 
;;;   "accel-group"              GtkAccelGroup*        : Read / Write
;;; 
;;; The accel group holding accelerators for the menu.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-path" property
;;; 
;;;   "accel-path"               gchar*                : Read / Write
;;; 
;;; An accel path used to conveniently construct accel paths of child items.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gint                  : Read / Write
;;; 
;;; The index of the currently selected menu item, or -1 if no menu item is
;;; selected.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "attach-widget" property
;;; 
;;;   "attach-widget"            GtkWidget*            : Read / Write
;;; 
;;; The widget the menu is attached to. Setting this property attaches the menu
;;; without a GtkMenuDetachFunc. If you need to use a detacher, use
;;; gtk_menu_attach_to_widget() directly.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "monitor" property
;;; 
;;;   "monitor"                  gint                  : Read / Write
;;; 
;;; The monitor the menu will be popped up on.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "reserve-toggle-size" property
;;; 
;;;   "reserve-toggle-size"      gboolean              : Read / Write
;;; 
;;; A boolean that indicates whether the menu reserves space for toggles and 
;;; icons, regardless of their actual presence.
;;; 
;;; This property should only be changed from its default value for 
;;; special-purposes such as tabular menus. Regular menus that are connected to
;;; a menu bar or context menus should reserve toggle space for consistency.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "tearoff-state" property
;;; 
;;;   "tearoff-state"            gboolean              : Read / Write
;;; 
;;; A boolean that indicates whether the menu is torn-off.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "tearoff-title" property
;;; 
;;;   "tearoff-title"            gchar*                : Read / Write
;;; 
;;; A title that may be displayed by the window manager when this menu is 
;;; torn-off.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "bottom-attach" child property
;;; 
;;;   "bottom-attach"            gint                  : Read / Write
;;; 
;;; The row number to attach the bottom of the child to.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "left-attach" child property
;;; 
;;;   "left-attach"              gint                  : Read / Write
;;; 
;;; The column number to attach the left side of the child to.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "right-attach" child property
;;; 
;;;   "right-attach"             gint                  : Read / Write
;;; 
;;; The column number to attach the right side of the child to.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "top-attach" child property
;;; 
;;;   "top-attach"               gint                  : Read / Write
;;; 
;;; The row number to attach the top of the child to.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-placement" style property
;;; 
;;;   "arrow-placement"          GtkArrowPlacement     : Read
;;; 
;;; Indicates where scroll arrows should be placed.
;;; 
;;; Default value: GTK_ARROWS_BOTH
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-scaling" style property
;;; 
;;;   "arrow-scaling"            gfloat                : Read
;;; 
;;; Arbitrary constant to scale down the size of the scroll arrow.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.7
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "double-arrows" style property
;;; 
;;;   "double-arrows"            gboolean              : Read
;;; 
;;; When scrolling, always show both arrows.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "horizontal-offset" style property
;;; 
;;;   "horizontal-offset"        gint                  : Read
;;; 
;;; When the menu is a submenu, position it this number of pixels offset 
;;; horizontally.
;;; 
;;; Default value: -2
;;;
;;; ----------------------------------------------------------------------------
;;; The "horizontal-padding" style property
;;; 
;;;   "horizontal-padding"       gint                  : Read
;;; 
;;; Extra space at the left and right edges of the menu.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "vertical-offset" style property
;;; 
;;;   "vertical-offset"          gint                  : Read
;;; 
;;; When the menu is a submenu, position it this number of pixels offset 
;;; vertically.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "vertical-padding" style property
;;; 
;;;   "vertical-padding"         gint                  : Read
;;; 
;;; Extra space at the top and bottom of the menu.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-scroll" signal
;;; 
;;; void user_function (GtkMenu      *menu,
;;;                     GtkScrollType scroll_type,
;;;                     gpointer      user_data)        : Action
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; scroll_type :
;;;     a GtkScrollType
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenu
;;; 
;;; struct GtkMenu;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenu" gtk-menu
  (:superclass gtk-menu-shell
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_menu_get_type")
  ((accel-group gtk-menu-accel-group
    "accel-group" "GtkAccelGroup" t t)
   (accel-path gtk-menu-accel-path
    "accel-path" "gchararray" t t)
   (active gtk-menu-active
    "active" "gint" t t)
   (attach-widget gtk-menu-attach-widget
    "attach-widget" "GtkWidget" t t)
   (monitor gtk-menu-monitor
    "monitor" "gint" t t)
   (tearoff-state gtk-menu-tearoff-state
    "tearoff-state" "gboolean" t t)
   (tearoff-title gtk-menu-tearoff-title
    "tearoff-title" "gchararray" t t)
   (:cffi screen gtk-menu-screen g-object
          nil "gtk_menu_set_screen")
   (:cffi title gtk-menu-title
          (:string :free-from-foreign nil :free-to-foreign t)
          "gtk_menu_get_title" "gtk_menu_set_title")))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_new ()
;;; 
;;; GtkWidget * gtk_menu_new (void);
;;; 
;;; Creates a new GtkMenu
;;; 
;;; Returns :
;;;     a new GtkMenu
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_screen ()
;;; 
;;; void gtk_menu_set_screen (GtkMenu *menu, GdkScreen *screen);
;;; 
;;; Sets the GdkScreen on which the menu will be displayed.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; screen :
;;;     a GdkScreen, or NULL if the screen should be determined by the widget
;;;     the menu is attached to.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_reorder_child ()
;;; 
;;; void gtk_menu_reorder_child (GtkMenu *menu,
;;;                              GtkWidget *child,
;;;                              gint position);
;;; 
;;; Moves child to a new position in the list of menu children.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; child :
;;;     the GtkMenuItem to move
;;; 
;;; position :
;;;     the new position to place child. Positions are numbered from 0 to n - 1
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_reorder_child" gtk-menu-reorder-child) :void
  (menu g-object)
  (child g-object)
  (position :int))

(export 'gtk-menu-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_attach ()
;;; 
;;; void gtk_menu_attach (GtkMenu *menu,
;;;                       GtkWidget *child,
;;;                       guint left_attach,
;;;                       guint right_attach,
;;;                       guint top_attach,
;;;                       guint bottom_attach);
;;; 
;;; Adds a new GtkMenuItem to a (table) menu. The number of 'cells' that an
;;; item will occupy is specified by left_attach, right_attach, top_attach and
;;; bottom_attach. These each represent the leftmost, rightmost, uppermost and
;;; lower column and row numbers of the table. (Columns and rows are indexed
;;; from zero).
;;; 
;;; Note that this function is not related to gtk_menu_detach().
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; child :
;;;     a GtkMenuItem
;;; 
;;; left_attach :
;;;     The column number to attach the left side of the item to
;;; 
;;; right_attach :
;;;     The column number to attach the right side of the item to
;;; 
;;; top_attach :
;;;     The row number to attach the top of the item to
;;; 
;;; bottom_attach :
;;;     The row number to attach the bottom of the item to
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_attach" gtk-menu-attach) :void
  (menu g-object)
  (child g-object)
  (left-attach :uint)
  (right-attach :uint)
  (top-attach :uint)
  (bottom-attach :uint))

(export 'gtk-menu-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup_for_device ()
;;; 
;;; void gtk_menu_popup_for_device (GtkMenu *menu,
;;;                                 GdkDevice *device,
;;;                                 GtkWidget *parent_menu_shell,
;;;                                 GtkWidget *parent_menu_item,
;;;                                 GtkMenuPositionFunc func,
;;;                                 gpointer data,
;;;                                 GDestroyNotify destroy,
;;;                                 guint button,
;;;                                 guint32 activate_time);
;;; 
;;; Displays a menu and makes it available for selection.
;;; 
;;; Applications can use this function to display context-sensitive menus, and 
;;; will typically supply NULL for the parent_menu_shell, parent_menu_item,
;;; func, data and destroy parameters. The default menu positioning function
;;; will position the menu at the current position of device (or its
;;; corresponding pointer).
;;; 
;;; The button parameter should be the mouse button pressed to initiate the 
;;; menu popup. If the menu popup was initiated by something other than a mouse 
;;; button press, such as a mouse button release or a keypress, button should
;;; be 0.
;;; 
;;; The activate_time parameter is used to conflict-resolve initiation of 
;;; concurrent requests for mouse/keyboard grab requests. To function properly,
;;; this needs to be the time stamp of the user event (such as a mouse click or
;;; key press) that caused the initiation of the popup. Only if no such event is
;;; available, gtk_get_current_event_time() can be used instead.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; parent_menu_shell :
;;;     the menu shell containing the triggering menu item, or NULL
;;; 
;;; parent_menu_item :
;;;     the menu item whose activation triggered the popup, or NULL
;;; 
;;; func :
;;;     a user supplied function used to position the menu, or NULL
;;; 
;;; data :
;;;     user supplied data to be passed to func
;;; 
;;; destroy :
;;;     destroy notify for data
;;; 
;;; button :
;;;     the mouse button which was pressed to initiate the event
;;; 
;;; activate_time :
;;;     the time at which the activation event occurred
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup ()
;;; 
;;; void gtk_menu_popup (GtkMenu *menu,
;;;                      GtkWidget *parent_menu_shell,
;;;                      GtkWidget *parent_menu_item,
;;;                      GtkMenuPositionFunc func,
;;;                      gpointer data,
;;;                      guint button,
;;;                      guint32 activate_time);
;;; 
;;; Displays a menu and makes it available for selection.
;;; 
;;; Applications can use this function to display context-sensitive menus, and
;;; will typically supply NULL for the parent_menu_shell, parent_menu_item, func
;;; and data parameters. The default menu positioning function will position the
;;; menu at the current mouse cursor position.
;;; 
;;; The button parameter should be the mouse button pressed to initiate the
;;; menu popup. If the menu popup was initiated by something other than a mouse
;;; button press, such as a mouse button release or a keypress, button should
;;; be 0.
;;; 
;;; The activate_time parameter is used to conflict-resolve initiation of 
;;; concurrent requests for mouse/keyboard grab requests. To function properly, 
;;; this needs to be the timestamp of the user event (such as a mouse click or
;;; key press) that caused the initiation of the popup. Only if no such event
;;; is available, gtk_get_current_event_time() can be used instead.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; parent_menu_shell :
;;;     the menu shell containing the triggering menu item, or NULL.
;;; 
;;; parent_menu_item :
;;;     the menu item whose activation triggered the popup, or NULL.
;;; 
;;; func :
;;;     a user supplied function used to position the menu, or NULL.
;;; 
;;; data :
;;;     user supplied data to be passed to func.
;;; 
;;; button :
;;;     the mouse button which was pressed to initiate the event.
;;; 
;;; activate_time :
;;;     the time at which the activation event occurred.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popup" %gtk-menu-popup) :void
  (menu g-object)
  (parent-menu-shell g-object)
  (parent-menu-item g-object)
  (menu-position-func :pointer)
  (data :pointer)
  (button :uint)
  (activate-time :uint32))

(defcallback gtk-menu-position-func-callback :void
    ((menu g-object)
     (x :pointer)
     (y :pointer)
     (push-in :pointer)
     (data :pointer))
  (restart-case
    (multiple-value-bind (rx ry rpush-in)
        (funcall (get-stable-pointer-value data) menu)
      (setf (mem-ref x :int) rx
            (mem-ref y :int) ry
            (mem-ref push-in :boolean) rpush-in))
    (return-zero () (setf (mem-ref x :int) 0
                          (mem-ref y :int) 0
                          (mem-ref push-in :boolean) nil))))

(defun gtk-menu-popup (menu &key parent-menu-shell
                             parent-menu-item
                             position-func
                             (button 0)
                             (activate-time (gtk-get-current-event-time)))
  (if position-func
      (with-stable-pointer (ptr position-func)
        (%gtk-menu-popup menu
                         parent-menu-shell
                         parent-menu-item
                         (callback gtk-menu-position-func-callback)
                         ptr
                         button
                         activate-time))
      (%gtk-menu-popup menu
                       parent-menu-shell
                       parent-menu-item
                       (null-pointer)
                       (null-pointer)
                       button
                       activate-time)))

(export 'gtk-menu-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_accel_group ()
;;; 
;;; void gtk_menu_set_accel_group (GtkMenu *menu, GtkAccelGroup *accel_group);
;;; 
;;; Set the GtkAccelGroup which holds global accelerators for the menu. This 
;;; accelerator group needs to also be added to all windows that this menu is
;;; being used in with gtk_window_add_accel_group(), in order for those windows
;;; to support all the accelerators contained in this group.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; accel_group :
;;;     the GtkAccelGroup to be associated with the menu
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_accel_group ()
;;; 
;;; GtkAccelGroup * gtk_menu_get_accel_group (GtkMenu *menu);
;;; 
;;; Gets the GtkAccelGroup which holds global accelerators for the menu. See 
;;; gtk_menu_set_accel_group().
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     the GtkAccelGroup associated with the menu.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_accel_path ()
;;; 
;;; void gtk_menu_set_accel_path (GtkMenu *menu, const gchar *accel_path);
;;; 
;;; Sets an accelerator path for this menu from which accelerator paths for its
;;; immediate children, its menu items, can be constructed. The main purpose of 
;;; this function is to spare the programmer the inconvenience of having to call
;;; gtk_menu_item_set_accel_path() on each menu item that should support runtime
;;; user changable accelerators. Instead, by just calling
;;; gtk_menu_set_accel_path() on their parent, each menu item of this menu, that
;;; contains a label describing its purpose, automatically gets an accel path
;;; assigned.
;;; 
;;; For example, a menu containing menu items "New" and "Exit", will, after 
;;; gtk_menu_set_accel_path (menu, "<Gnumeric-Sheet>/File"); has been called, 
;;; assign its items the accel paths: "<Gnumeric-Sheet>/File/New" and 
;;; "<Gnumeric-Sheet>/File/Exit".
;;; 
;;; Assigning accel paths to menu items then enables the user to change their 
;;; accelerators at runtime. More details about accelerator paths and their
;;; default setups can be found at gtk_accel_map_add_entry().
;;; 
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you 
;;; pass a static string, you can save some memory by interning it first with 
;;; g_intern_static_string().
;;; 
;;; menu :
;;;     a valid GtkMenu
;;; 
;;; accel_path :
;;;     a valid accelerator path
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_accel_path ()
;;; 
;;; const gchar * gtk_menu_get_accel_path (GtkMenu *menu);
;;; 
;;; Retrieves the accelerator path set on the menu.
;;; 
;;; menu :
;;;     a valid GtkMenu
;;; 
;;; Returns :
;;;     the accelerator path set on the menu.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_title ()
;;; 
;;; void gtk_menu_set_title (GtkMenu *menu, const gchar *title);
;;; 
;;; Sets the title string for the menu.
;;; 
;;; The title is displayed when the menu is shown as a tearoff menu. If title 
;;; is NULL, the menu will see if it is attached to a parent menu item, and if
;;; so it will try to use the same text as that menu item's label.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; title :
;;;     a string containing the title for the menu
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_title ()
;;; 
;;; const gchar * gtk_menu_get_title (GtkMenu *menu);
;;; 
;;; Returns the title of the menu. See gtk_menu_set_title().
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     the title of the menu, or NULL if the menu has no title set on it. This 
;;;     string is owned by GTK+ and should not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_monitor ()
;;; 
;;; void gtk_menu_set_monitor (GtkMenu *menu, gint monitor_num);
;;; 
;;; Informs GTK+ on which monitor a menu should be popped up.
;;; See gdk_screen_get_monitor_geometry().
;;; 
;;; This function should be called from a GtkMenuPositionFunc if the menu should
;;; not appear on the same monitor as the pointer. This information can't be
;;; reliably inferred from the coordinates returned by a GtkMenuPositionFunc,
;;; since, for very long menus, these coordinates may extend beyond the monitor
;;; boundaries or even the screen boundaries.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; monitor_num :
;;;     the number of the monitor on which the menu should be popped up
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_monitor ()
;;; 
;;; gint gtk_menu_get_monitor (GtkMenu *menu);
;;; 
;;; Retrieves the number of the monitor on which to show the menu.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     the number of the monitor on which the menu should be popped up or -1,
;;;     if no monitor has been set
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_tearoff_state ()
;;; 
;;; gboolean gtk_menu_get_tearoff_state (GtkMenu *menu);
;;; 
;;; Returns whether the menu is torn off. See gtk_menu_set_tearoff_state().
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     TRUE if the menu is currently torn off.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_reserve_toggle_size ()
;;; 
;;; void gtk_menu_set_reserve_toggle_size (GtkMenu *menu,
;;;                                        gboolean reserve_toggle_size);
;;; 
;;; Sets whether the menu should reserve space for drawing toggles or icons,
;;; regardless of their actual presence.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; reserve_toggle_size :
;;;     whether to reserve size for toggles
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_reserve_toggle_size ()
;;; 
;;; gboolean gtk_menu_get_reserve_toggle_size (GtkMenu *menu);
;;; 
;;; Returns whether the menu reserves space for toggles and icons, regardless
;;; of their actual presence.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     Whether the menu reserves toggle space
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popdown ()
;;; 
;;; void gtk_menu_popdown (GtkMenu *menu);
;;; 
;;; Removes the menu from the screen.
;;; 
;;; menu :
;;;     a GtkMenu
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popdown" gtk-menu-popdown) :void
  (menu g-object))

(export 'gtk-menu-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_reposition ()
;;; 
;;; void gtk_menu_reposition (GtkMenu *menu);
;;; 
;;; Repositions the menu according to its position function.
;;; 
;;; menu :
;;;     a GtkMenu
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_reposition" gtk-menu-reposition) :void
  (menu g-object))

(export 'gtk-menu-reposition)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_active ()
;;; 
;;; GtkWidget * gtk_menu_get_active (GtkMenu *menu);
;;; 
;;; Returns the selected menu item from the menu. This is used by the 
;;; GtkOptionMenu.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     the GtkMenuItem that was last selected in the menu. If a selection has
;;;     not yet been made, the first menu item is selected
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_active ()
;;; 
;;; void gtk_menu_set_active (GtkMenu *menu, guint index);
;;; 
;;; Selects the specified menu item within the menu. This is used by the
;;; GtkOptionMenu and should not be used by anyone else.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; index :
;;;     the index of the menu item to select. Iindex values are from 0 to n-1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_tearoff_state ()
;;; 
;;; void gtk_menu_set_tearoff_state (GtkMenu *menu, gboolean torn_off);
;;; 
;;; Changes the tearoff state of the menu. A menu is normally displayed as drop 
;;; down menu which persists as long as the menu is active. It can also be 
;;; displayed as a tearoff menu which persists until it is closed or reattached.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; torn_off :
;;;     If TRUE, menu is displayed as a tearoff menu.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_attach_to_widget ()
;;; 
;;; void gtk_menu_attach_to_widget (GtkMenu *menu,
;;;                                 GtkWidget *attach_widget,
;;;                                 GtkMenuDetachFunc detacher);
;;; 
;;; Attaches the menu to the widget and provides a callback function that will
;;; be invoked when the menu calls gtk_menu_detach() during its destruction.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; attach_widget :
;;;     the GtkWidget that the menu will be attached to
;;; 
;;; detacher :
;;;     the user supplied callback function that will be called when the menu
;;;     calls gtk_menu_detach()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_detach ()
;;; 
;;; void gtk_menu_detach (GtkMenu *menu);
;;; 
;;; Detaches the menu from the widget to which it had been attached. This
;;; function will call the callback function, detacher, provided when the
;;; gtk_menu_attach_to_widget() function was called.
;;; 
;;; menu :
;;;     a GtkMenu
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_detach" gtk-menu-detach) :void
  (menu (g-object menu)))

(export 'gtk-menu-detach)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_attach_widget ()
;;; 
;;; GtkWidget * gtk_menu_get_attach_widget (GtkMenu *menu);
;;; 
;;; Returns the GtkWidget that the menu is attached to.
;;; 
;;; menu :
;;;     a GtkMenu
;;; 
;;; Returns :
;;;     the GtkWidget that the menu is attached to. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_for_attach_widget ()
;;; 
;;; GList * gtk_menu_get_for_attach_widget (GtkWidget *widget);
;;; 
;;; Returns a list of the menus which are attached to this widget. This list is 
;;; owned by GTK+ and must not be modified.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the list of menus attached to his widget.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_get_for_attach_widget" gtk-menu-attached-to-widget)
    (g-list (g-object gtk-menu) :free-from-foreign nil)
  (width (g-object gtk-widget)))

(export 'gtk-menu-attached-to-widget)

;;; ----------------------------------------------------------------------------
;;; GtkMenuPositionFunc ()
;;; 
;;; void (*GtkMenuPositionFunc) (GtkMenu *menu,
;;;                              gint *x,
;;;                              gint *y,
;;;                              gboolean *push_in,
;;;                              gpointer user_data);
;;; 
;;; A user function supplied when calling gtk_menu_popup() which controls the 
;;; positioning of the menu when it is displayed. The function sets the x and y 
;;; parameters to the coordinates where the menu is to be drawn. To make the
;;; menu appear on a different monitor than the mouse pointer,
;;; gtk_menu_set_monitor() must be called.
;;; 
;;; menu :
;;;     a GtkMenu.
;;; 
;;; x :
;;;     address of the gint representing the horizontal position where the menu 
;;;     shall be drawn.
;;; 
;;; y :
;;;     address of the gint representing the vertical position where the menu 
;;;     shall be drawn. This is an output parameter.
;;; 
;;; push_in :
;;;     This parameter controls how menus placed outside the monitor are 
;;;     handled. If this is set to TRUE and part of the menu is outside the
;;;      monitor then GTK+ pushes the window into the visible area, effectively
;;;      modifying the popup position. Note that moving and possibly resizing
;;;      the menu around will alter the scroll position to keep the menu items
;;;      "in place", i.e. at the same monitor position they would have been
;;;      without resizing. In practice, this behavior is only useful for
;;;      combobox popups or option menus and cannot be used to simply confine a
;;;      menu to monitor boundaries. In that case, changing the scroll offset
;;;      is not desirable.
;;; 
;;; user_data :
;;;     the data supplied by the user in the gtk_menu_popup() data parameter.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkMenuDetachFunc ()
;;; 
;;; void (*GtkMenuDetachFunc) (GtkWidget *attach_widget, GtkMenu *menu);
;;; 
;;; A user function supplied when calling gtk_menu_attach_to_widget() which 
;;; will be called when the menu is later detached from the widget.
;;; 
;;; attach_widget :
;;;     the GtkWidget that the menu is being detached from.
;;; 
;;; menu :
;;;     the GtkMenu being detached.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.menu.lisp ----------------------------------------------
