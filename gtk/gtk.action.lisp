;;; ----------------------------------------------------------------------------
;;; gtk.action.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkAction
;;;
;;; An action which can be triggered by a menu or toolbar item
;;;
;;; Synopsis
;;;
;;;     GtkAction
;;;
;;;     gtk_action_new
;;;     gtk_action_get_name
;;;     gtk_action_is_sensitive
;;;     gtk_action_get_sensitive
;;;     gtk_action_set_sensitive
;;;     gtk_action_is_visible
;;;     gtk_action_get_visible
;;;     gtk_action_set_visible
;;;     gtk_action_activate
;;;     gtk_action_create_icon
;;;     gtk_action_create_menu_item
;;;     gtk_action_create_tool_item
;;;     gtk_action_create_menu
;;;     gtk_action_get_proxies
;;;     gtk_action_connect_accelerator
;;;     gtk_action_disconnect_accelerator
;;;     gtk_action_block_activate
;;;     gtk_action_unblock_activate
;;;     gtk_action_get_always_show_image
;;;     gtk_action_set_always_show_image
;;;     gtk_action_get_accel_path
;;;     gtk_action_set_accel_path
;;;     gtk_action_get_accel_closure
;;;     gtk_action_set_accel_group
;;;     gtk_action_set_label
;;;     gtk_action_get_label
;;;     gtk_action_set_short_label
;;;     gtk_action_get_short_label
;;;     gtk_action_set_tooltip
;;;     gtk_action_get_tooltip
;;;     gtk_action_set_stock_id
;;;     gtk_action_get_stock_id
;;;     gtk_action_set_gicon
;;;     gtk_action_get_gicon
;;;     gtk_action_set_icon_name
;;;     gtk_action_get_icon_name
;;;     gtk_action_set_visible_horizontal
;;;     gtk_action_get_visible_horizontal
;;;     gtk_action_set_visible_vertical
;;;     gtk_action_get_visible_vertical
;;;     gtk_action_set_is_important
;;;     gtk_action_get_is_important
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkAction
;;;          +----GtkToggleAction
;;;          +----GtkRecentAction
;;;
;;; Implemented Interfaces
;;;
;;; GtkAction implements GtkBuildable.
;;;
;;; Properties
;;;
;;;   "action-group"             GtkActionGroup*      : Read / Write
;;;   "always-show-image"        gboolean             : Read / Write / Construct
;;;   "gicon"                    GIcon*               : Read / Write
;;;   "hide-if-empty"            gboolean             : Read / Write
;;;   "icon-name"                gchar*               : Read / Write
;;;   "is-important"             gboolean             : Read / Write
;;;   "label"                    gchar*               : Read / Write
;;;   "name"                     gchar*               : Read / Write / Construct
;;;   "sensitive"                gboolean             : Read / Write
;;;   "short-label"              gchar*               : Read / Write
;;;   "stock-id"                 gchar*               : Read / Write
;;;   "tooltip"                  gchar*               : Read / Write
;;;   "visible"                  gboolean             : Read / Write
;;;   "visible-horizontal"       gboolean             : Read / Write
;;;   "visible-overflown"        gboolean             : Read / Write
;;;   "visible-vertical"         gboolean             : Read / Write
;;;
;;; Signals
;;;
;;;   "activate"                                      : No Recursion
;;;
;;; Description
;;;
;;; Actions represent operations that the user can be perform, along with some
;;; information how it should be presented in the interface. Each action
;;; provides methods to create icons, menu items and toolbar items representing
;;; itself.
;;;
;;; As well as the callback that is called when the action gets activated, the
;;; following also gets associated with the action:
;;;
;;;     a name (not translated, for path lookup)
;;;
;;;     a label (translated, for display)
;;;
;;;     an accelerator
;;;
;;;     whether label indicates a stock id
;;;
;;;     a tooltip (optional, translated)
;;;
;;;     a toolbar label (optional, shorter than label)
;;;
;;; The action will also have some state information:
;;;
;;;     visible (shown/hidden)
;;;
;;;     sensitive (enabled/disabled)
;;;
;;; Apart from regular actions, there are toggle actions, which can be toggled
;;; between two states and radio actions, of which only one in a group can be in
;;; the "active" state. Other actions can be implemented as GtkAction
;;; subclasses.
;;;
;;; Each action can have one or more proxy widgets. To act as an action proxy,
;;; widget needs to implement GtkActivatable interface. Proxies mirror the state
;;; of the action and should change when the action's state changes. Properties
;;; that are always mirrored by proxies are "sensitive" and "visible". "gicon",
;;; "icon-name", "label", "short-label" and "stock-id" properties are only
;;; mirorred if proxy widget has "use-action-appearance" property set to TRUE.
;;;
;;; When the proxy is activated, it should activate its action.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-group" property
;;;
;;;   "action-group"             GtkActionGroup*       : Read / Write
;;;
;;; The GtkActionGroup this GtkAction is associated with, or NULL (for internal
;;; use).
;;;
;;; ----------------------------------------------------------------------------
;;; The "always-show-image" property
;;;
;;;   "always-show-image"        gboolean             : Read / Write / Construct
;;;
;;; If TRUE, the action's menu item proxies will ignore the "gtk-menu-images"
;;; setting and always show their image, if available.
;;;
;;; Use this property if the menu item would be useless or hard to use without
;;; their image.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;; The "gicon" property
;;;
;;;   "gicon"                    GIcon*                : Read / Write
;;;
;;; The GIcon displayed in the GtkAction.
;;;
;;; Note that the stock icon is preferred, if the "stock-id" property holds the
;;; id of an existing stock icon.
;;;
;;; This is an appearance property and thus only applies if
;;; "use-action-appearance" is TRUE.
;;;
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "hide-if-empty" property
;;;
;;;   "hide-if-empty"            gboolean              : Read / Write
;;;
;;; When TRUE, empty menu proxies for this action are hidden.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-name" property
;;;
;;;   "icon-name"                gchar*                : Read / Write
;;;
;;; The name of the icon from the icon theme.
;;;
;;; Note that the stock icon is preferred, if the "stock-id" property holds the
;;; id of an existing stock icon, and the GIcon is preferred if the "gicon"
;;; property is set.
;;;
;;; This is an appearance property and thus only applies if
;;; "use-action-appearance" is TRUE.
;;;
;;; Default value: NULL
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-important" property
;;;
;;;   "is-important"             gboolean              : Read / Write
;;;
;;; Whether the action is considered important. When TRUE, toolitem proxies for
;;; this action show text in GTK_TOOLBAR_BOTH_HORIZ mode.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;;
;;;   "label"                    gchar*                : Read / Write
;;;
;;; The label used for menu items and buttons that activate this action. If the
;;; label is NULL, GTK+ uses the stock label specified via the stock-id
;;; property.
;;;
;;; This is an appearance property and thus only applies if
;;; "use-action-appearance" is TRUE.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "name" property
;;;
;;;   "name"                     gchar*               : Read / Write / Construct
;;;
;;; A unique name for the action.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "sensitive" property
;;;
;;;   "sensitive"                gboolean              : Read / Write
;;;
;;; Whether the action is enabled.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "short-label" property
;;;
;;;   "short-label"              gchar*                : Read / Write
;;;
;;; A shorter label that may be used on toolbar buttons.
;;;
;;; This is an appearance property and thus only applies if
;;; "use-action-appearance" is TRUE.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "stock-id" property
;;;
;;;   "stock-id"                 gchar*                : Read / Write
;;;
;;; The stock icon displayed in widgets representing this action.
;;;
;;; This is an appearance property and thus only applies if
;;; "use-action-appearance" is TRUE.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip" property
;;;
;;;   "tooltip"                  gchar*                : Read / Write
;;;
;;; A tooltip for this action.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible" property
;;;
;;;   "visible"                  gboolean              : Read / Write
;;;
;;; Whether the action is visible.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible-horizontal" property
;;;
;;;   "visible-horizontal"       gboolean              : Read / Write
;;;
;;; Whether the toolbar item is visible when the toolbar is in a horizontal
;;; orientation.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible-overflown" property
;;;
;;;   "visible-overflown"        gboolean              : Read / Write
;;;
;;; When TRUE, toolitem proxies for this action are represented in the toolbar
;;; overflow menu.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible-vertical" property
;;;
;;;   "visible-vertical"         gboolean              : Read / Write
;;;
;;; Whether the toolbar item is visible when the toolbar is in a vertical
;;; orientation.
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
;;; void user_function (GtkAction *action,
;;;                     gpointer   user_data)      : No Recursion
;;;
;;; The "activate" signal is emitted when the action is activated.
;;;
;;; action :
;;;     the GtkAction
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAction
;;;
;;; struct GtkAction;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAction" gtk-action
  (:superclass g-object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_action_get_type")
  ((action-group
    gtk-action-action-group
    "action-group" "GtkActionGroup" t t)
   (gicon
    gtk-action-gicon
    "gicon" "GIcon" t t)
   (hide-if-empty
    gtk-action-hide-if-empty
    "hide-if-empty" "gboolean" t t)
   (icon-name
    gtk-action-icon-name
    "icon-name" "gchararray" t t)
   (is-important
    gtk-action-is-important
    "is-important" "gboolean" t t)
   (label
    gtk-action-label
    "label" "gchararray" t t)
   (name
    gtk-action-name
    "name" "gchararray" t nil)
   (sensitive
    gtk-action-sensitive
    "sensitive" "gboolean" t t)
   (short-label
    gtk-action-short-label
    "short-label" "gchararray" t t)
   (stock-id
    gtk-action-stock-id
    "stock-id" "gchararray" t t)
   (tooltip
    gtk-action-tooltip
    "tooltip" "gchararray" t t)
   (visible
    gtk-action-visible
    "visible" "gboolean" t t)
   (visible-horizontal
    gtk-action-visible-horizontal
    "visible-horizontal" "gboolean" t t)
   (visible-overflown
    gtk-action-visible-overflown
    "visible-overflown" "gboolean" t t)
   (visible-vertical
    gtk-action-visible-vertical
    "visible-vertical" "gboolean" t t)
   (:cffi accel-path
          gtk-action-accel-path
          (:string :free-from-foreign nil :free-to-foreign t)
          "gtk_action_get_accel_path"
          "gtk_action_set_accel_path")
   (:cffi accel-group
          gtk-action-accel-group g-object nil
          "gtk_action_set_accel_group")))

;;; ----------------------------------------------------------------------------
;;; gtk_action_new ()
;;;
;;; GtkAction * gtk_action_new (const gchar *name,
;;;                             const gchar *label,
;;;                             const gchar *tooltip,
;;;                             const gchar *stock_id);
;;;
;;; Creates a new GtkAction object. To add the action to a GtkActionGroup and
;;; set the accelerator for the action, call
;;; gtk_action_group_add_action_with_accel(). See the section called
;;; “UI Definitions” for information on allowed action names.
;;;
;;; name :
;;;     A unique name for the action
;;;
;;; label :
;;;     the label displayed in menu items and on buttons, or NULL
;;;
;;; tooltip :
;;;     a tooltip for the action, or NULL
;;;
;;; stock_id :
;;;     the stock icon to display in widgets representing the action, or NULL
;;;
;;; Returns :
;;;     a new GtkAction
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_name ()
;;;
;;; const gchar * gtk_action_get_name (GtkAction *action);
;;;
;;; Returns the name of the action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     the name of the action. The string belongs to GTK+ and should not be
;;;     freed.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_sensitive ()
;;;
;;; gboolean gtk_action_is_sensitive (GtkAction *action);
;;;
;;; Returns whether the action is effectively sensitive.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     TRUE if the action and its associated action group are both sensitive.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_is_sensitive" gtk-action-is-sensitive) :boolean
  (action g-object))

(export 'gtk-action-is-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_sensitive ()
;;;
;;; gboolean gtk_action_get_sensitive (GtkAction *action);
;;;
;;; Returns whether the action itself is sensitive. Note that this doesn't
;;; necessarily mean effective sensitivity. See gtk_action_is_sensitive() for
;;; that.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     TRUE if the action itself is sensitive.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_sensitive ()
;;;
;;; void gtk_action_set_sensitive (GtkAction *action, gboolean sensitive);
;;;
;;; Sets the ::sensitive property of the action to sensitive. Note that this
;;; doesn't necessarily mean effective sensitivity. See
;;; gtk_action_is_sensitive() for that.
;;;
;;; action :
;;;     the action object
;;;
;;; sensitive :
;;;     TRUE to make the action sensitive
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_visible ()
;;;
;;; gboolean gtk_action_is_visible (GtkAction *action);
;;;
;;; Returns whether the action is effectively visible.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     TRUE if the action and its associated action group are both visible.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_is_visible" gtk-action-is-visible) :boolean
  (action g-object))

(export 'gtk-action-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_visible ()
;;;
;;; gboolean gtk_action_get_visible (GtkAction *action);
;;;
;;; Returns whether the action itself is visible. Note that this doesn't
;;; necessarily mean effective visibility. See gtk_action_is_sensitive() for
;;; that.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     TRUE if the action itself is visible.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_visible ()
;;;
;;; void gtk_action_set_visible (GtkAction *action, gboolean visible);
;;;
;;; Sets the ::visible property of the action to visible. Note that this doesn't
;;; necessarily mean effective visibility. See gtk_action_is_visible() for that.
;;;
;;; action :
;;;     the action object
;;;
;;; visible :
;;;     TRUE to make the action visible
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_activate ()
;;;
;;; void gtk_action_activate (GtkAction *action);
;;;
;;; Emits the "activate" signal on the specified action, if it isn't
;;; insensitive. This gets called by the proxy widgets when they get activated.
;;;
;;; It can also be used to manually activate an action.
;;;
;;; action :
;;;     the action object
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_icon ()
;;;
;;; GtkWidget * gtk_action_create_icon (GtkAction *action,
;;;                                     GtkIconSize icon_size);
;;;
;;; This function is intended for use by action implementations to create icons
;;; displayed in the proxy widgets.
;;;
;;; action :
;;;     the action object
;;;
;;; icon_size :
;;;     the size of the icon that should be created
;;;
;;; Returns :
;;;     a widget that displays the icon for this action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_icon" gtk-action-create-icon) g-object
  (action g-object)
  (icon-size gtk-icon-size))

(export 'gtk-action-create-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu_item ()
;;;
;;; GtkWidget * gtk_action_create_menu_item (GtkAction *action);
;;;
;;; Creates a menu item widget that proxies for the given action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     a menu item connected to the action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_menu_item" gtk-action-create-menu-item) g-object
  (action g-object))

(export 'gtk-action-create-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_tool_item ()
;;;
;;; GtkWidget * gtk_action_create_tool_item (GtkAction *action);
;;;
;;; Creates a toolbar item widget that proxies for the given action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     a toolbar item connected to the action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_tool_item" gtk-action-create-tool-item) g-object
  (action g-object))

(export 'gtk-action-create-tool-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu ()
;;;
;;; GtkWidget * gtk_action_create_menu (GtkAction *action);
;;;
;;; If action provides a GtkMenu widget as a submenu for the menu item or the
;;; toolbar item it creates, this function returns an instance of that menu.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     the menu item provided by the action, or NULL
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_menu" gtk-action-create-menu) g-object
  (action g-object))

(export 'gtk-action-create-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_proxies ()
;;;
;;; GSList * gtk_action_get_proxies (GtkAction *action);
;;;
;;; Returns the proxy widgets for an action. See also
;;; gtk_activatable_get_related_action().
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     a GSList of proxy widgets. The list is owned by GTK+ and must not be
;;;     modified
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_get_proxies" gtk-action-proxies)
    (g-slist g-object :free-from-foreign nil)
  (action g-object))

(export 'gtk-action-proxies)

;;; ----------------------------------------------------------------------------
;;; gtk_action_connect_accelerator ()
;;;
;;; void gtk_action_connect_accelerator (GtkAction *action);
;;;
;;; Installs the accelerator for action if action has an accel path and group.
;;; See gtk_action_set_accel_path() and gtk_action_set_accel_group()
;;;
;;; Since multiple proxies may independently trigger the installation of the
;;; accelerator, the action counts the number of times this function has been
;;; called and doesn't remove the accelerator until
;;; gtk_action_disconnect_accelerator() has been called as many times.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_connect_accelerator" gtk-action-connect-accelerator) :void
  (action g-object))

(export 'gtk-action-connect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_disconnect_accelerator ()
;;;
;;; void gtk_action_disconnect_accelerator (GtkAction *action);
;;;
;;; Undoes the effect of one call to gtk_action_connect_accelerator().
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_disconnect_accelerator" gtk-action-disconnect-accelerator)
    :void
  (action g-object))

(export 'gtk-action-disconnect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_block_activate ()
;;;
;;; void gtk_action_block_activate (GtkAction *action);
;;;
;;; Disable activation signals from the action
;;;
;;; This is needed when updating the state of your proxy GtkActivatable widget
;;; could result in calling gtk_action_activate(), this is a convenience
;;; function to avoid recursing in those cases (updating toggle state for
;;; instance).
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_block_activate" gtk-action-block-activate) :void
  (action (g-object gtk-action)))

(export 'gtk-action-block-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_unblock_activate ()
;;;
;;; void gtk_action_unblock_activate (GtkAction *action);
;;;
;;; Reenable activation signals from the action
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_unblock_activate" gtk-action-unblock-activate) :void
  (action (g-object gtk-action)))

(export 'gtk-action-unblock-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_always_show_image ()
;;;
;;; gboolean gtk_action_get_always_show_image (GtkAction *action);
;;;
;;; Returns whether action's menu item proxies will ignore the "gtk-menu-images"
;;; setting and always show their image, if available.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     TRUE if the menu item proxies will always show their image
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_always_show_image ()
;;;
;;; void gtk_action_set_always_show_image (GtkAction *action,
;;;                                        gboolean always_show);
;;;
;;; Sets whether action's menu item proxies will ignore the "gtk-menu-images"
;;; setting and always show their image, if available.
;;;
;;; Use this if the menu item would be useless or hard to use without their
;;; image.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; always_show :
;;;     TRUE if menuitem proxies should always show their image
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_path ()
;;;
;;; const gchar * gtk_action_get_accel_path (GtkAction *action);
;;;
;;; Returns the accel path for this action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     the accel path for this action, or NULL if none is set. The returned
;;;     string is owned by GTK+ and must not be freed or modified.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_accel_path ()
;;;
;;; void gtk_action_set_accel_path (GtkAction *action, const gchar *accel_path);
;;;
;;; Sets the accel path for this action. All proxy widgets associated with the
;;; action will have this accel path, so that their accelerators are consistent.
;;;
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first with
;;; g_intern_static_string().
;;;
;;; action :
;;;     the action object
;;;
;;; accel_path :
;;;     the accelerator path
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_closure ()
;;;
;;; GClosure * gtk_action_get_accel_closure (GtkAction *action);
;;;
;;; Returns the accel closure for this action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     the accel closure for this action. The returned closure is owned by GTK+
;;;     and must not be unreffed or modified.
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_accel_group ()
;;;
;;; void gtk_action_set_accel_group (GtkAction *action,
;;;                                  GtkAccelGroup *accel_group);
;;;
;;; Sets the GtkAccelGroup in which the accelerator for this action will be
;;; installed.
;;;
;;; action :
;;;     the action object
;;;
;;; accel_group :
;;;     a GtkAccelGroup or NULL
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_label ()
;;;
;;; void gtk_action_set_label (GtkAction *action, const gchar *label);
;;;
;;; Sets the label of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; label :
;;;     the label text to set
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_label ()
;;;
;;; const gchar * gtk_action_get_label (GtkAction *action);
;;;
;;; Gets the label text of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     the label text
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_short_label ()
;;;
;;; void gtk_action_set_short_label (GtkAction *action,
;;;                                  const gchar *short_label);
;;;
;;; Sets a shorter label text on action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; short_label :
;;;     the label text to set
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_short_label ()
;;;
;;; const gchar * gtk_action_get_short_label (GtkAction *action);
;;;
;;; Gets the short label text of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     the short label text.
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_tooltip ()
;;;
;;; void gtk_action_set_tooltip (GtkAction *action, const gchar *tooltip);
;;;
;;; Sets the tooltip text on action
;;;
;;; action :
;;;     a GtkAction
;;;
;;; tooltip :
;;;     the tooltip text
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_tooltip ()
;;;
;;; const gchar * gtk_action_get_tooltip (GtkAction *action);
;;;
;;; Gets the tooltip text of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     the tooltip text
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_stock_id ()
;;;
;;; void gtk_action_set_stock_id (GtkAction *action, const gchar *stock_id);
;;;
;;; Sets the stock id on action
;;;
;;; action :
;;;     a GtkAction
;;;
;;; stock_id :
;;;     the stock id
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_stock_id ()
;;;
;;; const gchar * gtk_action_get_stock_id (GtkAction *action);
;;;
;;; Gets the stock id of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     the stock id
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_gicon ()
;;;
;;; void gtk_action_set_gicon (GtkAction *action, GIcon *icon);
;;;
;;; Sets the icon of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; icon :
;;;     the GIcon to set
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_gicon ()
;;;
;;; GIcon * gtk_action_get_gicon (GtkAction *action);
;;;
;;; Gets the gicon of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     The action's GIcon if one is set.
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_icon_name ()
;;;
;;; void gtk_action_set_icon_name (GtkAction *action, const gchar *icon_name);
;;;
;;; Sets the icon name on action
;;;
;;; action :
;;;     a GtkAction
;;;
;;; icon_name :
;;;     the icon name to set
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_icon_name ()
;;;
;;; const gchar * gtk_action_get_icon_name (GtkAction *action);
;;;
;;; Gets the icon name of action.
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     the icon name
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_visible_horizontal ()
;;;
;;; void gtk_action_set_visible_horizontal (GtkAction *action,
;;;                                         gboolean visible_horizontal);
;;;
;;; Sets whether action is visible when horizontal
;;;
;;; action :
;;;     a GtkAction
;;;
;;; visible_horizontal :
;;;     whether the action is visible horizontally
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_visible_horizontal ()
;;;
;;; gboolean gtk_action_get_visible_horizontal (GtkAction *action);
;;;
;;; Checks whether action is visible when horizontal
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     whether action is visible when horizontal
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_visible_vertical ()
;;;
;;; void gtk_action_set_visible_vertical (GtkAction *action,
;;;                                       gboolean visible_vertical);
;;;
;;; Sets whether action is visible when vertical
;;;
;;; action :
;;;     a GtkAction
;;;
;;; visible_vertical :
;;;     whether the action is visible vertically
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_visible_vertical ()
;;;
;;; gboolean gtk_action_get_visible_vertical (GtkAction *action);
;;;
;;; Checks whether action is visible when horizontal
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     whether action is visible when horizontal
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_is_important ()
;;;
;;; void gtk_action_set_is_important (GtkAction *action, gboolean is_important);
;;;
;;; Sets whether the action is important, this attribute is used primarily by
;;; toolbar items to decide whether to show a label or not.
;;;
;;; action :
;;;     the action object
;;;
;;; is_important :
;;;     TRUE to make the action important
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_is_important ()
;;;
;;; gboolean gtk_action_get_is_important (GtkAction *action);
;;;
;;; Checks whether action is important or not
;;;
;;; action :
;;;     a GtkAction
;;;
;;; Returns :
;;;     whether action is important
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.action.lisp --------------------------------------------
