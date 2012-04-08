;;; ----------------------------------------------------------------------------
;;; gtk.item-factory.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkItemFactory
;;; 
;;; A factory for menus
;;; 
;;; Synopsis
;;; 
;;;     GtkItemFactory
;;;     GtkItemFactoryEntry
;;;     GtkItemFactoryItem
;;;
;;;     gtk_item_factory_new
;;;     gtk_item_factory_construct
;;;     gtk_item_factory_add_foreign
;;;     gtk_item_factory_from_widget
;;;     gtk_item_factory_path_from_widget
;;;     gtk_item_factory_get_item
;;;     gtk_item_factory_get_widget
;;;     gtk_item_factory_get_widget_by_action
;;;     gtk_item_factory_get_item_by_action
;;;     gtk_item_factory_create_item
;;;     gtk_item_factory_create_items
;;;     gtk_item_factory_create_items_ac
;;;     gtk_item_factory_delete_item
;;;     gtk_item_factory_delete_entry
;;;     gtk_item_factory_delete_entries
;;;     gtk_item_factory_popup
;;;     gtk_item_factory_popup_with_data
;;;     gtk_item_factory_popup_data
;;;     gtk_item_factory_popup_data_from_widget
;;;     gtk_item_factory_from_path
;;;     gtk_item_factory_create_menu_entries
;;;     gtk_item_factories_path_delete
;;;     gtk_item_factory_set_translate_func
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkItemFactory
;;; 
;;; Description
;;; 
;;; As of GTK+ 2.4, GtkItemFactory has been deprecated in favour of
;;; GtkUIManager.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkItemFactory
;;; 
;;; struct GtkItemFactory;
;;; 
;;; Warning
;;; 
;;; GtkItemFactory is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkItemFactory" gtk-item-factory
  (:superclass gtk-object
   :export t
   :interfaces nil
   :type-initializer "gtk_item_factory_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkPrintFunc ()
;;; 
;;; void (*GtkPrintFunc) (gpointer func_data, const gchar *str);
;;; 
;;; Warning
;;; 
;;; GtkPrintFunc is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTranslateFunc ()
;;; 
;;; gchar * (*GtkTranslateFunc) (const gchar *path, gpointer func_data);
;;; 
;;; The function used to translate messages in e.g. GtkIconFactory and
;;; GtkActionGroup.
;;; 
;;; path :
;;;     The id of the message. In GtkItemFactory this will be a path from a
;;;     GtkItemFactoryEntry, in GtkActionGroup, it will be a label or tooltip
;;;     from a GtkActionEntry.
;;; 
;;; func_data :
;;;     user data passed in when registering the function
;;; 
;;; Returns :
;;;     the translated message
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkItemFactoryCallback ()
;;; 
;;; void (*GtkItemFactoryCallback) ();
;;; 
;;; Warning
;;; 
;;; GtkItemFactoryCallback is deprecated and should not be used in newly-written
;;; code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkItemFactoryCallback1 ()
;;; 
;;; void (*GtkItemFactoryCallback1) (gpointer callback_data,
;;;                                  guint callback_action,
;;;                                  GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; GtkItemFactoryCallback1 is deprecated and should not be used in
;;; newly-written code.
;;; ----------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------- 
;;; GtkItemFactoryCallback2 ()
;;; 
;;; void (*GtkItemFactoryCallback2) (GtkWidget *widget,
;;;                                  gpointer callback_data,
;;;                                  guint callback_action);
;;; 
;;; Warning
;;; 
;;; GtkItemFactoryCallback2 is deprecated and should not be used in
;;; newly-written code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkItemFactoryEntry
;;; 
;;; struct GtkItemFactoryEntry {
;;;   gchar *path;
;;;   gchar *accelerator;
;;; 
;;;   GtkItemFactoryCallback callback;
;;;   guint                  callback_action;
;;; 
;;;   /* possible values:
;;;    * NULL           -> "<Item>"
;;;    * ""             -> "<Item>"
;;;    * "<Title>"      -> create a title item
;;;    * "<Item>"       -> create a simple item
;;;    * "<ImageItem>"  -> create an item holding an image
;;;    * "<StockItem>"  -> create an item holding a stock image
;;;    * "<CheckItem>"  -> create a check item
;;;    * "<ToggleItem>" -> create a toggle item
;;;    * "<RadioItem>"  -> create a radio item
;;;    * <path>         -> path of a radio item to link against
;;;    * "<Separator>"  -> create a separator
;;;    * "<Tearoff>"    -> create a tearoff separator
;;;    * "<Branch>"     -> create an item to hold sub items
;;;    * "<LastBranch>" -> create a right justified item to hold sub items
;;;    */
;;;   gchar *item_type;
;;; 
;;;   /* Extra data for some item types:
;;;    *  ImageItem  -> pointer to inlined pixbuf stream
;;;    *  StockItem  -> name of stock item
;;;    */
;;;   gconstpointer extra_data;
;;; };
;;; 
;;; Warning
;;; 
;;; GtkItemFactoryEntry is deprecated and should not be used in newly-written
;;; code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkItemFactoryItem
;;; 
;;; struct GtkItemFactoryItem {
;;;   gchar *path;
;;;   GSList *widgets;
;;; };
;;; 
;;; Warning
;;; 
;;; GtkItemFactoryItem is deprecated and should not be used in newly-written
;;; code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_new ()
;;; 
;;; GtkItemFactory * gtk_item_factory_new (GType container_type,
;;;                                        const gchar *path,
;;;                                        GtkAccelGroup *accel_group);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_new has been deprecated since version 2.4 and should not
;;; be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Creates a new GtkItemFactory.
;;; 
;;; Beware that the returned object does not have a floating reference.
;;; 
;;; container_type :
;;;     the kind of menu to create; can be GTK_TYPE_MENU_BAR, GTK_TYPE_MENU or
;;;     GTK_TYPE_OPTION_MENU
;;; 
;;; path :
;;;     the factory path of the new item factory, a string of the form "<name>"
;;; 
;;; accel_group :
;;;     a GtkAccelGroup to which the accelerators for the menu items will be
;;;     added, or NULL to create a new one
;;; 
;;; Returns :
;;;     a new GtkItemFactory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_construct ()
;;; 
;;; void gtk_item_factory_construct (GtkItemFactory *ifactory,
;;;                                  GType container_type,
;;;                                  const gchar *path,
;;;                                  GtkAccelGroup *accel_group);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_construct has been deprecated since version 2.4 and should
;;; not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Initializes an item factory.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; container_type :
;;;     the kind of menu to create; can be GTK_TYPE_MENU_BAR, GTK_TYPE_MENU or
;;;     GTK_TYPE_OPTION_MENU
;;; 
;;; path :
;;;     the factory path of ifactory, a string of the form "<name>"
;;; 
;;; accel_group :
;;;     a GtkAccelGroup to which the accelerators for the menu items will be
;;;     added, or NULL to create a new one
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_add_foreign ()
;;; 
;;; void gtk_item_factory_add_foreign (GtkWidget *accel_widget,
;;;                                    const gchar *full_path,
;;;                                    GtkAccelGroup *accel_group,
;;;                                    guint keyval,
;;;                                    GdkModifierType modifiers);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_add_foreign has been deprecated since version 2.4 and
;;; should not be used in newly-written code. The recommended API for this
;;; purpose are the functions gtk_menu_item_set_accel_path() and
;;; gtk_widget_set_accel_path(); don't use gtk_item_factory_add_foreign() in
;;; new code, since it is likely to be removed in the future.
;;; 
;;; Installs an accelerator for accel_widget in accel_group, that causes the
;;; ::activate signal to be emitted if the accelerator is activated.
;;; 
;;; This function can be used to make widgets participate in the accel
;;; saving/restoring functionality provided by gtk_accel_map_save() and
;;; gtk_accel_map_load(), even if they haven't been created by an item factory.
;;; 
;;; accel_widget :
;;;     widget to install an accelerator on
;;; 
;;; full_path :
;;;     the full path for the accel_widget
;;; 
;;; accel_group :
;;;     the accelerator group to install the accelerator in
;;; 
;;; keyval :
;;;     key value of the accelerator
;;; 
;;; modifiers :
;;;     modifier combination of the accelerator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_from_widget ()
;;; 
;;; GtkItemFactory * gtk_item_factory_from_widget (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_from_widget has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the item factory from which a widget was created.
;;; 
;;; widget :
;;;     a widget
;;; 
;;; Returns :
;;;     the item factory from which widget was created, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_path_from_widget ()
;;; 
;;; const gchar * gtk_item_factory_path_from_widget (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_path_from_widget has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; If widget has been created by an item factory, returns the full path to it.
;;; (The full path of a widget is the concatenation of the factory path
;;; specified in gtk_item_factory_new() with the path specified in the
;;; GtkItemFactoryEntry from which the widget was created.)
;;; 
;;; widget :
;;;     a widget
;;; 
;;; Returns :
;;;     the full path to widget if it has been created by an item factory, NULL
;;; otherwise. This value is owned by GTK+ and must not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_get_item ()
;;; 
;;; GtkWidget * gtk_item_factory_get_item (GtkItemFactory *ifactory,
;;;                                        const gchar *path);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_get_item has been deprecated since version 2.4 and should
;;; not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the menu item which corresponds to path.
;;; 
;;; If the widget corresponding to path is a menu item which opens a submenu,
;;; then the item is returned. If you are interested in the submenu, use
;;; gtk_item_factory_get_widget() instead.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; path :
;;;     the path to the menu item
;;; 
;;; Returns :
;;;     the menu item for the given path, or NULL if path doesn't lead to a
;;;     menu item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_get_widget ()
;;; 
;;; GtkWidget * gtk_item_factory_get_widget (GtkItemFactory *ifactory,
;;;                                          const gchar *path);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_get_widget has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the widget which corresponds to path.
;;; 
;;; If the widget corresponding to path is a menu item which opens a submenu,
;;; then the submenu is returned. If you are interested in the menu item, use
;;; gtk_item_factory_get_item() instead.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; path :
;;;     the path to the widget
;;; 
;;; Returns :
;;;     the widget for the given path, or NULL if path doesn't lead to a widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_get_widget_by_action ()
;;; 
;;; GtkWidget * gtk_item_factory_get_widget_by_action (GtkItemFactory *ifactory,
;;;                                                    guint action);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_get_widget_by_action has been deprecated since version 2.4
;;; and should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the widget which was constructed from the GtkItemFactoryEntry with
;;; the given action.
;;; 
;;; If there are multiple items with the same action, the result is undefined.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; action :
;;;     an action as specified in the callback_action field of
;;;     GtkItemFactoryEntry
;;; 
;;; Returns :
;;;     the widget which corresponds to the given action, or NULL if no widget
;;;     was found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_get_item_by_action ()
;;; 
;;; GtkWidget * gtk_item_factory_get_item_by_action (GtkItemFactory *ifactory,
;;;                                                  guint action);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_get_item_by_action has been deprecated since version 2.4
;;; and should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the menu item which was constructed from the first
;;; GtkItemFactoryEntry with the given action.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; action :
;;;     an action as specified in the callback_action field of
;;;     GtkItemFactoryEntry
;;; 
;;; Returns :
;;;     the menu item which corresponds to the given action, or NULL if no menu
;;;     item was found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_create_item ()
;;; 
;;; void gtk_item_factory_create_item (GtkItemFactory *ifactory,
;;;                                    GtkItemFactoryEntry *entry,
;;;                                    gpointer callback_data,
;;;                                    guint callback_type);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_create_item has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Creates an item for entry.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; entry :
;;;     the GtkItemFactoryEntry to create an item for
;;; 
;;; callback_data :
;;;     data passed to the callback function of entry
;;; 
;;; callback_type :
;;;     1 if the callback function of entry is of type GtkItemFactoryCallback1,
;;;     2 if it is of type GtkItemFactoryCallback2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_create_items ()
;;; 
;;; void gtk_item_factory_create_items (GtkItemFactory *ifactory,
;;;                                     guint n_entries,
;;;                                     GtkItemFactoryEntry *entries,
;;;                                     gpointer callback_data);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_create_items has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Creates the menu items from the entries.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; n_entries :
;;;     the length of entries
;;; 
;;; entries :
;;;     an array of GtkItemFactoryEntrys whose callback members must by of type
;;;     GtkItemFactoryCallback1
;;; 
;;; callback_data :
;;;     data passed to the callback functions of all entries
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_create_items_ac ()
;;; 
;;; void gtk_item_factory_create_items_ac (GtkItemFactory *ifactory,
;;;                                        guint n_entries,
;;;                                        GtkItemFactoryEntry *entries,
;;;                                        gpointer callback_data,
;;;                                        guint callback_type);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_create_items_ac has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Creates the menu items from the entries.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; n_entries :
;;;     the length of entries
;;; 
;;; entries :
;;;     an array of GtkItemFactoryEntrys
;;; 
;;; callback_data :
;;;     data passed to the callback functions of all entries
;;; 
;;; callback_type :
;;;     1 if the callback functions in entries are of type
;;;       GtkItemFactoryCallback1,
;;;     2 if they are of type GtkItemFactoryCallback2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_delete_item ()
;;; 
;;; void gtk_item_factory_delete_item (GtkItemFactory *ifactory,
;;;                                    const gchar *path);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_delete_item has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Deletes the menu item which was created for path by the given item factory.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; path :
;;;     a path
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_delete_entry ()
;;; 
;;; void gtk_item_factory_delete_entry (GtkItemFactory *ifactory,
;;;                                     GtkItemFactoryEntry *entry);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_delete_entry has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Deletes the menu item which was created from entry by the given item
;;; factory.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; entry :
;;;     a GtkItemFactoryEntry
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_delete_entries ()
;;; 
;;; void gtk_item_factory_delete_entries (GtkItemFactory *ifactory,
;;;                                       guint n_entries,
;;;                                       GtkItemFactoryEntry *entries);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_delete_entries has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Deletes the menu items which were created from the entries by the given item
;;; factory.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; n_entries :
;;;     the length of entries
;;; 
;;; entries :
;;;     an array of GtkItemFactoryEntrys
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_popup ()
;;; 
;;; void gtk_item_factory_popup (GtkItemFactory *ifactory,
;;;                              guint x,
;;;                              guint y,
;;;                              guint mouse_button,
;;;                              guint32 time_);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_popup has been deprecated since version 2.4 and should not
;;; be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Pops up the menu constructed from the item factory at (x, y).
;;; 
;;; The mouse_button parameter should be the mouse button pressed to initiate
;;; the menu popup. If the menu popup was initiated by something other than a
;;; mouse button press, such as a mouse button release or a keypress,
;;; mouse_button should be 0.
;;; 
;;; The time_ parameter should be the time stamp of the event that initiated the
;;; popup. If such an event is not available, use gtk_get_current_event_time()
;;; instead.
;;; 
;;; The operation of the mouse_button and the time_ parameter is the same as the
;;; button and activation_time parameters for gtk_menu_popup().
;;; 
;;; ifactory :
;;;     a GtkItemFactory of type GTK_TYPE_MENU (see gtk_item_factory_new())
;;; 
;;; x :
;;;     the x position
;;; 
;;; y :
;;;     the y position
;;; 
;;; mouse_button :
;;;     the mouse button which was pressed to initiate the popup
;;; 
;;; time_ :
;;;     the time at which the activation event occurred
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_popup_with_data ()
;;; 
;;; void gtk_item_factory_popup_with_data (GtkItemFactory *ifactory,
;;;                                        gpointer popup_data,
;;;                                        GDestroyNotify destroy,
;;;                                        guint x,
;;;                                        guint y,
;;;                                        guint mouse_button,
;;;                                        guint32 time_);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_popup_with_data has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Pops up the menu constructed from the item factory at (x, y). Callbacks can
;;; access the popup_data while the menu is posted via
;;; gtk_item_factory_popup_data() and gtk_item_factory_popup_data_from_widget().
;;; 
;;; The mouse_button parameter should be the mouse button pressed to initiate
;;; the menu popup. If the menu popup was initiated by something other than a
;;; mouse button press, such as a mouse button release or a keypress,
;;; mouse_button should be 0.
;;; 
;;; The time_ parameter should be the time stamp of the event that initiated the
;;; popup. If such an event is not available, use gtk_get_current_event_time()
;;; instead.
;;; 
;;; The operation of the mouse_button and the time_ parameters is the same as
;;; the button and activation_time parameters for gtk_menu_popup().
;;; 
;;; ifactory :
;;;     a GtkItemFactory of type GTK_TYPE_MENU (see gtk_item_factory_new())
;;; 
;;; popup_data :
;;;     data available for callbacks while the menu is posted
;;; 
;;; destroy :
;;;     a GDestroyNotify function to be called on popup_data when the menu is
;;;     unposted
;;; 
;;; x :
;;;     the x position
;;; 
;;; y :
;;;     the y position
;;; 
;;; mouse_button :
;;;     the mouse button which was pressed to initiate the popup
;;; 
;;; time_ :
;;;     the time at which the activation event occurred
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_popup_data ()
;;; 
;;; gpointer gtk_item_factory_popup_data (GtkItemFactory *ifactory);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_popup_data has been deprecated since version 2.4 and should
;;; not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the popup_data which was passed to
;;; gtk_item_factory_popup_with_data(). This data is available until the menu is
;;; popped down again.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; Returns :
;;;     popup_data associated with ifactory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_popup_data_from_widget ()
;;; 
;;; gpointer gtk_item_factory_popup_data_from_widget (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_popup_data_from_widget has been deprecated since version
;;; 2.4 and should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Obtains the popup_data which was passed to
;;; gtk_item_factory_popup_with_data(). This data is available until the menu is
;;; popped down again.
;;; 
;;; widget :
;;;     a widget
;;; 
;;; Returns :
;;;     popup_data associated with the item factory from which widget was
;;;     created, or NULL if widget wasn't created by an item factory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_from_path ()
;;; 
;;; GtkItemFactory * gtk_item_factory_from_path (const gchar *path);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_from_path has been deprecated since version 2.4 and should
;;; not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Finds an item factory which has been constructed using the "<name>" prefix
;;; of path as the path argument for gtk_item_factory_new().
;;; 
;;; path :
;;;     a string starting with a factory path of the form "<name>"
;;; 
;;; Returns :
;;;     the GtkItemFactory created for the given factory path, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_create_menu_entries ()
;;; 
;;; void gtk_item_factory_create_menu_entries (guint n_entries,
;;;                                            GtkMenuEntry *entries);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_create_menu_entries has been deprecated since version 2.4
;;; and should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Creates the menu items from the entries.
;;; 
;;; n_entries :
;;;     the length of entries
;;; 
;;; entries :
;;;     an array of GtkMenuEntrys
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factories_path_delete ()
;;; 
;;; void gtk_item_factories_path_delete (const gchar *ifactory_path,
;;;                                      const gchar *path);
;;; 
;;; Warning
;;; 
;;; gtk_item_factories_path_delete has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Deletes all widgets constructed from the specified path.
;;; 
;;; ifactory_path :
;;;     a factory path to prepend to path. May be NULL if path starts with a
;;;     factory path
;;; 
;;; path :
;;;     a path
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_item_factory_set_translate_func ()
;;; 
;;; void gtk_item_factory_set_translate_func (GtkItemFactory *ifactory,
;;;                                           GtkTranslateFunc func,
;;;                                           gpointer data,
;;;                                           GDestroyNotify notify);
;;; 
;;; Warning
;;; 
;;; gtk_item_factory_set_translate_func has been deprecated since version 2.4
;;; and should not be used in newly-written code. Use GtkUIManager instead.
;;; 
;;; Sets a function to be used for translating the path elements before they
;;; are displayed.
;;; 
;;; ifactory :
;;;     a GtkItemFactory
;;; 
;;; func :
;;;     the GtkTranslateFunc function to be used to translate path elements
;;; 
;;; data :
;;;     data to pass to func and notify
;;; 
;;; notify :
;;;     a GDestroyNotify function to be called when ifactory is destroyed and
;;;     when the translation function is changed again
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.item-factory.lisp --------------------------------------
