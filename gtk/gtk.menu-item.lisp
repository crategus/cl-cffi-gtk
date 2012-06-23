;;; ----------------------------------------------------------------------------
;;; gtk.menu-item.lisp
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
;;; GtkMenuItem
;;;
;;; The widget used for item in menus
;;;
;;; Synopsis
;;;
;;;     GtkMenuItem
;;;
;;;     gtk_menu_item_new
;;;     gtk_menu_item_new_with_label
;;;     gtk_menu_item_new_with_mnemonic
;;;     gtk_menu_item_set_right_justified
;;;     gtk_menu_item_get_right_justified
;;;     gtk_menu_item_get_label
;;;     gtk_menu_item_set_label
;;;     gtk_menu_item_get_use_underline
;;;     gtk_menu_item_set_use_underline
;;;     gtk_menu_item_set_submenu
;;;     gtk_menu_item_get_submenu
;;;     gtk_menu_item_set_accel_path
;;;     gtk_menu_item_get_accel_path
;;;     gtk_menu_item_select
;;;     gtk_menu_item_deselect
;;;     gtk_menu_item_activate
;;;     gtk_menu_item_toggle_size_request
;;;     gtk_menu_item_toggle_size_allocate
;;;     gtk_menu_item_get_reserve_indicator
;;;     gtk_menu_item_set_reserve_indicator
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkMenuItem
;;;                                  +----GtkCheckMenuItem
;;;                                  +----GtkImageMenuItem
;;;                                  +----GtkSeparatorMenuItem
;;;                                  +----GtkTearoffMenuItem
;;;
;;; Implemented Interfaces
;;;
;;; GtkMenuItem implements AtkImplementorIface, GtkBuildable and GtkActivatable.
;;;
;;; Properties
;;;
;;;   "accel-path"               gchar*                : Read / Write
;;;   "label"                    gchar*                : Read / Write
;;;   "right-justified"          gboolean              : Read / Write
;;;   "submenu"                  GtkMenu*              : Read / Write
;;;   "use-underline"            gboolean              : Read / Write
;;;
;;; Style Properties
;;;
;;;   "arrow-scaling"            gfloat                : Read
;;;   "arrow-spacing"            gint                  : Read
;;;   "horizontal-padding"       gint                  : Read
;;;   "selected-shadow-type"     GtkShadowType         : Read
;;;   "toggle-spacing"           gint                  : Read
;;;   "width-chars"              gint                  : Read
;;;
;;; Signals
;;;
;;;   "activate"                                       : Action
;;;   "activate-item"                                  : Run First
;;;   "deselect"                                       : Run First
;;;   "select"                                         : Run First
;;;   "toggle-size-allocate"                           : Run First
;;;   "toggle-size-request"                            : Run First
;;;
;;; Description
;;;
;;; The GtkMenuItem widget and the derived widgets are the only valid childs for
;;; menus. Their function is to correctly handle highlighting, alignment, events
;;; and submenus.
;;;
;;; As it derives from GtkBin it can hold any valid child widget, altough only a
;;; few are really useful.
;;;
;;; GtkMenuItem as GtkBuildable
;;;
;;; The GtkMenuItem implementation of the GtkBuildable interface supports adding
;;; a submenu by specifying "submenu" as the "type" attribute of a <child>
;;; element.
;;;
;;; Example 77. A UI definition fragment with submenus
;;;
;;; <object class="GtkMenuItem">
;;;   <child type="submenu">
;;;     <object class="GtkMenu"/>
;;;   </child>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-path" property
;;;
;;;   "accel-path"               gchar*                : Read / Write
;;;
;;; Sets the accelerator path of the menu item, through which runtime changes of
;;; the menu item's accelerator caused by the user can be identified and saved
;;; to persistant storage.
;;;
;;; Default value: NULL
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;;
;;;   "label"                    gchar*                : Read / Write
;;;
;;; The text for the child label.
;;;
;;; Default value: ""
;;;
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "right-justified" property
;;;
;;;   "right-justified"          gboolean              : Read / Write
;;;
;;; Sets whether the menu item appears justified at the right side of a menu
;;; bar.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "submenu" property
;;;
;;;   "submenu"                  GtkMenu*              : Read / Write
;;;
;;; The submenu attached to the menu item, or NULL if it has none.
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-underline" property
;;;
;;;   "use-underline"            gboolean              : Read / Write
;;;
;;; TRUE if underlines in the text indicate mnemonics.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-scaling" style property
;;;
;;;   "arrow-scaling"            gfloat                : Read
;;;
;;; Amount of space used up by arrow, relative to the menu item's font size.
;;;
;;; Allowed values: [0,2]
;;;
;;; Default value: 0.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-spacing" style property
;;;
;;;   "arrow-spacing"            gint                  : Read
;;;
;;; Space between label and arrow.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 10
;;;
;;; ----------------------------------------------------------------------------
;;; The "horizontal-padding" style property
;;;
;;;   "horizontal-padding"       gint                  : Read
;;;
;;; Padding to left and right of the menu item.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 3
;;;
;;; ----------------------------------------------------------------------------
;;; The "selected-shadow-type" style property
;;;
;;;   "selected-shadow-type"     GtkShadowType         : Read
;;;
;;; Shadow type when item is selected.
;;;
;;; Default value: GTK_SHADOW_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-spacing" style property
;;;
;;;   "toggle-spacing"           gint                  : Read
;;;
;;; Space between icon and label.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 5
;;;
;;; ----------------------------------------------------------------------------
;;; The "width-chars" style property
;;;
;;;   "width-chars"              gint                  : Read
;;;
;;; The minimum desired width of the menu item in characters.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 12
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;;
;;; void user_function (GtkMenuItem *menuitem,
;;;                     gpointer     user_data)      : Action
;;;
;;; Emitted when the item is activated.
;;;
;;; menuitem :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-item" signal
;;;
;;; void user_function (GtkMenuItem *menuitem,
;;;                     gpointer     user_data)      : Run First
;;;
;;; Emitted when the item is activated, but also if the menu item has a submenu.
;;; For normal applications, the relevant signal is "activate".
;;;
;;; menuitem :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "deselect" signal
;;;
;;; void user_function (GtkMenuItem *menuitem,
;;;                     gpointer     user_data)      : Run First
;;;
;;; ----------------------------------------------------------------------------
;;; The "select" signal
;;;
;;; void user_function (GtkMenuItem *menuitem,
;;;                     gpointer     user_data)      : Run First
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-size-allocate" signal
;;;
;;; void user_function (GtkMenuItem *menuitem,
;;;                     gint         arg1,
;;;                     gpointer     user_data)      : Run First
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-size-request" signal
;;;
;;; void user_function (GtkMenuItem *menuitem,
;;;                     gpointer     arg1,
;;;                     gpointer     user_data)      : Run First
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuItem
;;;
;;; struct GtkMenuItem;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuItem" gtk-menu-item
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_menu_item_get_type")
  ((accel-path
    gtk-menu-item-accel-path
    "accel-path" "gchararray" t t)
   (label
    gtk-menu-item-label "label" "gchararray" t t)
   (right-justified
    gtk-menu-item-right-justified
    "right-justified" "gboolean" t t)
   (submenu
    gtk-menu-item-submenu
    "submenu" "GtkMenu" t t)
   (use-underline
    gtk-menu-item-use-underline
    "use-underline" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new ()
;;;
;;; GtkWidget * gtk_menu_item_new (void);
;;;
;;; Creates a new GtkMenuItem.
;;;
;;; Returns :
;;;     the newly created GtkMenuItem
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-new))

(defun gtk-menu-item-new ()
  (make-instance 'gtk-menu-item))

(export 'gt-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_label ()
;;;
;;; GtkWidget * gtk_menu_item_new_with_label (const gchar *label);
;;;
;;; Creates a new GtkMenuItem whose child is a GtkLabel.
;;;
;;; label :
;;;     the text for the label
;;;
;;; Returns :
;;;     the newly created GtkMenuItem
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-new-with-label))

(defun gtk-menu-item-new-with-label (label)
  (make-instance 'gtk-menu-item
                 :label label))

(export 'gtk-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_mnemonic ()
;;;
;;; GtkWidget * gtk_menu_item_new_with_mnemonic (const gchar *label);
;;;
;;; Creates a new GtkMenuItem containing a label.
;;;
;;; The label will be created using gtk_label_new_with_mnemonic(), so
;;; underscores in label indicate the mnemonic for the menu item.
;;;
;;; label :
;;;     The text of the button, with an underscore in front of the mnemonic
;;;     character
;;;
;;; Returns :
;;;     a new GtkMenuItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_right_justified ()
;;;
;;; void gtk_menu_item_set_right_justified (GtkMenuItem *menu_item,
;;;                                         gboolean right_justified);
;;;
;;; Warning
;;;
;;; gtk_menu_item_set_right_justified has been deprecated since version 3.2 and
;;; should not be used in newly-written code. If you insist on using it, use
;;; gtk_widget_set_hexpand() and gtk_widget_set_halign().
;;;
;;; Sets whether the menu item appears justified at the right side of a menu
;;; bar. This was traditionally done for "Help" menu items, but is now
;;; considered a bad idea. (If the widget layout is reversed for a right-to-left
;;; language like Hebrew or Arabic, right-justified-menu-items appear at the
;;; left.)
;;;
;;; menu_item :
;;;     a GtkMenuItem.
;;;
;;; right_justified :
;;;     if TRUE the menu item will appear at the far right if added to a menu
;;;     bar
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_right_justified ()
;;;
;;; gboolean gtk_menu_item_get_right_justified (GtkMenuItem *menu_item);
;;;
;;; Warning
;;;
;;; gtk_menu_item_get_right_justified has been deprecated since version 3.2 and
;;; should not be used in newly-written code. See
;;; gtk_menu_item_set_right_justified()
;;;
;;; Gets whether the menu item appears justified at the right side of the menu
;;; bar.
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; Returns :
;;;     TRUE if the menu item will appear at the far right if added to a menu
;;;     bar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_label ()
;;;
;;; const gchar * gtk_menu_item_get_label (GtkMenuItem *menu_item);
;;;
;;; Sets text on the menu_item label
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; Returns :
;;;     The text in the menu_item label. This is the internal string used by the
;;;     label, and must not be modified.
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_label ()
;;;
;;; void gtk_menu_item_set_label (GtkMenuItem *menu_item, const gchar *label);
;;;
;;; Sets text on the menu_item label
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; label :
;;;     the text you want to set
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_use_underline ()
;;;
;;; gboolean gtk_menu_item_get_use_underline (GtkMenuItem *menu_item);
;;;
;;; Checks if an underline in the text indicates the next character should be
;;; used for the mnemonic accelerator key.
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; Returns :
;;;     TRUE if an embedded underline in the label indicates the mnemonic
;;;     accelerator key.
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_use_underline ()
;;;
;;; void gtk_menu_item_set_use_underline (GtkMenuItem *menu_item,
;;;                                       gboolean setting);
;;;
;;; If true, an underline in the text indicates the next character should be
;;; used for the mnemonic accelerator key.
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; setting :
;;;     TRUE if underlines in the text indicate mnemonics
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_submenu ()
;;;
;;; void gtk_menu_item_set_submenu (GtkMenuItem *menu_item, GtkWidget *submenu);
;;;
;;; Sets or replaces the menu item's submenu, or removes it when a NULL submenu
;;; is passed.
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; submenu :
;;;     the submenu, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-set-submenu))

(defun gtk-menu-item-set-submenu (menu-item submenu)
  (setf (gtk-menu-item-submenu menu-item) submenu))

(export 'gtk-menu-item-set-submenu)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_submenu ()
;;;
;;; GtkWidget * gtk_menu_item_get_submenu (GtkMenuItem *menu_item);
;;;
;;; Gets the submenu underneath this menu item, if any. See
;;; gtk_menu_item_set_submenu().
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; Returns :
;;;     submenu for this menu item, or NULL if none
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-get-submenu))

(defun gtk-menu-item-get-submenu (menu-item)
  (gtk-menu-item-submenu menu-item))

(export 'gtk-menu-item-get-submenu)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_accel_path ()
;;;
;;; void gtk_menu_item_set_accel_path (GtkMenuItem *menu_item,
;;;                                    const gchar *accel_path);
;;;
;;; Set the accelerator path on menu_item, through which runtime changes of the
;;; menu item's accelerator caused by the user can be identified and saved to
;;; persistent storage (see gtk_accel_map_save() on this). To set up a default
;;; accelerator for this menu item, call gtk_accel_map_add_entry() with the same
;;; accel_path. See also gtk_accel_map_add_entry() on the specifics of
;;; accelerator paths, and gtk_menu_set_accel_path() for a more convenient
;;; variant of this function.
;;;
;;; This function is basically a convenience wrapper that handles calling
;;; gtk_widget_set_accel_path() with the appropriate accelerator group for the
;;; menu item.
;;;
;;; Note that you do need to set an accelerator on the parent menu with
;;; gtk_menu_set_accel_group() for this to work.
;;;
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first with
;;; g_intern_static_string().
;;;
;;; menu_item :
;;;     a valid GtkMenuItem
;;;
;;; accel_path :
;;;     accelerator path, corresponding to this menu item's functionality, or
;;;     NULL to unset the current path
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_accel_path ()
;;;
;;; const gchar * gtk_menu_item_get_accel_path (GtkMenuItem *menu_item);
;;;
;;; Retrieve the accelerator path that was previously set on menu_item.
;;;
;;; See gtk_menu_item_set_accel_path() for details.
;;;
;;; menu_item :
;;;     a valid GtkMenuItem
;;;
;;; Returns :
;;;     the accelerator path corresponding to this menu item's functionality, or
;;;     NULL if not set
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_select ()
;;;
;;; void gtk_menu_item_select (GtkMenuItem *menu_item);
;;;
;;; Emits the "select" signal on the given item. Behaves exactly like
;;; gtk_item_select.
;;;
;;; menu_item :
;;;     the menu item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_deselect ()
;;;
;;; void gtk_menu_item_deselect (GtkMenuItem *menu_item);
;;;
;;; Emits the "deselect" signal on the given item. Behaves exactly like
;;; gtk_item_deselect.
;;;
;;; menu_item :
;;;     the menu item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_activate ()
;;;
;;; void gtk_menu_item_activate (GtkMenuItem *menu_item);
;;;
;;; Emits the "activate" signal on the given item
;;;
;;; menu_item :
;;;     the menu item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_request ()
;;;
;;; void gtk_menu_item_toggle_size_request (GtkMenuItem *menu_item,
;;;                                         gint *requisition);
;;;
;;; Emits the "toggle-size-request" signal on the given item.
;;;
;;; menu_item :
;;;     the menu item
;;;
;;; requisition :
;;;     the requisition to use as signal data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_allocate ()
;;;
;;; void gtk_menu_item_toggle_size_allocate (GtkMenuItem *menu_item,
;;;                                          gint allocation);
;;;
;;; Emits the "toggle-size-allocate" signal on the given item.
;;;
;;; menu_item :
;;;     the menu item.
;;;
;;; allocation :
;;;     the allocation to use as signal data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_reserve_indicator ()
;;;
;;; gboolean gtk_menu_item_get_reserve_indicator (GtkMenuItem *menu_item);
;;;
;;; Returns whether the menu_item reserves space for the submenu indicator,
;;; regardless if it has a submenu or not.
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; Returns :
;;;     TRUE if menu_item always reserves space for the submenu indicator
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_reserve_indicator ()
;;;
;;; void gtk_menu_item_set_reserve_indicator (GtkMenuItem *menu_item,
;;;                                           gboolean reserve);
;;;
;;; Sets whether the menu_item should reserve space for the submenu indicator,
;;; regardless if it actually has a submenu or not.
;;;
;;; There should be little need for applications to call this functions.
;;;
;;; menu_item :
;;;     a GtkMenuItem
;;;
;;; reserve :
;;;     the new value
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.menu-item.lisp -----------------------------------------
