;;; ----------------------------------------------------------------------------
;;; gio.menu.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;; GMenu
;;;
;;;     A simple implementation of GMenuModel
;;;
;;; Types and Values
;;;
;;;     GMenu
;;;     GMenuItem
;;;
;;; Functions
;;;
;;;     g_menu_new
;;;     g_menu_freeze
;;;     g_menu_insert
;;;     g_menu_prepend
;;;     g_menu_append
;;;     g_menu_insert_item
;;;     g_menu_append_item
;;;     g_menu_prepend_item
;;;     g_menu_insert_section
;;;     g_menu_prepend_section
;;;     g_menu_append_section
;;;     g_menu_append_submenu
;;;     g_menu_insert_submenu
;;;     g_menu_prepend_submenu
;;;     g_menu_remove
;;;     g_menu_remove_all
;;;
;;;
;;;     g_menu_item_new
;;;     g_menu_item_new_section
;;;     g_menu_item_new_submenu
;;;     g_menu_item_new_from_model
;;;     g_menu_item_set_label
;;;     g_menu_item_set_icon
;;;     g_menu_item_set_action_and_target_value
;;;     g_menu_item_set_action_and_target
;;;     g_menu_item_set_detailed_action
;;;     g_menu_item_set_section
;;;     g_menu_item_set_submenu
;;;
;;;     g_menu_item_get_attribute_value
;;;     g_menu_item_get_attribute
;;;     g_menu_item_get_link
;;;     g_menu_item_set_attribute_value
;;;     g_menu_item_set_attribute
;;;     g_menu_item_set_link
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GMenuItem
;;;     ╰── GMenuModel
;;;         ╰── GMenu
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GMenu
;;; ----------------------------------------------------------------------------

(define-g-object-class "GMenu" g-menu
  (:superclass g-menu-model
   :export t
   :interfaces nil
   :type-initializer "g_menu_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-menu 'type)
 "@version{2021-4-15}
  @begin{short}
    The @sym{g-menu} object is a simple implementation of @class{g-menu-model}.
  @end{short}
  You populate a @sym{g-menu} object by adding @class{g-menu-item} objects to
  it.

  There are some convenience functions to allow you to directly add items,
  avoiding a @class{g-menu-item} object, for the common cases. To add a regular
  item, use the function @fun{g-menu-insert}. To add a section, use the function
  @fun{g-menu-insert-section}. To add a submenu, use the function
  @fun{g-menu-insert-submenu}.
  @see-class{g-menu-model}
  @see-class{g-menu-item}")

;;; ----------------------------------------------------------------------------
;;; g_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-menu-new))

(defun g-menu-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @return{A new @class{g-menu} object.}
  @short{Creates a new @class{g-menu} object.}
  The new menu has no items.
  @see-class{g-menu}"
  (make-instance 'g-menu))

(export 'g-menu-new)

;;; ----------------------------------------------------------------------------
;;; g_menu_freeze ()
;;;
;;; void g_menu_freeze (GMenu *menu);
;;;
;;; Marks menu as frozen.
;;;
;;; After the menu is frozen, it is an error to attempt to make any changes to
;;; it. In effect this means that the GMenu API must no longer be used.
;;;
;;; This function causes g_menu_model_is_mutable() to begin returning FALSE,
;;; which has some positive performance implications.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_insert ()
;;;
;;; void g_menu_insert (GMenu *menu,
;;;                     gint position,
;;;                     const gchar *label,
;;;                     const gchar *detailed_action);
;;;
;;; Convenience function for inserting a normal menu item into menu. Combine
;;; g_menu_item_new() and g_menu_insert_item() for a more flexible alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; position :
;;;     the position at which to insert the item
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; detailed_action :
;;;     the detailed action string, or NULL. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend ()
;;;
;;; void g_menu_prepend (GMenu *menu,
;;;                      const gchar *label,
;;;                      const gchar *detailed_action);
;;;
;;; Convenience function for prepending a normal menu item to the start of menu.
;;; Combine g_menu_item_new() and g_menu_insert_item() for a more flexible
;;; alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; detailed_action :
;;;     the detailed action string, or NULL. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_append ()
;;;
;;; void g_menu_append (GMenu *menu,
;;;                     const gchar *label,
;;;                     const gchar *detailed_action);
;;;
;;; Convenience function for appending a normal menu item to the end of menu.
;;; Combine g_menu_item_new() and g_menu_insert_item() for a more flexible
;;; alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; detailed_action :
;;;     the detailed action string, or NULL. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_item ()
;;;
;;; void g_menu_insert_item (GMenu *menu, gint position, GMenuItem *item);
;;;
;;; Inserts item into menu.
;;;
;;; The "insertion" is actually done by copying all of the attribute and link
;;; values of item and using them to form a new item within menu. As such, item
;;; itself is not really inserted, but rather, a menu item that is exactly the
;;; same as the one presently described by item.
;;;
;;; This means that item is essentially useless after the insertion occurs. Any
;;; changes you make to it are ignored unless it is inserted again (at which
;;; point its updated values will be copied).
;;;
;;; You should probably just free item once you're done.
;;;
;;; There are many convenience functions to take care of common cases. See
;;; g_menu_insert(), g_menu_insert_section() and g_menu_insert_submenu() as
;;; well as "prepend" and "append" variants of each of these functions.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; position :
;;;     the position at which to insert the item
;;;
;;; item :
;;;     the GMenuItem to insert
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_append_item ()
;;;
;;; void g_menu_append_item (GMenu *menu, GMenuItem *item);
;;;
;;; Appends item to the end of menu.
;;;
;;; See g_menu_insert_item() for more information.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; item :
;;;     a GMenuItem to append
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_item ()
;;;
;;; void g_menu_prepend_item (GMenu *menu, GMenuItem *item);
;;;
;;; Prepends item to the start of menu.
;;;
;;; See g_menu_insert_item() for more information.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; item :
;;;     a GMenuItem to prepend
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_insert_section" g-menu-insert-section) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[menu]{a @class{g-menu} object}
  @argument[position]{an integer with the position at which to insert the item}
  @argument[label]{a string with the section label, or @code{nil}}
  @argument[section]{a @class{g-menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for inserting a section menu item into menu.
  @end{short}
  Combine the functions @fun{g-menu-item-new-section} and
  @fun{g-menu-insert-item} for a more flexible alternative.
  @see-class{g-menu}
  @see-class{g-menu-model}
  @see-function{g-menu-item-new-section}
  @see-function{g-menu-insert-item}"
  (menu (g-object g-menu))
  (position :int)
  (label :string)
  (section (g-object g-menu-model)))

(export 'g-menu-insert-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_section ()
;;;
;;; void g_menu_prepend_section (GMenu *menu,
;;;                              const gchar *label,
;;;                              GMenuModel *section);
;;;
;;; Convenience function for prepending a section menu item to the start of
;;; menu. Combine g_menu_item_new_section() and g_menu_insert_item() for a more
;;; flexible alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; section :
;;;     a GMenuModel with the items of the section
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_append_section ()
;;;
;;; void g_menu_append_section (GMenu *menu,
;;;                             const gchar *label,
;;;                             GMenuModel *section);
;;;
;;; Convenience function for appending a section menu item to the end of menu.
;;; Combine g_menu_item_new_section() and g_menu_insert_item() for a more
;;; flexible alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; section :
;;;     a GMenuModel with the items of the section
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_append_submenu ()
;;;
;;; void g_menu_append_submenu (GMenu *menu,
;;;                             const gchar *label,
;;;                             GMenuModel *submenu);
;;;
;;; Convenience function for appending a submenu menu item to the end of menu.
;;; Combine g_menu_item_new_submenu() and g_menu_insert_item() for a more
;;; flexible alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; submenu :
;;;     a GMenuModel with the items of the submenu
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_submenu ()
;;;
;;; void g_menu_insert_submenu (GMenu *menu,
;;;                             gint position,
;;;                             const gchar *label,
;;;                             GMenuModel *submenu);
;;;
;;; Convenience function for inserting a submenu menu item into menu. Combine
;;; g_menu_item_new_submenu() and g_menu_insert_item() for a more flexible
;;; alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; position :
;;;     the position at which to insert the item
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; submenu :
;;;     a GMenuModel with the items of the submenu
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_submenu ()
;;;
;;; void g_menu_prepend_submenu (GMenu *menu,
;;;                              const gchar *label,
;;;                              GMenuModel *submenu);
;;;
;;; Convenience function for prepending a submenu menu item to the start of
;;; menu. Combine g_menu_item_new_submenu() and g_menu_insert_item() for a more
;;; flexible alternative.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; submenu :
;;;     a GMenuModel with the items of the submenu
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_remove ()
;;;
;;; void g_menu_remove (GMenu *menu, gint position);
;;;
;;; Removes an item from the menu.
;;;
;;; position gives the index of the item to remove.
;;;
;;; It is an error if position is not in range the range from 0 to one less
;;; than the number of items in the menu.
;;;
;;; It is not possible to remove items by identity since items are added to the
;;; menu simply by copying their links and attributes (ie: identity of the item
;;; itself is not preserved).
;;;
;;; menu :
;;;     a GMenu
;;;
;;; position :
;;;     the position of the item to remove
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_remove_all ()
;;;
;;; void g_menu_remove_all (GMenu *menu);
;;;
;;; Removes all items in the menu.
;;;
;;; menu :
;;;     a GMenu
;;;
;;; Since 2.38
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GMenuItem" g-menu-item
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_menu_item_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-menu-item 'type)
 "@version{2021-4-15}
  @begin{short}
    The @sym{g-menu-item} object is an opaque structure type.
  @end{short}
  You must access it using the functions below.
  @see-class{g-menu}")

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new ()
;;;
;;; GMenuItem * g_menu_item_new (const gchar *label,
;;;                              const gchar *detailed_action);
;;;
;;; Creates a new GMenuItem.
;;;
;;; If label is non-NULL it is used to set the "label" attribute of the new
;;; item.
;;;
;;; If detailed_action is non-NULL it is used to set the "action" and possibly
;;; the "target" attribute of the new item. See
;;; g_menu_item_set_detailed_action() for more information.
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; detailed_action :
;;;     the detailed action string, or NULL. [allow-none]
;;;
;;; Returns :
;;;     a new GMenuItem
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_section ()
;;;
;;; GMenuItem * g_menu_item_new_section (const gchar *label,
;;;                                      GMenuModel *section);
;;;
;;; Creates a new GMenuItem representing a section.
;;;
;;; This is a convenience API around g_menu_item_new() and
;;; g_menu_item_set_section().
;;;
;;; The effect of having one menu appear as a section of another is exactly as
;;; it sounds: the items from section become a direct part of the menu that
;;; menu_item is added to.
;;;
;;; Visual separation is typically displayed between two non-empty sections. If
;;; label is non-NULL then it will be encorporated into this visual indication.
;;; This allows for labeled subsections of a menu.
;;;
;;; As a simple example, consider a typical "Edit" menu from a simple program.
;;; It probably contains an "Undo" and "Redo" item, followed by a separator,
;;; followed by "Cut", "Copy" and "Paste".
;;;
;;; This would be accomplished by creating three GMenu instances. The first
;;; would be populated with the "Undo" and "Redo" items, and the second with
;;; the "Cut", "Copy" and "Paste" items. The first and second menus would then
;;; be added as submenus of the third. In XML format, this would look something
;;; like the following:
;;;
;;; <menu id='edit-menu'>
;;;   <section>
;;;     <item label='Undo'/>
;;;     <item label='Redo'/>
;;;   </section>
;;;   <section>
;;;     <item label='Cut'/>
;;;     <item label='Copy'/>
;;;     <item label='Paste'/>
;;;   </section>
;;; </menu>
;;;
;;; The following example is exactly equivalent. It is more illustrative of the
;;; exact relationship between the menus and items (keeping in mind that the
;;; 'link' element defines a new menu that is linked to the containing one).
;;; The style of the second example is more verbose and difficult to read (and
;;; therefore not recommended except for the purpose of understanding what is
;;; really going on).
;;;
;;; <menu id='edit-menu'>
;;;   <item>
;;;     <link name='section'>
;;;       <item label='Undo'/>
;;;       <item label='Redo'/>
;;;     </link>
;;;   </item>
;;;   <item>
;;;     <link name='section'>
;;;       <item label='Cut'/>
;;;       <item label='Copy'/>
;;;       <item label='Paste'/>
;;;     </link>
;;;   </item>
;;; </menu>
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; section :
;;;     a GMenuModel with the items of the section
;;;
;;; Returns :
;;;     a new GMenuItem
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_submenu ()
;;;
;;; GMenuItem * g_menu_item_new_submenu (const gchar *label,
;;;                                      GMenuModel *submenu);
;;;
;;; Creates a new GMenuItem representing a submenu.
;;;
;;; This is a convenience API around g_menu_item_new() and
;;; g_menu_item_set_submenu().
;;;
;;; label :
;;;     the section label, or NULL. [allow-none]
;;;
;;; submenu :
;;;     a GMenuModel with the items of the submenu
;;;
;;; Returns :
;;;     a new GMenuItem
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_from_model ()
;;;
;;; GMenuItem * g_menu_item_new_from_model (GMenuModel *model,
;;;                                         gint item_index);
;;;
;;; Creates a GMenuItem as an exact copy of an existing menu item in a
;;; GMenuModel.
;;;
;;; item_index must be valid (ie: be sure to call g_menu_model_get_n_items()
;;; first).
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; item_index :
;;;     the index of an item in model
;;;
;;; Returns :
;;;     a new GMenuItem.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_label ()
;;;
;;; void g_menu_item_set_label (GMenuItem *menu_item, const gchar *label);
;;;
;;; Sets or unsets the "label" attribute of menu_item.
;;;
;;; If label is non-NULL it is used as the label for the menu item. If it is
;;; NULL then the label attribute is unset.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; label :
;;;     the label to set, or NULL to unset. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_icon ()
;;;
;;; void g_menu_item_set_icon (GMenuItem *menu_item, GIcon *icon);
;;;
;;; Sets (or unsets) the icon on menu_item.
;;;
;;; This call is the same as calling g_icon_serialize() and using the result as
;;; the value to g_menu_item_set_attribute_value() for G_MENU_ATTRIBUTE_ICON.
;;;
;;; This API is only intended for use with "noun" menu items; things like
;;; bookmarks or applications in an "Open With" menu. Don't use it on menu
;;; items corresponding to verbs (eg: stock icons for 'Save' or 'Quit').
;;;
;;; If icon is NULL then the icon is unset.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; icon :
;;;     a GIcon, or NULL
;;;
;;; Since 2.38
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_action_and_target_value ()
;;;
;;; void g_menu_item_set_action_and_target_value (GMenuItem *menu_item,
;;;                                               const gchar *action,
;;;                                               GVariant *target_value);
;;;
;;; Sets or unsets the "action" and "target" attributes of menu_item.
;;;
;;; If action is NULL then both the "action" and "target" attributes are unset
;;; (and target_value is ignored).
;;;
;;; If action is non-NULL then the "action" attribute is set. The "target"
;;; attribute is then set to the value of target_value if it is non-NULL or
;;; unset otherwise.
;;;
;;; Normal menu items (ie: not submenu, section or other custom item types) are
;;; expected to have the "action" attribute set to identify the action that they
;;; are associated with. The state type of the action help to determine the
;;; disposition of the menu item. See GAction and GActionGroup for an overview
;;; of actions.
;;;
;;; In general, clicking on the menu item will result in activation of the named
;;; action with the "target" attribute given as the parameter to the action
;;; invocation. If the "target" attribute is not set then the action is invoked
;;; with no parameter.
;;;
;;; If the action has no state then the menu item is usually drawn as a plain
;;; menu item (ie: with no additional decoration).
;;;
;;; If the action has a boolean state then the menu item is usually drawn as a
;;; toggle menu item (ie: with a checkmark or equivalent indication). The item
;;; should be marked as 'toggled' or 'checked' when the boolean state is TRUE.
;;;
;;; If the action has a string state then the menu item is usually drawn as a
;;; radio menu item (ie: with a radio bullet or equivalent indication). The item
;;; should be marked as 'selected' when the string state is equal to the value
;;; of the target property.
;;;
;;; See g_menu_item_set_action_and_target() or g_menu_item_set_detailed_action()
;;; for two equivalent calls that are probably more convenient for most uses.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; action :
;;;     the name of the action for this item. [allow-none]
;;;
;;; target_value :
;;;     a GVariant to use as the action target. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_action_and_target ()
;;;
;;; void g_menu_item_set_action_and_target (GMenuItem *menu_item,
;;;                                         const gchar *action,
;;;                                         const gchar *format_string,
;;;                                         ...);
;;;
;;; Sets or unsets the "action" and "target" attributes of menu_item.
;;;
;;; If action is NULL then both the "action" and "target" attributes are unset
;;; (and format_string is ignored along with the positional parameters).
;;;
;;; If action is non-NULL then the "action" attribute is set. format_string is
;;; then inspected. If it is non-NULL then the proper position parameters are
;;; collected to create a GVariant instance to use as the target value. If it is
;;; NULL then the positional parameters are ignored and the "target" attribute
;;; is unset.
;;;
;;; See also g_menu_item_set_action_and_target_value() for an equivalent call
;;; that directly accepts a GVariant. See g_menu_item_set_detailed_action() for
;;; a more convenient version that works with string-typed targets.
;;;
;;; See also g_menu_item_set_action_and_target_value() for a description of the
;;; semantics of the action and target attributes.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; action :
;;;     the name of the action for this item. [allow-none]
;;;
;;; format_string :
;;;     a GVariant format string. [allow-none]
;;;
;;; ... :
;;;     positional parameters, as per format_string
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_detailed_action ()
;;;
;;; void g_menu_item_set_detailed_action (GMenuItem *menu_item,
;;;                                       const gchar *detailed_action);
;;;
;;; Sets the "action" and possibly the "target" attribute of menu_item.
;;;
;;; The format of detailed_action is the same format parsed by
;;; g_action_parse_detailed_name().
;;;
;;; See g_menu_item_set_action_and_target() or
;;; g_menu_item_set_action_and_target_value() for more flexible (but slightly
;;; less convenient) alternatives.
;;;
;;; See also g_menu_item_set_action_and_target_value() for a description of the
;;; semantics of the action and target attributes.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; detailed_action :
;;;     the "detailed" action string
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_section ()
;;;
;;; void g_menu_item_set_section (GMenuItem *menu_item, GMenuModel *section);
;;;
;;; Sets or unsets the "section" link of menu_item to section.
;;;
;;; The effect of having one menu appear as a section of another is exactly as
;;; it sounds: the items from section become a direct part of the menu that
;;; menu_item is added to. See g_menu_item_new_section() for more information
;;; about what it means for a menu item to be a section.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; section :
;;;     a GMenuModel, or NULL. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_submenu ()
;;;
;;; void g_menu_item_set_submenu (GMenuItem *menu_item, GMenuModel *submenu);
;;;
;;; Sets or unsets the "submenu" link of menu_item to submenu.
;;;
;;; If submenu is non-NULL, it is linked to. If it is NULL then the link is
;;; unset.
;;;
;;; The effect of having one menu appear as a submenu of another is exactly as
;;; it sounds.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; submenu :
;;;     a GMenuModel, or NULL. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_attribute_value ()
;;;
;;; GVariant * g_menu_item_get_attribute_value
;;;                                          (GMenuItem *menu_item,
;;;                                           const gchar *attribute,
;;;                                           const GVariantType *expected_type)
;;;
;;; Queries the named attribute on menu_item.
;;;
;;; If expected_type is specified and the attribute does not have this type,
;;; NULL is returned. NULL is also returned if the attribute simply does not
;;; exist.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; attribute :
;;;     the attribute name to query
;;;
;;; expected_type :
;;;     the expected type of the attribute. [allow-none]
;;;
;;; Returns :
;;;     the attribute value, or NULL. [transfer full]
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_attribute ()
;;;
;;; gboolean g_menu_item_get_attribute (GMenuItem *menu_item,
;;;                                     const gchar *attribute,
;;;                                     const gchar *format_string,
;;;                                     ...);
;;;
;;; Queries the named attribute on menu_item.
;;;
;;; If the attribute exists and matches the GVariantType corresponding to
;;; format_string then format_string is used to deconstruct the value into the
;;; positional parameters and TRUE is returned.
;;;
;;; If the attribute does not exist, or it does exist but has the wrong type,
;;; then the positional parameters are ignored and FALSE is returned.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; attribute :
;;;     the attribute name to query
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     positional parameters, as per format_string
;;;
;;; Returns :
;;;     TRUE if the named attribute was found with the expected type
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_link ()
;;;
;;; GMenuModel * g_menu_item_get_link (GMenuItem *menu_item, const gchar *link);
;;;
;;; Queries the named link on menu_item.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; link :
;;;     the link name to query
;;;
;;; Returns :
;;;     the link, or NULL. [transfer full]
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_attribute_value ()
;;;
;;; void g_menu_item_set_attribute_value (GMenuItem *menu_item,
;;;                                       const gchar *attribute,
;;;                                       GVariant *value);
;;;
;;; Sets or unsets an attribute on menu_item.
;;;
;;; The attribute to set or unset is specified by attribute. This can be one of
;;; the standard attribute names G_MENU_ATTRIBUTE_LABEL,
;;; G_MENU_ATTRIBUTE_ACTION, G_MENU_ATTRIBUTE_TARGET, or a custom attribute
;;; name. Attribute names are restricted to lowercase characters, numbers and
;;; '-'. Furthermore, the names must begin with a lowercase character, must not
;;; end with a '-', and must not contain consecutive dashes.
;;;
;;; must consist only of lowercase ASCII characters, digits and '-'.
;;;
;;; If value is non-NULL then it is used as the new value for the attribute. If
;;; value is NULL then the attribute is unset. If the value GVariant is
;;; floating, it is consumed.
;;;
;;; See also g_menu_item_set_attribute() for a more convenient way to do the
;;; same.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; attribute :
;;;     the attribute to set
;;;
;;; value :
;;;     a GVariant to use as the value, or NULL. [allow-none]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_attribute ()
;;;
;;; void g_menu_item_set_attribute (GMenuItem *menu_item,
;;;                                 const gchar *attribute,
;;;                                 const gchar *format_string,
;;;                                 ...);
;;;
;;; Sets or unsets an attribute on menu_item.
;;;
;;; The attribute to set or unset is specified by attribute. This can be one of
;;; the standard attribute names G_MENU_ATTRIBUTE_LABEL,
;;; G_MENU_ATTRIBUTE_ACTION, G_MENU_ATTRIBUTE_TARGET, or a custom attribute
;;; name. Attribute names are restricted to lowercase characters, numbers and
;;; '-'. Furthermore, the names must begin with a lowercase character, must not
;;; end with a '-', and must not contain consecutive dashes.
;;;
;;; If format_string is non-NULL then the proper position parameters are
;;; collected to create a GVariant instance to use as the attribute value. If it
;;; is NULL then the positional parameterrs are ignored and the named attribute
;;; is unset.
;;;
;;; See also g_menu_item_set_attribute_value() for an equivalent call that
;;; directly accepts a GVariant.
;;;
;;; menu_item :
;;;     a GMenuItem
;;;
;;; attribute :
;;;     the attribute to set
;;;
;;; format_string :
;;;     a GVariant format string, or NULL. [allow-none]
;;;
;;; ... :
;;;     positional parameters, as per format_string
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_link ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_menu_item_set_link" g-menu-item-set-link) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[item]{a @class{g-menu-item} object}
  @argument[link]{a string with the type of link to establish or unset}
  @argument[model]{the @class{g-menu-model} object to link to, or @code{nil} to
    unset}
  @begin{short}
    Creates a link from @arg{item} to @arg{model} if non-@code{nil},
    or unsets it.
  @end{short}

  Links are used to establish a relationship between a particular menu item
  and another menu. For example, @var{+g-menu-linke-submenu+} is used to
  associate a submenu with a particular menu item, and
  @var{+g-menu-link-section+} is used to create a section. Other types of link
  can be used, but there is no guarantee that clients will be able to make sense
  of them. Link types are restricted to lowercase characters, numbers and '-'.
  Furthermore, the names must begin with a lowercase character, must not end
  with a '-', and must not contain consecutive dashes.
  @see-class{g-menu-item}
  @see-class{g-menu-model}"
  (menu-item (g-object g-menu-item))
  (link :string)
  (model (g-object g-menu-model)))

(export 'g-menu-item-set-link)

;;; --- End of file gio.menu.lisp ----------------------------------------------
