;;; ----------------------------------------------------------------------------
;;; gio.menu-model.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.36.4 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GMenuModel
;;;
;;; An abstract class representing the contents of a menu
;;;
;;; Synopsis
;;;
;;;     GMenuModel
;;;
;;;     g_menu_model_is_mutable
;;;     g_menu_model_get_n_items
;;;
;;;     G_MENU_ATTRIBUTE_ACTION
;;;     G_MENU_ATTRIBUTE_LABEL
;;;     G_MENU_ATTRIBUTE_TARGET
;;;     G_MENU_LINK_SECTION
;;;     G_MENU_LINK_SUBMENU
;;;
;;;     g_menu_model_get_item_attribute_value
;;;     g_menu_model_get_item_attribute
;;;     g_menu_model_get_item_link
;;;     g_menu_model_iterate_item_attributes
;;;     g_menu_model_iterate_item_links
;;;
;;;     g_menu_model_items_changed
;;;
;;;     GMenuAttributeIter
;;;
;;;     g_menu_attribute_iter_get_next
;;;     g_menu_attribute_iter_get_name
;;;     g_menu_attribute_iter_get_value
;;;     g_menu_attribute_iter_next
;;;
;;;     GMenuLinkIter
;;;
;;;     g_menu_link_iter_get_name
;;;     g_menu_link_iter_get_next
;;;     g_menu_link_iter_get_value
;;;     g_menu_link_iter_next
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GMenuModel
;;;          +----GDBusMenuModel
;;;          +----GMenu
;;;
;;;   GObject
;;;    +----GMenuAttributeIter
;;;
;;;   GObject
;;;    +----GMenuLinkIter
;;;
;;; Signals
;;;
;;;   "items-changed"                                  : Run Last
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GMenuModel
;;;
;;; typedef struct _GMenuModel GMenuModel;
;;;
;;; GMenuModel is an opaque structure type. You must access it using the
;;; functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GMenuModel" g-menu-model
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_menu_model_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-menu-model 'type)
 "@version{#2013-8-8}
  @begin{short}
    @sym{g-menu-model} represents the contents of a menu - an ordered list of
    menu items. The items are associated with actions, which can be activated
    through them. Items can be grouped in sections, and may have submenus
    associated with them. Both items and sections usually have some
    representation data, such as labels or icons. The type of the associated
    action, i. e. whether it is stateful, and what kind of state it has, can
    influence the representation of the item.
  @end{short}

  The conceptual model of menus in @sym{g-menu-model} is hierarchical: sections
  and submenus are again represented by @sym{g-menu-model}'s. Menus themselves
  do not define their own roles. Rather, the role of a particular
  @sym{g-menu-model} is defined by the item that references it, or, in the case
  of the 'root' menu, is defined by the context in which it is used.

  As an example, consider the visible portions of the menu in Figure 2,
  \"An example menu\".

  Figure 2. An example menu

  @image[menu-example]{}

  There are 8 \"menus\" visible in the screenshot: one menubar, two submenus and
  5 sections:
  @begin{itemize}
    @item{the toplevel menubar (containing 4 items)}
    @item{the View submenu (containing 3 sections)}
    @item{the first section of the View submenu (containing 2 items)}
    @item{the second section of the View submenu (containing 1 item)}
    @item{the final section of the View submenu (containing 1 item)}
    @item{the Highlight Mode submenu (containing 2 sections)}
    @item{the Sources section (containing 2 items)}
    @item{the Markup section (containing 2 items)}
  @end{itemize}
  Figure 3, \"A menu model\" illustrates the conceptual connection between these
  8 menus. Each large block in the figure represents a menu and the smaller
  blocks within the large block represent items in that menu. Some items
  contain references to other menus.

  Figure 3. A menu model

  @image[menu-model]{}

  Notice that the separators visible in Figure 2, \"An example menu\" appear
  nowhere in Figure 3, \"A menu model\". This is because separators are not
  explicitly represented in the menu model. Instead, a separator is inserted
  between any two non-empty sections of a menu. Section items can have labels
  just like any other item. In that case, a display system may show a section
  header instead of a separator.

  The motivation for this abstract model of application controls is that
  modern user interfaces tend to make these controls available outside the
  application. Examples include global menus, jumplists, dash boards, etc. To
  support such uses, it is necessary to 'export' information about actions and
  their representation in menus, which is exactly what the
  @class{g-action-group} exporter and the @sym{g-menu-model} exporter do for
  @class{g-action-group} and @sym{g-menu-model}. The client-side counterparts to
  make use of the exported information are @code{GDBusActionGroup} and
  @code{GDBusMenuModel}.

  The API of @sym{g-menu-model} is very generic, with iterators for the
  attributes and links of an item, see the functions
  @fun{g-menu-model-iterate-item-attributes} and
  @fun{g-menu-model-iterate-item-links}. The 'standard' attributes and link
  types have predefined names: @var{+g-menu-attribute-label+},
  @var{+g-menu-attribute-action+}, @var{+g-menu-attribute-target+},
  @bvar{+g-menu-link-section+} and @var{+g-menu-link-submenu+}.

  Items in a @sym{g-menu-model} represent active controls if they refer to an
  action that can get activated when the user interacts with the menu item. The
  reference to the action is encoded by the string ID in the
  @var{+g-menu-attribute-action+} attribute. An action ID uniquely identifies an
  action in an action group. Which action group(s) provide actions depends on
  the context in which the menu model is used. E. g. when the model is exported
  as the application menu of a @class{gtk-application}, actions can be
  application-wide or window-specific, and thus come from two different action
  groups. By convention, the application-wide actions have names that start with
  \"app.\", while the names of window-specific actions start with \"win.\".

  While a wide variety of stateful actions is possible, the following is the
  minimum that is expected to be supported by all users of exported menu
  information:
  @begin{itemize}
    @item{an action with no parameter type and no state}
    @item{an action with no parameter type and boolean state}
    @item{an action with string parameter type and string state}
  @end{itemize}
  @b{Stateless.}  A stateless action typically corresponds to an ordinary menu
  item. Selecting such a menu item will activate the action, with no parameter.

  @b{Boolean State.}  An action with a boolean state will most typically be used
  with a \"toggle\" or \"switch\" menu item. The state can be set directly, but
  activating the action, with no parameter, results in the state being
  toggled. Selecting a toggle menu item will activate the action. The menu
  item should be rendered as \"checked\" when the state is true.

  @b{String Parameter and State.}  Actions with string parameters and state will
  most typically be used to represent an enumerated choice over the items
  available for a group of radio menu items. Activating the action with a
  string parameter is equivalent to setting that parameter as the state. Radio
  menu items, in addition to being associated with the action, will have a
  target value. Selecting that menu item will result in activation of the
  action with the target value as the parameter. The menu item should be
  rendered as \"selected\" when the state of the action is equal to the target
  value of the menu item.
  @begin[Signal Details]{dictionary}
    @subheading{The \"items-changed\" signal}
      @begin{pre}
 lambda (model position removed added)   : Run Last
      @end{pre}
      Emitted when a change has occured to the menu.

      The only changes that can occur to a menu is that items are removed or
      added. Items may not change, except by being removed and added back in the
      same location. This signal is capable of describing both of those changes
      at the same time.

      The signal means that starting at the index position, removed items were
      removed and added items were added in their place. If removed is zero then
      only items were added. If added is zero then only items were removed.

      As an example, if the menu contains items a, b, c, d (in that order) and
      the signal (2, 1, 3) occurs then the new composition of the menu will be
      a, b, _, _, _, d, with each _ representing some new item.

      Signal handlers may query the model, particularly the added items, and
      expect to see the results of the modification that is being reported. The
      signal is emitted after the modification.
      @begin[code]{table}
        @entry[model]{The @sym{g-menu-model} that is changing.}
        @entry[position]{The position of the change.}
        @entry[removed]{The number of items removed.}
        @entry[added]{The number of items added.}
      @end{table}
  @end{dictionary}
  Since 2.32")

;;; ----------------------------------------------------------------------------
;;; g_menu_model_is_mutable ()
;;;
;;; gboolean g_menu_model_is_mutable (GMenuModel *model);
;;;
;;; Queries if model is mutable.
;;;
;;; An immutable GMenuModel will never emit the "items-changed" signal.
;;; Consumers of the model may make optimisations accordingly.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; Returns :
;;;     TRUE if the model is mutable (ie: "items-changed" may be emitted).
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_n_items ()
;;;
;;; gint g_menu_model_get_n_items (GMenuModel *model);
;;;
;;; Query the number of items in model.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; Returns :
;;;     the number of items
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_MENU_ATTRIBUTE_ACTION
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+g-menu-attribute-action+ atdoc:*variable-name-alias*)
      "Constant")

(defparameter +g-menu-attribute-action+ "action"
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @variable-value{\"action\"}
  @begin{short}
    The menu item attribute which holds the action name of the item.
  @end{short}
  Action names are namespaced with an identifier for the action group in which
  the action resides. For example, \"win.\" for window-specific actions and
  \"app.\" for application-wide actions.

  See also the functions @fun{g-menu-model-get-item-attribute} and
  @fun{g-menu-item-set-attribute}.

  Since 2.32
  @see-function{g-menu-model-get-item-attribute}
  @see-function{g-menu-item-set-attribute}")

(export '+g-menu-attribute-action+)

;;; ----------------------------------------------------------------------------
;;; G_MENU_ATTRIBUTE_LABEL
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+g-menu-attribute-label+ atdoc:*variable-name-alias*)
      "Constant")

(defparameter +g-menu-attribute-label+ "label"
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @variable-value{\"label\"}
  @begin{short}
    The menu item attribute which holds the label of the item.
  @end{short}

  Since 2.32")

(export '+g-menu-attribute-label+)

;;; ----------------------------------------------------------------------------
;;; G_MENU_ATTRIBUTE_TARGET
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+g-menu-attribute-target+ atdoc:*variable-name-alias*)
      "Constant")

(defparameter +g-menu-attribute-target+ "target"
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @variable-value{\"target\"}
  @begin{short}
    The menu item attribute which holds the target with which the item's action
    will be activated.
  @end{short}

  See also the function @fun{g-menu-item-set-action-and-target}.

  Since 2.32
  @see-function{g-menu-item-set-action-and-target}")

(export '+g-menu-attribute-target+)

;;; ----------------------------------------------------------------------------
;;; G_MENU_LINK_SECTION
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+g-menu-link-section+ atdoc:*variable-name-alias*) "Constant")

(defparameter +g-menu-link-section+ "section"
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @variable-value{\"section\"}
  @begin{short}
    The name of the link that associates a menu item with a section. The linked
    menu will usually be shown in place of the menu item, using the item's label
    as a header.
  @end{short}

  See also the function @fun{g-menu-item-set-link}.

  Since 2.32
  @see-function{g-menu-item-set-link}")

(export '+g-menu-link-section+)

;;; ----------------------------------------------------------------------------
;;; G_MENU_LINK_SUBMENU
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+g-menu-link-submenu+ atdoc:*variable-name-alias*) "Constant")

(defparameter +g-menu-link-submenu+ "submenu"
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @variable-value{\"submenu\"}
  @begin{short}
    The name of the link that associates a menu item with a submenu.
  @end{short}

  See also the function @fun{g-menu-item-set-link}.

  Since 2.32
  @see-function{g-menu-item-set-link}")

(export '+g-menu-link-submenu+)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_item_attribute_value ()
;;;
;;; GVariant * g_menu_model_get_item_attribute_value
;;;                                           (GMenuModel *model,
;;;                                           gint item_index,
;;;                                           const gchar *attribute,
;;;                                           const GVariantType *expected_type)
;;;
;;; Queries the item at position item_index in model for the attribute specified
;;; by attribute.
;;;
;;; If expected_type is non-NULL then it specifies the expected type of the
;;; attribute. If it is NULL then any type will be accepted.
;;;
;;; If the attribute exists and matches expected_type (or if the expected type
;;; is unspecified) then the value is returned.
;;;
;;; If the attribute does not exist, or does not match the expected type then
;;; NULL is returned.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; item_index :
;;;     the index of the item
;;;
;;; attribute :
;;;     the attribute to query
;;;
;;; expected_type :
;;;     the expected type of the attribute, or NULL. [allow-none]
;;;
;;; Returns :
;;;     the value of the attribute. [transfer full]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_item_attribute ()
;;;
;;; gboolean g_menu_model_get_item_attribute (GMenuModel *model,
;;;                                           gint item_index,
;;;                                           const gchar *attribute,
;;;                                           const gchar *format_string,
;;;                                           ...);
;;;
;;; Queries item at position item_index in model for the attribute specified by
;;; attribute.
;;;
;;; If the attribute exists and matches the GVariantType corresponding to
;;; format_string then format_string is used to deconstruct the value into the
;;; positional parameters and TRUE is returned.
;;;
;;; If the attribute does not exist, or it does exist but has the wrong type,
;;; then the positional parameters are ignored and FALSE is returned.
;;;
;;; This function is a mix of g_menu_model_get_item_attribute_value() and
;;; g_variant_get(), followed by a g_variant_unref(). As such, format_string
;;; must make a complete copy of the data (since the GVariant may go away after
;;; the call to g_variant_unref()). In particular, no '&' characters are allowed
;;; in format_string.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; item_index :
;;;     the index of the item
;;;
;;; attribute :
;;;     the attribute to query
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
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_item_link ()
;;;
;;; GMenuModel * g_menu_model_get_item_link (GMenuModel *model,
;;;                                          gint item_index,
;;;                                          const gchar *link);
;;;
;;; Queries the item at position item_index in model for the link specified by
;;; link.
;;;
;;; If the link exists, the linked GMenuModel is returned. If the link does not
;;; exist, NULL is returned.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; item_index :
;;;     the index of the item
;;;
;;; link :
;;;     the link to query
;;;
;;; Returns :
;;;     the linked GMenuModel, or NULL. [transfer full]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_iterate_item_attributes ()
;;;
;;; GMenuAttributeIter * g_menu_model_iterate_item_attributes
;;;                                                         (GMenuModel *model,
;;;                                                          gint item_index);
;;;
;;; Creates a GMenuAttributeIter to iterate over the attributes of the item at
;;; position item_index in model.
;;;
;;; You must free the iterator with g_object_unref() when you are done.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; item_index :
;;;     the index of the item
;;;
;;; Returns :
;;;     a new GMenuAttributeIter. [transfer full]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_iterate_item_links ()
;;;
;;; GMenuLinkIter * g_menu_model_iterate_item_links (GMenuModel *model,
;;;                                                  gint item_index);
;;;
;;; Creates a GMenuLinkIter to iterate over the links of the item at position
;;; item_index in model.
;;;
;;; You must free the iterator with g_object_unref() when you are done.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; item_index :
;;;     the index of the item
;;;
;;; Returns :
;;;     a new GMenuLinkIter. [transfer full]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_items_changed ()
;;;
;;; void g_menu_model_items_changed (GMenuModel *model,
;;;                                  gint position,
;;;                                  gint removed,
;;;                                  gint added);
;;;
;;; Requests emission of the "items-changed" signal on model.
;;;
;;; This function should never be called except by GMenuModel subclasses. Any
;;; other calls to this function will very likely lead to a violation of the
;;; interface of the model.
;;;
;;; The implementation should update its internal representation of the menu
;;; before emitting the signal. The implementation should further expect to
;;; receive queries about the new state of the menu (and particularly added menu
;;; items) while signal handlers are running.
;;;
;;; The implementation must dispatch this call directly from a mainloop entry
;;; and not in response to calls -- particularly those from the GMenuModel API.
;;; Said another way: the menu must not change while user code is running
;;; without returning to the mainloop.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; position :
;;;     the position of the change
;;;
;;; removed :
;;;     the number of items removed
;;;
;;; added :
;;;     the number of items added
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GMenuAttributeIter
;;;
;;; struct GMenuAttributeIter;
;;;
;;; GMenuAttributeIter is an opaque structure type. You must access it using the
;;; functions below.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_get_next ()
;;;
;;; gboolean g_menu_attribute_iter_get_next (GMenuAttributeIter *iter,
;;;                                          const gchar **out_name,
;;;                                          GVariant **value);
;;;
;;; This function combines g_menu_attribute_iter_next() with
;;; g_menu_attribute_iter_get_name() and g_menu_attribute_iter_get_value().
;;;
;;; First the iterator is advanced to the next (possibly first) attribute. If
;;; that fails, then FALSE is returned and there are no other effects.
;;;
;;; If successful, name and value are set to the name and value of the attribute
;;; that has just been advanced to. At this point,
;;; g_menu_attribute_iter_get_name() and g_menu_attribute_iter_get_value() will
;;; return the same values again.
;;;
;;; The value returned in name remains valid for as long as the iterator remains
;;; at the current position. The value returned in value must be unreffed using
;;; g_variant_unref() when it is no longer in use.
;;;
;;; iter :
;;;     a GMenuAttributeIter
;;;
;;; out_name :
;;;     the type of the attribute. [out][allow-none][transfer none]
;;;
;;; value :
;;;     the attribute value. [out][allow-none][transfer full]
;;;
;;; Returns :
;;;     TRUE on success, or FALSE if there is no additional attribute
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_get_name ()
;;;
;;; const gchar * g_menu_attribute_iter_get_name (GMenuAttributeIter *iter);
;;;
;;; Gets the name of the attribute at the current iterator position, as a
;;; string.
;;;
;;; The iterator is not advanced.
;;;
;;; iter :
;;;     a GMenuAttributeIter
;;;
;;; Returns :
;;;     the name of the attribute
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_get_value ()
;;;
;;; GVariant * g_menu_attribute_iter_get_value (GMenuAttributeIter *iter);
;;;
;;; Gets the value of the attribute at the current iterator position.
;;;
;;; The iterator is not advanced.
;;;
;;; iter :
;;;     a GMenuAttributeIter
;;;
;;; Returns :
;;;     the value of the current attribute. [transfer full]
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_next ()
;;;
;;; gboolean g_menu_attribute_iter_next (GMenuAttributeIter *iter);
;;;
;;; Attempts to advance the iterator to the next (possibly first) attribute.
;;;
;;; TRUE is returned on success, or FALSE if there are no more attributes.
;;;
;;; You must call this function when you first acquire the iterator to advance
;;; it to the first attribute (and determine if the first attribute exists at
;;; all).
;;;
;;; iter :
;;;     a GMenuAttributeIter
;;;
;;; Returns :
;;;     TRUE on success, or FALSE when there are no more attributes
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GMenuLinkIter
;;;
;;; struct GMenuLinkIter;
;;;
;;; GMenuLinkIter is an opaque structure type. You must access it using the
;;; functions below.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_get_name ()
;;;
;;; const gchar * g_menu_link_iter_get_name (GMenuLinkIter *iter);
;;;
;;; Gets the name of the link at the current iterator position.
;;;
;;; The iterator is not advanced.
;;;
;;; iter :
;;;     a GMenuLinkIter
;;;
;;; Returns :
;;;     the type of the link
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_get_next ()
;;;
;;; gboolean g_menu_link_iter_get_next (GMenuLinkIter *iter,
;;;                                     const gchar **out_link,
;;;                                     GMenuModel **value);
;;;
;;; This function combines g_menu_link_iter_next() with
;;; g_menu_link_iter_get_name() and g_menu_link_iter_get_value().
;;;
;;; First the iterator is advanced to the next (possibly first) link. If that
;;; fails, then FALSE is returned and there are no other effects.
;;;
;;; If successful, out_link and value are set to the name and GMenuModel of the
;;; link that has just been advanced to. At this point,
;;; g_menu_link_iter_get_name() and g_menu_link_iter_get_value() will return the
;;; same values again.
;;;
;;; The value returned in out_link remains valid for as long as the iterator
;;; remains at the current position. The value returned in value must be
;;; unreffed using g_object_unref() when it is no longer in use.
;;;
;;; iter :
;;;     a GMenuLinkIter
;;;
;;; out_link :
;;;     the name of the link
;;;
;;; value :
;;;     the linked GMenuModel
;;;
;;; Returns :
;;;     TRUE on success, or FALSE if there is no additional link
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_get_value ()
;;;
;;; GMenuModel * g_menu_link_iter_get_value (GMenuLinkIter *iter);
;;;
;;; Gets the linked GMenuModel at the current iterator position.
;;;
;;; The iterator is not advanced.
;;;
;;; iter :
;;;     a GMenuLinkIter
;;;
;;; Returns :
;;;     the GMenuModel that is linked to.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_next ()
;;;
;;; gboolean g_menu_link_iter_next (GMenuLinkIter *iter);
;;;
;;; Attempts to advance the iterator to the next (possibly first) link.
;;;
;;; TRUE is returned on success, or FALSE if there are no more links.
;;;
;;; You must call this function when you first acquire the iterator to advance
;;; it to the first link (and determine if the first link exists at all).
;;;
;;; iter :
;;;     a GMenuLinkIter
;;;
;;; Returns :
;;;     TRUE on success, or FALSE when there are no more links
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.menu-model-lisp ----------------------------------------
