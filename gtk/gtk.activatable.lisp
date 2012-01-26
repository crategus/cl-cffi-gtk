;;; ----------------------------------------------------------------------------
;;; gtk.activatable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.2 Reference Manual
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
;;; GtkActivatable
;;; 
;;; An interface for activatable widgets
;;; 	
;;; Synopsis
;;; 
;;;     GtkActivatable
;;;     GtkActivatableIface
;;;
;;;     gtk_activatable_do_set_related_action
;;;     gtk_activatable_get_related_action
;;;     gtk_activatable_get_use_action_appearance
;;;     gtk_activatable_sync_action_properties
;;;     gtk_activatable_set_related_action
;;;     gtk_activatable_set_use_action_appearance
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkActivatable
;;; 
;;; Prerequisites
;;; 
;;; GtkActivatable requires GObject.
;;; Known Implementations
;;; 
;;; GtkActivatable is implemented by GtkButton, GtkCheckButton,
;;; GtkCheckMenuItem, GtkColorButton, GtkFontButton, GtkImageMenuItem,
;;; GtkLinkButton, GtkLockButton, GtkMenuItem, GtkMenuToolButton,
;;; GtkRadioButton, GtkRadioMenuItem, GtkRadioToolButton, GtkRecentChooserMenu,
;;; GtkScaleButton, GtkSeparatorMenuItem, GtkSeparatorToolItem, GtkSwitch,
;;; GtkTearoffMenuItem, GtkToggleButton, GtkToggleToolButton, GtkToolButton,
;;; GtkToolItem and GtkVolumeButton.
;;;
;;; Properties
;;; 
;;;   "related-action"           GtkAction*            : Read / Write
;;;   "use-action-appearance"    gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; Activatable widgets can be connected to a GtkAction and reflects the state
;;; of its action. A GtkActivatable can also provide feedback through its
;;; action, as they are responsible for activating their related actions.
;;; 
;;; Implementing GtkActivatable
;;; 
;;; When extending a class that is already GtkActivatable; it is only necessary
;;; to implement the GtkActivatable->sync_action_properties() and
;;; GtkActivatable->update() methods and chain up to the parent implementation,
;;; however when introducing a new GtkActivatable class; the "related-action"
;;; and "use-action-appearance" properties need to be handled by the
;;; implementor. Handling these properties is mostly a matter of installing the
;;; action pointer and boolean flag on your instance, and calling
;;; gtk_activatable_do_set_related_action() and
;;; gtk_activatable_sync_action_properties() at the appropriate times.
;;; 
;;; Example 83. A class fragment implementing GtkActivatable
;;; 
;;; enum {
;;; ...
;;; 
;;; PROP_ACTIVATABLE_RELATED_ACTION,
;;; PROP_ACTIVATABLE_USE_ACTION_APPEARANCE
;;; }
;;; 
;;; struct _FooBarPrivate
;;; {
;;; 
;;;   ...
;;; 
;;;   GtkAction      *action;
;;;   gboolean        use_action_appearance;
;;; };
;;; 
;;; ...
;;; 
;;; static void foo_bar_activatable_interface_init (GtkActivatableIface *iface);
;;; static void foo_bar_activatable_update (GtkActivatable *activatable,
;;;                                         GtkAction      *action,
;;;                                         const gchar    *property_name);
;;; static void foo_bar_activatable_sync_action_properties
;;;                                                (GtkActivatable *activatable,
;;;                                                 GtkAction      *action);
;;; ...
;;; 
;;; static void
;;; foo_bar_class_init (FooBarClass *klass)
;;; {
;;; 
;;;   ...
;;; 
;;;   g_object_class_override_property (gobject_class,
;;;                                     PROP_ACTIVATABLE_RELATED_ACTION,
;;;                                     "related-action");
;;;   g_object_class_override_property (gobject_class,
;;;                                     PROP_ACTIVATABLE_USE_ACTION_APPEARANCE,
;;;                                     "use-action-appearance");
;;;   ...
;;; }
;;; 
;;; 
;;; static void
;;; foo_bar_activatable_interface_init (GtkActivatableIface  *iface)
;;; {
;;;   iface->update = foo_bar_activatable_update;
;;;   iface->sync_action_properties =foo_bar_activatable_sync_action_properties;
;;; }
;;; 
;;; ... Break the reference using gtk_activatable_do_set_related_action()...
;;; 
;;; static void 
;;; foo_bar_dispose (GObject *object)
;;; {
;;;   FooBar *bar = FOO_BAR (object);
;;;   FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
;;; 
;;;   ...
;;; 
;;;   if (priv->action)
;;;     {
;;;       gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), NULL);
;;;       priv->action = NULL;
;;;     }
;;;   G_OBJECT_CLASS (foo_bar_parent_class)->dispose (object);
;;; }
;;; 
;;; ... Handle the "related-action" and "use-action-appearance" properties ...
;;; 
;;; static void
;;; foo_bar_set_property (GObject         *object,
;;;                       guint            prop_id,
;;;                       const GValue    *value,
;;;                       GParamSpec      *pspec)
;;; {
;;;   FooBar *bar = FOO_BAR (object);
;;;   FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
;;; 
;;;   switch (prop_id)
;;;     {
;;; 
;;;       ...
;;; 
;;;     case PROP_ACTIVATABLE_RELATED_ACTION:
;;;       foo_bar_set_related_action (bar, g_value_get_object (value));
;;;       break;
;;;     case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
;;;       foo_bar_set_use_action_appearance (bar, g_value_get_boolean (value));
;;;       break;
;;;     default:
;;;       G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
;;;       break;
;;;     }
;;; }
;;; 
;;; static void
;;; foo_bar_get_property (GObject            *object,
;;;                          guint            prop_id,
;;;                          GValue          *value,
;;;                          GParamSpec      *pspec)
;;; {
;;;   FooBar *bar = FOO_BAR (object);
;;;   FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
;;; 
;;;   switch (prop_id)
;;;     { 
;;; 
;;;       ...
;;; 
;;;     case PROP_ACTIVATABLE_RELATED_ACTION:
;;;       g_value_set_object (value, priv->action);
;;;       break;
;;;     case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
;;;       g_value_set_boolean (value, priv->use_action_appearance);
;;;       break;
;;;     default:
;;;       G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
;;;       break;
;;;     }
;;; }
;;; 
;;; static void
;;; foo_bar_set_use_action_appearance (FooBar   *bar, 
;;;                    gboolean  use_appearance)
;;; {
;;;   FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
;;; 
;;;   if (priv->use_action_appearance != use_appearance)
;;;     {
;;;       priv->use_action_appearance = use_appearance;
;;;       
;;;       gtk_activatable_sync_action_properties (GTK_ACTIVATABLE (bar),
;;;                                               priv->action);
;;;     }
;;; }
;;; 
;;; ... call gtk_activatable_do_set_related_action() and then assign the
;;;     action pointer, no need to reference the action here since
;;;     gtk_activatable_do_set_related_action() already holds a reference here
;;;     for you...
;;; static void
;;; foo_bar_set_related_action (FooBar    *bar, 
;;;                 GtkAction *action)
;;; {
;;;   FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
;;; 
;;;   if (priv->action == action)
;;;     return;
;;; 
;;;   gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), action);
;;; 
;;;   priv->action = action;
;;; }
;;; 
;;; ... Selectively reset and update activatable depending on the
;;;     use-action-appearance property ...
;;; static void
;;; gtk_button_activatable_sync_action_properties (GtkActivatable *activatable,
;;;                                                GtkAction      *action)
;;; {
;;;   GtkButtonPrivate *priv = GTK_BUTTON_GET_PRIVATE (activatable);
;;; 
;;;   if (!action)
;;;     return;
;;; 
;;;   if (gtk_action_is_visible (action))
;;;     gtk_widget_show (GTK_WIDGET (activatable));
;;;   else
;;;     gtk_widget_hide (GTK_WIDGET (activatable));
;;;   
;;;   gtk_widget_set_sensitive (GTK_WIDGET (activatable),
;;;                             gtk_action_is_sensitive (action));
;;; 
;;;   ...
;;;   
;;;   if (priv->use_action_appearance)
;;;     {
;;;       if (gtk_action_get_stock_id (action))
;;;     foo_bar_set_stock (button, gtk_action_get_stock_id (action));
;;;       else if (gtk_action_get_label (action))
;;;     foo_bar_set_label (button, gtk_action_get_label (action));
;;; 
;;;       ...
;;; 
;;;     }
;;; }
;;; 
;;; static void 
;;; foo_bar_activatable_update (GtkActivatable *activatable,
;;;                             GtkAction      *action,
;;;                             const gchar    *property_name)
;;; {
;;;   FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (activatable);
;;; 
;;;   if (strcmp (property_name, "visible") == 0)
;;;     {
;;;       if (gtk_action_is_visible (action))
;;;     gtk_widget_show (GTK_WIDGET (activatable));
;;;       else
;;;     gtk_widget_hide (GTK_WIDGET (activatable));
;;;     }
;;;   else if (strcmp (property_name, "sensitive") == 0)
;;;     gtk_widget_set_sensitive (GTK_WIDGET (activatable),
;;;                               gtk_action_is_sensitive (action));
;;; 
;;;   ...
;;; 
;;;   if (!priv->use_action_appearance)
;;;     return;
;;; 
;;;   if (strcmp (property_name, "stock-id") == 0)
;;;     foo_bar_set_stock (button, gtk_action_get_stock_id (action));
;;;   else if (strcmp (property_name, "label") == 0)
;;;     foo_bar_set_label (button, gtk_action_get_label (action));
;;; 
;;;   ...
;;; }
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "related-action" property
;;; 
;;;   "related-action"           GtkAction*            : Read / Write
;;; 
;;; The action that this activatable will activate and receive updates from
;;; for various states and possibly appearance.
;;; 
;;; Note
;;; 
;;; GtkActivatable implementors need to handle the this property and call
;;; gtk_activatable_do_set_related_action() when it changes.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-action-appearance" property
;;; 
;;;   "use-action-appearance"    gboolean              : Read / Write
;;; 
;;; Whether this activatable should reset its layout and appearance when
;;; setting the related action or when the action changes appearance.
;;; 
;;; See the GtkAction documentation directly to find which properties should
;;; be ignored by the GtkActivatable when this property is FALSE.
;;; 
;;; Note
;;; 
;;; GtkActivatable implementors need to handle this property and call
;;; gtk_activatable_sync_action_properties() on the activatable widget when
;;; it changes.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActivatable
;;; 
;;; typedef struct _GtkActivatable GtkActivatable;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkActivatable" gtk-activatable
  (:export t
   :type-initializer "gtk_activatable_get_type")
  (related-action gtk-activatable-related-action
   "related-action" "GtkAction" t t)
  (use-action-appearance gtk-activatable-use-action-appearance
   "use-action-appearance" "gboolean" t t))

;;; ----------------------------------------------------------------------------
;;; struct GtkActivatableIface
;;; 
;;; struct GtkActivatableIface {
;;;   GTypeInterface g_iface;
;;; 
;;;   /* virtual table */
;;;   void   (* update)                  (GtkActivatable *activatable,
;;; 		                          GtkAction      *action,
;;; 		                          const gchar    *property_name);
;;;   void   (* sync_action_properties)  (GtkActivatable *activatable,
;;; 		                          GtkAction      *action);
;;; };
;;; 
;;; GTypeInterface g_iface;
;;; 
;;; update ()
;;; 	Called to update the activatable when its related action's properties
;;;     change. You must check the "use-action-appearance" property only apply
;;;     action properties that are meant to effect the appearance accordingly.
;;; 
;;; sync_action_properties ()
;;; 	Called to update the activatable completely, this is called internally
;;;     when "related-action" property is set or unset and by the implementor
;;;     when "use-action-appearance" changes.
;;;
;;; Note
;;; 
;;; This method can be called with a NULL action at times
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_do_set_related_action ()
;;; 
;;; void gtk_activatable_do_set_related_action (GtkActivatable *activatable,
;;;                                             GtkAction *action);
;;; 
;;; This is a utility function for GtkActivatable implementors.
;;; 
;;; When implementing GtkActivatable you must call this when handling changes
;;; of the "related-action", and you must also use this to break references in
;;; GObject->dispose().
;;; 
;;; This function adds a reference to the currently set related action for you,
;;; it also makes sure the GtkActivatable->update() method is called when the
;;; related GtkAction properties change and registers to the action's proxy
;;; list.
;;; 
;;; Note
;;; 
;;; Be careful to call this before setting the local copy of the GtkAction
;;; property, since this function uses gtk_activatable_get_action() to retrieve
;;; the previous action
;;; 
;;; activatable :
;;; 	a GtkActivatable
;;; 
;;; action :
;;; 	the GtkAction to set
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_get_related_action ()
;;; 
;;; GtkAction * gtk_activatable_get_related_action (GtkActivatable *activatable)
;;; 
;;; Gets the related GtkAction for activatable.
;;; 
;;; activatable :
;;; 	a GtkActivatable
;;; 
;;; Returns :
;;; 	the related GtkAction if one is set. [transfer none]
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_get_use_action_appearance ()
;;; 
;;; gboolean gtk_activatable_get_use_action_appearance
;;;                                                (GtkActivatable *activatable)
;;; 
;;; Gets whether this activatable should reset its layout and appearance when
;;; setting the related action or when the action changes appearance.
;;; 
;;; activatable :
;;; 	a GtkActivatable
;;; 
;;; Returns :
;;; 	whether activatable uses its actions appearance.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_sync_action_properties ()
;;; 
;;; void gtk_activatable_sync_action_properties (GtkActivatable *activatable,
;;;                                              GtkAction *action);
;;; 
;;; This is called to update the activatable completely, this is called
;;; internally when the "related-action" property is set or unset and by the
;;; implementing class when "use-action-appearance" changes.
;;; 
;;; activatable :
;;; 	a GtkActivatable
;;; 
;;; action :
;;; 	the related GtkAction or NULL. [allow-none]
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_set_related_action ()
;;; 
;;; void gtk_activatable_set_related_action (GtkActivatable *activatable,
;;;                                          GtkAction *action);
;;; 
;;; Sets the related action on the activatable object.
;;; 
;;; Note
;;; 
;;; GtkActivatable implementors need to handle the "related-action" property
;;; and call gtk_activatable_do_set_related_action() when it changes.
;;; 
;;; activatable :
;;; 	a GtkActivatable
;;; 
;;; action :
;;; 	the GtkAction to set
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_set_use_action_appearance ()
;;; 
;;; void gtk_activatable_set_use_action_appearance (GtkActivatable *activatable,
;;;                                                 gboolean use_appearance);
;;; 
;;; Sets whether this activatable should reset its layout and appearance when
;;; setting the related action or when the action changes appearance
;;; 
;;; Note
;;; 
;;; GtkActivatable implementors need to handle the "use-action-appearance"
;;; property and call gtk_activatable_sync_action_properties() to update
;;; activatable if needed.
;;; 
;;; activatable :
;;; 	a GtkActivatable
;;; 
;;; use_appearance :
;;; 	whether to use the actions appearance
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.aktivatable.lisp ---------------------------------------
