;;; ----------------------------------------------------------------------------
;;; gtk.activatable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;;     gtk_activatable_do_set_related_action              * deprecated *
;;;     gtk_activatable_get_related_action                 * deprecated *
;;;     gtk_activatable_get_use_action_appearance          * deprecated *
;;;     gtk_activatable_sync_action_properties             * deprecated *
;;;     gtk_activatable_set_related_action                 * deprecated *
;;;     gtk_activatable_set_use_action_appearance          * deprecated *
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActivatable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkActivatable" gtk-activatable
  (:export t
   :type-initializer "gtk_activatable_get_type")
  (related-action
   gtk-activatable-related-action
   "related-action" "GtkAction" t t)
  (use-action-appearance
   gtk-activatable-use-action-appearance
   "use-action-appearance" "gboolean" t t))

(deprecated-function :gtk gtk-activatable-related-action (3 10))
(deprecated-function :gtk (setf gtk-activatable-related-action) (3 10))
(deprecated-function :gtk gtk-activatable-use-action-appearance (3 10))
(deprecated-function :gtk (setf gtk-activatable-use-action-appearance) (3 10))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-activatable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-activatable 'type)
 "@version{2013-6-2}
  @begin{short}
    Activatable widgets can be connected to a @class{gtk-action} and reflects
    the state of its action. A @sym{gtk-activatable} can also provide feedback
    through its action, as they are responsible for activating their related
    actions.
  @end{short}

  @subheading{Implementing gtk-activatable}
    When extending a class that is already @sym{gtk-activatable}; it is only
    necessary to implement the @code{GtkActivatable->sync_action_properties()}
    and @code{GtkActivatable->update()} methods and chain up to the parent
    implementation, however when introducing a new @sym{gtk-activatable} class;
    the @code{\"related-action\"} and @code{\"use-action-appearance\"}
    properties need to be handled by the implementor. Handling these properties
    is mostly a matter of installing the action pointer and boolean flag on your
    instance, and calling @fun{gtk-activatable-do-set-related-action} and
    @fun{gtk-activatable-sync-action-properties} at the appropriate times.

    @b{Example:} A class fragment implementing @sym{gtk-activatable}
    @begin{pre}
 enum {
   ...

   PROP_ACTIVATABLE_RELATED_ACTION,
   PROP_ACTIVATABLE_USE_ACTION_APPEARANCE
   @}

   struct _FooBarPrivate
   {

     ...

     GtkAction      *action;
     gboolean        use_action_appearance;
   @};

   ...

   static void foo_bar_activatable_interface_init
                                          (GtkActivatableIface *iface);
   static void foo_bar_activatable_update (GtkActivatable *activatable,
                                           GtkAction      *action,
                                           const gchar    *property_name);
   static void foo_bar_activatable_sync_action_properties
                                          (GtkActivatable *activatable,
                                           GtkAction      *action);
   ...

   static void
   foo_bar_class_init (FooBarClass *klass)
   {

     ...

     g_object_class_override_property (gobject_class,
                                       PROP_ACTIVATABLE_RELATED_ACTION,
                                       \"related-action\");
     g_object_class_override_property (gobject_class,
                                       PROP_ACTIVATABLE_USE_ACTION_APPEARANCE,
                                       \"use-action-appearance\");

     ...
   @}

   static void
   foo_bar_activatable_interface_init (GtkActivatableIface  *iface)
   {
     iface->update = foo_bar_activatable_update;
     iface->sync_action_properties =
                                  foo_bar_activatable_sync_action_properties;
   @}

   ... Break the reference using gtk_activatable_do_set_related_action()...

   static void
   foo_bar_dispose (GObject *object)
   {
     FooBar *bar = FOO_BAR (object);
     FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);

     ...

    if (priv->action)
      {
         gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), NULL);
        priv->action = NULL;
      @}
     G_OBJECT_CLASS (foo_bar_parent_class)->dispose (object);
   @}

   ... Handle the \"related-action\" and \"use-action-appearance\" properties ...

   static void
   foo_bar_set_property (GObject         *object,
                         guint            prop_id,
                         const GValue    *value,
                         GParamSpec      *pspec)
   {
     FooBar *bar = FOO_BAR (object);
     FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);

     switch (prop_id)
       {

         ...

       case PROP_ACTIVATABLE_RELATED_ACTION:
         foo_bar_set_related_action (bar, g_value_get_object (value));
         break;
       case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
         foo_bar_set_use_action_appearance (bar,
                                            g_value_get_boolean (value));
         break;
       default:
         G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
         break;
       @}
   @}

   static void
   foo_bar_get_property (GObject         *object,
                            guint         prop_id,
                            GValue       *value,
                            GParamSpec   *pspec)
   {
     FooBar *bar = FOO_BAR (object);
     FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);

     switch (prop_id)
       {

         ...

       case PROP_ACTIVATABLE_RELATED_ACTION:
         g_value_set_object (value, priv->action);
         break;
       case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
         g_value_set_boolean (value, priv->use_action_appearance);
         break;
       default:
         G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
         break;
       @}
  @}


  static void
  foo_bar_set_use_action_appearance (FooBar   *bar,
                     gboolean  use_appearance)
  {
    FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);

    if (priv->use_action_appearance != use_appearance)
      {
        priv->use_action_appearance = use_appearance;

        gtk_activatable_sync_action_properties (GTK_ACTIVATABLE (bar),
                                                priv->action);
      @}
  @}

... call gtk_activatable_do_set_related_action() and then assign the action
pointer, no need to reference the action here since
gtk_activatable_do_set_related_action() already holds a reference here for
you...

  static void
  foo_bar_set_related_action (FooBar    *bar,
                  GtkAction *action)
  {
    FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);

    if (priv->action == action)
      return;

    gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), action);

    priv->action = action;
  @}

... Selectively reset and update activatable depending on the
use-action-appearance property ...

  static void
  gtk_button_activatable_sync_action_properties
                                               (GtkActivatable *activatable,
                                                GtkAction      *action)
  {
    GtkButtonPrivate *priv = GTK_BUTTON_GET_PRIVATE (activatable);

    if (!action)
      return;

    if (gtk_action_is_visible (action))
      gtk_widget_show (GTK_WIDGET (activatable));
    else
      gtk_widget_hide (GTK_WIDGET (activatable));

    gtk_widget_set_sensitive (GTK_WIDGET (activatable),
                              gtk_action_is_sensitive (action));

    ...

    if (priv->use_action_appearance)
      {
        if (gtk_action_get_stock_id (action))
      foo_bar_set_stock (button, gtk_action_get_stock_id (action));
        else if (gtk_action_get_label (action))
      foo_bar_set_label (button, gtk_action_get_label (action));

        ...

      @}
  @}

  static void
  foo_bar_activatable_update (GtkActivatable       *activatable,
                              GtkAction            *action,
                              const gchar          *property_name)
  {
    FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (activatable);

    if (strcmp (property_name, \"visible\") == 0)
      {
        if (gtk_action_is_visible (action))
      gtk_widget_show (GTK_WIDGET (activatable));
        else
      gtk_widget_hide (GTK_WIDGET (activatable));
      @}
    else if (strcmp (property_name, \"sensitive\") == 0)
      gtk_widget_set_sensitive (GTK_WIDGET (activatable),
                                gtk_action_is_sensitive (action));

    ...

    if (!priv->use_action_appearance)
      return;

    if (strcmp (property_name, \"stock-id\") == 0)
      foo_bar_set_stock (button, gtk_action_get_stock_id (action));
    else if (strcmp (property_name, \"label\") == 0)
      foo_bar_set_label (button, gtk_action_get_label (action));

    ...
  @}
    @end{pre}
  @see-slot{gtk-activatable-related-action}
  @see-slot{gtk-activatable-use-action-appearance}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "related-action"
                                               'gtk-activatable) 't)
 "The @code{\"related-action\"} property of type @class{gtk-action}
  (Read / Write)  @br{}
  The action that this activatable will activate and receive updates from for
  various states and possibly appearance. @br{}
  @b{Note:} @class{gtk-activatable} implementors need to handle this
  property and call the @fun{gtk-activatable-do-set-related-action} function
  when it changes. @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-action-appearance"
                                               'gtk-activatable) 't)
 "The @code{\"use-action-appearance\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this activatable should reset its layout and appearance when setting
  the related action or when the action changes appearance.
  See the @class{gtk-action} documentation directly to find which properties
  should be ignored by the @class{gtk-activatable} when this property is
  @code{nil}. @br{}
  @b{Note:}
  @class{gtk-activatable} implementors need to handle this property and call
  @fun{gtk-activatable-sync-action-properties} on the activatable widget when
  it changes. @br{}
  Default value: @code{true} @br{}
  Since 2.16")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-activatable-related-action atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-activatable-related-action 'function)
 "@version{2013-12-8}
  Accessor of the slot @code{\"related-action\"} of the @class{gtk-activatable}
  class.
  @see-class{gtk-activatable}
  @see-function{gtk-activatable-get-related-action}
  @see-function{gtk-activatable-set-related-action}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-activatable-use-action-appearance
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-activatable-use-action-appearance 'function)
 "@version{2013-12-8}
  Accessor of the slot @code{\"use-action-appearance\"} of the
  @class{gtk-activatable} class.
  @see-class{gtk-activatable}
  @see-function{gtk-activatable-get-use-action-appearance}
  @see-function{gtk-activatable-set-use-action-appearance}")

;;; ----------------------------------------------------------------------------
;;; struct GtkActivatableIface
;;;
;;; struct GtkActivatableIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* virtual table */
;;;   void (* update)                 (GtkActivatable *activatable,
;;;                                    GtkAction      *action,
;;;                                    const gchar    *property_name);
;;;   void (* sync_action_properties) (GtkActivatable *activatable,
;;;                                    GtkAction      *action);
;;; };
;;;
;;; GTypeInterface g_iface;
;;;
;;;
;;; update ()
;;;     Called to update the activatable when its related action's properties
;;;     change. You must check the "use-action-appearance" property only apply
;;;     action properties that are meant to effect the appearance accordingly.
;;;
;;; sync_action_properties ()
;;;     Called to update the activatable completely, this is called internally
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
;;; ----------------------------------------------------------------------------

(deprecated-function :gtk gtk-activatable-do-set-related-action (3 10))

(defcfun ("gtk_activatable_do_set_related_action"
           gtk-activatable-do-set-related-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[activatable]{a @class{gtk-activatable} object}
  @argument[action]{the @class{gtk-action} object to set}
  @begin{short}
    This is a utility function for @class{gtk-activatable} interface
    implementors.
  @end{short}

  When implementing the @class{gtk-activatable} interface you must call this
  when handling changes of the @code{\"related-action\"} property, and you must
  also use this to break references in @code{GObject->dispose()}.

  This function adds a reference to the currently set related action for you,
  it also makes sure the @code{GtkActivatable->update()} method is called when
  the related @class{gtk-action} object properties change and registers to the
  action's proxy list.

  @subheading{Note}
    Be careful to call this before setting the local copy of the
    @class{gtk-action} object property, since this function uses
    @fun{gtk-activatable-get-related-action} to retrieve the previous action.

  Since 2.16
  @see-class{gtk-activatable}
  @see-class{gtk-action}
  @see-function{gtk-activatable-get-related-action}"
  (activatable (g-object gtk-activatable))
  (action (g-object gtk-action)))

(export 'gtk-activatable-do-set-related-action)

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_get_related_action ()
;;; ----------------------------------------------------------------------------

(deprecated-function :gtk gtk-activable-get-related-action (3 10))

(declaim (inline gtk-activable-get-related-action))

(defun gtk-activatable-get-related-action (activatable)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[activatable]{a @class{gtk-activatable} object}
  @return{The related @class{gtk-action} object if one is set.}
  @short{Gets the related @class{gtk-action} object for @arg{activatable}.}

  Since 2.16
  @see-class{gtk-activatable}
  @see-function{gtk-activatable-set-related-action}"
  (gtk-activatable-related-action activatable))

(export 'gtk-activatable-get-related-action)

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_get_use_action_appearance ()
;;; ----------------------------------------------------------------------------

(deprecated-function :gtk gtk-activatable-get-use-action-appearance (3 10))

(declaim (inline gtk-activatable-get-use-action-appearance))

(defun gtk-activatable-get-use-action-appearance (activatable)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[activatable]{a @class{gtk-activatable} object}
  @return{Whether @arg{activatable} uses its actions appearance.}
  @begin{short}
    Gets whether this activatable should reset its layout and appearance when
    setting the related action or when the action changes appearance.
  @end{short}

  Since 2.16
  @see-class{gtk-activatable}"
  (gtk-activatable-use-action-appearance activatable))

(export 'gtk-activatable-get-use-action-appearance)

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_sync_action_properties ()
;;; ----------------------------------------------------------------------------

(deprecated-function :gtk gtk-activatable-sync-action-properties (3 10))

(defcfun ("gtk_activatable_sync_action_properties"
           gtk-activatable-sync-action-properties) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[activatable]{a @class{gtk-activatable} object}
  @argument[action]{the related @class{gtk-action} or @code{nil}}
  @begin{short}
    This is called to update the @arg{activatable} completely, this is called
    internally when the @code{\"related-action\"} property is set or unset and
    by the implementing class when @code{\"use-action-appearance\"} changes.
  @end{short}

  Since 2.16
  @see-class{gtk-activatable}
  @see-class{gtk-action}"
  (activatable (g-object gtk-activatable))
  (action (g-object gtk-action)))

(export 'gtk-activatable-sync-action-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_set_related_action ()
;;; ----------------------------------------------------------------------------

(deprecated-function :gtk gtk-activatable-set-related-action (3 10))

(declaim (inline gtk-activatable-set-related-action))

(defun gtk-activatable-set-related-action (activatable action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[activatable]{a @class{gtk-activatable} object}
  @argument[action]{the @class{gtk-action} object to set}
  @begin{short}
    Sets the related action on the activatable object.
  @end{short}

  @subheading{Note}
    @class{gtk-activatable} implementors need to handle the
    @code{\"related-action\"} property and call the function
    @fun{gtk-activatable-do-set-related-action} when it changes.

  Since 2.16
  @see-class{gtk-activatable}
  @see-class{gtk-action}
  @see-fun{gtk-activatable-do-set-related-action}"
  (setf (gtk-activatable-related-action activatable) action))

(export 'gtk-activatable-set-related-action)

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_set_use_action_appearance ()
;;; ----------------------------------------------------------------------------

(deprecated-function :gtk gtk-activatable-set-use-action-appearance (3 10))

(declaim (inline gtk-activatable-set-use-action-appearance))

(defun gtk-activatable-set-use-action-appearance (activatable use-appearance)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[activatable]{a @class{gtk-activatable} object}
  @argument[use-appearance]{whether to use the actions appearance}
  @begin{short}
    Sets whether this activatable should reset its layout and appearance when
    setting the related action or when the action changes appearance
  @end{short}

  @subheading{Note}
    @class{gtk-activatable} implementors need to handle the
    @code{\"use-action-appearance\"} property and call the function
    @fun{gtk-activatable-sync-action-properties} to update @arg{activatable}
    if needed.

  Since 2.16
  @see-class{gtk-activatable}
  @see-function{gtk-activatable-sync-action-properties}"
  (setf (gtk-activatable-use-action-appearance activatable) use-appearance))

(export 'gtk-activatable-set-use-action-appearance)

;;; --- End of file gtk.activatable.lisp ---------------------------------------
