;;; ----------------------------------------------------------------------------
;;; gtk.action-group.lisp
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
;;;﻿
;;; GtkActionGroup
;;;
;;; A group of actions
;;;
;;; Synopsis
;;;
;;;     GtkActionGroup
;;;
;;;     gtk_action_group_new
;;;     gtk_action_group_get_name
;;;     gtk_action_group_get_sensitive
;;;     gtk_action_group_set_sensitive
;;;     gtk_action_group_get_visible
;;;     gtk_action_group_set_visible
;;;     gtk_action_group_get_action
;;;     gtk_action_group_list_actions
;;;     gtk_action_group_add_action
;;;     gtk_action_group_add_action_with_accel
;;;     gtk_action_group_remove_action
;;;
;;;     GtkActionEntry
;;;
;;;     gtk_action_group_add_actions
;;;     gtk_action_group_add_actions_full
;;;
;;;     GtkToggleActionEntry
;;;
;;;     gtk_action_group_add_toggle_actions
;;;     gtk_action_group_add_toggle_actions_full
;;;
;;;     GtkRadioActionEntry
;;;
;;;     gtk_action_group_add_radio_actions
;;;     gtk_action_group_add_radio_actions_full
;;;     gtk_action_group_set_translate_func
;;;     gtk_action_group_set_translation_domain
;;;     gtk_action_group_translate_string
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkActionGroup
;;;
;;; Implemented Interfaces
;;;
;;; GtkActionGroup implements GtkBuildable.
;;;
;;; Properties
;;;
;;;   "name"                     gchar*               : Read / Write / Construct
;;;   "sensitive"                gboolean             : Read / Write
;;;   "visible"                  gboolean             : Read / Write
;;;
;;; Signals
;;;
;;;   "connect-proxy"
;;;   "disconnect-proxy"
;;;   "post-activate"
;;;   "pre-activate"
;;;
;;; Description
;;;
;;; Actions are organised into groups. An action group is essentially a map from
;;; names to GtkAction objects.
;;;
;;; All actions that would make sense to use in a particular context should be
;;; in a single group. Multiple action groups may be used for a particular user
;;; interface. In fact, it is expected that most nontrivial applications will
;;; make use of multiple groups. For example, in an application that can edit
;;; multiple documents, one group holding global actions (e.g. quit, about,
;;; new), and one group per document holding actions that act on that document
;;; (eg. save, cut/copy/paste, etc). Each window's menus would be constructed
;;; from a combination of two action groups.
;;;
;;; Accelerators are handled by the GTK+ accelerator map. All actions are
;;; assigned an accelerator path (which normally has the form
;;; <Actions>/group-name/action-name) and a shortcut is associated with this
;;; accelerator path. All menuitems and toolitems take on this accelerator path.
;;; The GTK+ accelerator map code makes sure that the correct shortcut is
;;; displayed next to the menu item.
;;;
;;; GtkActionGroup as GtkBuildable
;;;
;;; The GtkActionGroup implementation of the GtkBuildable interface accepts
;;; GtkAction objects as <child> elements in UI definitions.
;;;
;;; Note that it is probably more common to define actions and action groups in
;;; the code, since they are directly related to what the code can do.
;;;
;;; The GtkActionGroup implementation of the GtkBuildable interface supports a
;;; custom <accelerator> element, which has attributes named key and modifiers
;;; and allows to specify accelerators. This is similar to the <accelerator>
;;; element of GtkWidget, the main difference is that it doesn't allow you to
;;; specify a signal.
;;;
;;; Example 82. A GtkDialog UI definition fragment.
;;;
;;;   <object class="GtkActionGroup" id="actiongroup">
;;;     <child>
;;;       <object class="GtkAction" id="About">
;;;           <property name="name">About</property>
;;;           <property name="stock_id">gtk-about</property>
;;;           <signal handler="about_activate" name="activate"/>
;;;       </object>
;;;       <accelerator key="F1" modifiers="GDK_CONTROL_MASK | GDK_SHIFT_MASK"/>
;;;     </child>
;;;   </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "name" property
;;;
;;;   "name"                     gchar*               : Read / Write / Construct
;;;
;;; A name for the action group.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "sensitive" property
;;;
;;;   "sensitive"                gboolean              : Read / Write
;;;
;;; Whether the action group is enabled.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible" property
;;;
;;;   "visible"                  gboolean              : Read / Write
;;;
;;; Whether the action group is visible.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "connect-proxy" signal
;;;
;;; void user_function (GtkActionGroup *action_group,
;;;                     GtkAction      *action,
;;;                     GtkWidget      *proxy,
;;;                     gpointer        user_data)
;;;
;;; The ::connect-proxy signal is emitted after connecting a proxy to an action
;;; in the group. Note that the proxy may have been connected to a different
;;; action before.
;;;
;;; This is intended for simple customizations for which a custom action class
;;; would be too clumsy, e.g. showing tooltips for menuitems in the statusbar.
;;;
;;; GtkUIManager proxies the signal and provides global notification just before
;;; any action is connected to a proxy, which is probably more convenient to
;;; use.
;;;
;;; action_group :
;;;     the group
;;;
;;; action :
;;;     the action
;;;
;;; proxy :
;;;     the proxy
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "disconnect-proxy" signal
;;;
;;; void user_function (GtkActionGroup *action_group,
;;;                     GtkAction      *action,
;;;                     GtkWidget      *proxy,
;;;                     gpointer        user_data)
;;;
;;; The ::disconnect-proxy signal is emitted after disconnecting a proxy from an
;;; action in the group.
;;;
;;; GtkUIManager proxies the signal and provides global notification just before
;;; any action is connected to a proxy, which is probably more convenient to
;;; use.
;;;
;;; action_group :
;;;     the group
;;;
;;; action :
;;;     the action
;;;
;;; proxy :
;;;     the proxy
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "post-activate" signal
;;;
;;; void user_function (GtkActionGroup *action_group,
;;;                     GtkAction      *action,
;;;                     gpointer        user_data)
;;;
;;; The ::post-activate signal is emitted just after the action in the
;;; action_group is activated
;;;
;;; This is intended for GtkUIManager to proxy the signal and provide global
;;; notification just after any action is activated.
;;;
;;; action_group :
;;;     the group
;;;
;;; action :
;;;     the action
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "pre-activate" signal
;;;
;;; void user_function (GtkActionGroup *action_group,
;;;                     GtkAction      *action,
;;;                     gpointer        user_data)
;;;
;;; The ::pre-activate signal is emitted just before the action in the
;;; action_group is activated
;;;
;;; This is intended for GtkUIManager to proxy the signal and provide global
;;; notification just before any action is activated.
;;;
;;; action_group :
;;;     the group
;;;
;;; action :
;;;     the action
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkActionGroup
;;;
;;; struct GtkActionGroup;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkActionGroup" gtk-action-group
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_action_group_get_type")
  ((name
    gtk-action-group-name
    "name" "gchararray" t nil)
   (sensitive
    gtk-action-group-sensitive
    "sensitive" "gboolean" t t)
   (visible
    gtk-action-group-visible
    "visible" "gboolean" t t)
   (:cffi translate-function
          gtk-action-group-translate-function nil
          nil gtk-action-group-set-translate-func)
   (:cffi translation-domain
          gtk-action-group-translation-domain nil
          nil gtk-action-group-set-translation-domain)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_new ()
;;;
;;; GtkActionGroup * gtk_action_group_new (const gchar *name);
;;;
;;; Creates a new GtkActionGroup object. The name of the action group is used
;;; when associating keybindings with the actions.
;;;
;;; name :
;;;     the name of the action group.
;;;
;;; Returns :
;;;     the new GtkActionGroup
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_get_name ()
;;;
;;; const gchar * gtk_action_group_get_name (GtkActionGroup *action_group);
;;;
;;; Gets the name of the action group.
;;;
;;; action_group :
;;;     the action group
;;;
;;; Returns :
;;;     the name of the action group.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_get_sensitive ()
;;;
;;; gboolean gtk_action_group_get_sensitive (GtkActionGroup *action_group);
;;;
;;; Returns TRUE if the group is sensitive. The constituent actions can only be
;;; logically sensitive (see gtk_action_is_sensitive()) if they are sensitive
;;; (see gtk_action_get_sensitive()) and their group is sensitive.
;;;
;;; action_group :
;;;     the action group
;;;
;;; Returns :
;;;     TRUE if the group is sensitive.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_sensitive ()
;;;
;;; void gtk_action_group_set_sensitive (GtkActionGroup *action_group,
;;;                                      gboolean sensitive);
;;;
;;; Changes the sensitivity of action_group
;;;
;;; action_group :
;;;     the action group
;;;
;;; sensitive :
;;;     new sensitivity
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_get_visible ()
;;;
;;; gboolean gtk_action_group_get_visible (GtkActionGroup *action_group);
;;;
;;; Returns TRUE if the group is visible. The constituent actions can only be
;;; logically visible (see gtk_action_is_visible()) if they are visible (see
;;; gtk_action_get_visible()) and their group is visible.
;;;
;;; action_group :
;;;     the action group
;;;
;;; Returns :
;;;     TRUE if the group is visible.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_visible ()
;;;
;;; void gtk_action_group_set_visible (GtkActionGroup *action_group,
;;;                                    gboolean visible);
;;;
;;; Changes the visible of action_group.
;;;
;;; action_group :
;;;     the action group
;;;
;;; visible :
;;;     new visiblity
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_get_action ()
;;;
;;; GtkAction * gtk_action_group_get_action (GtkActionGroup *action_group,
;;;                                          const gchar *action_name);
;;;
;;; Looks up an action in the action group by name.
;;;
;;; action_group :
;;;     the action group
;;;
;;; action_name :
;;;     the name of the action
;;;
;;; Returns :
;;;     the action, or NULL if no action by that name exists
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_get_action" gtk-action-group-get-action) g-object
  (action-group g-object)
  (action-name :string))

(export 'gtk-action-group-get-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_list_actions ()
;;;
;;; GList * gtk_action_group_list_actions (GtkActionGroup *action_group);
;;;
;;; Lists the actions in the action group.
;;;
;;; action_group :
;;;     the action group
;;;
;;; Returns :
;;;     an allocated list of the action objects in the action group
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_list_actions" gtk-action-group-list-actions)
    (g-list g-object :free-from-foreign t)
  (action-group g-object))

(export 'gtk-action-group-lisp-actions)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action ()
;;;
;;; void gtk_action_group_add_action (GtkActionGroup *action_group,
;;;                                   GtkAction *action);
;;;
;;; Adds an action object to the action group. Note that this function does not
;;; set up the accel path of the action, which can lead to problems if a user
;;; tries to modify the accelerator of a menuitem associated with the action.
;;; Therefore you must either set the accel path yourself with
;;; gtk_action_set_accel_path(), or use
;;; gtk_action_group_add_action_with_accel (..., NULL).
;;;
;;; action_group :
;;;     the action group
;;;
;;; action :
;;;     an action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defun gtk-action-group-add-action (action-group action &key accelerator)
  (gtk-action-group-add-action-with-accel action-group
                                          action
                                          (if accelerator
                                              accelerator
                                              (null-pointer))))

(export 'gtk-action-group-add-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action_with_accel ()
;;;
;;; void gtk_action_group_add_action_with_accel (GtkActionGroup *action_group,
;;;                                              GtkAction *action,
;;;                                              const gchar *accelerator);
;;;
;;; Adds an action object to the action group and sets up the accelerator.
;;;
;;; If accelerator is NULL, attempts to use the accelerator associated with the
;;; stock_id of the action.
;;;
;;; Accel paths are set to <Actions>/group-name/action-name.
;;;
;;; action_group :
;;;     the action group
;;;
;;; action :
;;;     the action to add
;;;
;;; accelerator :
;;;     the accelerator for the action, in the format understood by
;;;     gtk_accelerator_parse(), or "" for no accelerator, or NULL to use the
;;;     stock accelerator
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_add_action_with_accel"
          gtk-action-group-add-action-with-accel) :void
  (action-group g-object)
  (action g-object)
  (accelerator :string))

(export 'gtk-action-group-add-action-with-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_remove_action ()
;;;
;;; void gtk_action_group_remove_action (GtkActionGroup *action_group,
;;;                                      GtkAction *action);
;;;
;;; Removes an action object from the action group.
;;;
;;; action_group :
;;;     the action group
;;;
;;; action :
;;;     an action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_remove_action" gtk-action-group-remove-action) :void
  (action-group g-object)
  (action g-object))

(export 'gtk-action-group-remove-action)

;;; ----------------------------------------------------------------------------
;;; struct GtkActionEntry
;;;
;;; struct GtkActionEntry {
;;;   const gchar     *name;
;;;   const gchar     *stock_id;
;;;   const gchar     *label;
;;;   const gchar     *accelerator;
;;;   const gchar     *tooltip;
;;;   GCallback  callback;
;;; };
;;;
;;; GtkActionEntry structs are used with gtk_action_group_add_actions() to
;;; construct actions.
;;;
;;; const gchar *name;
;;;     The name of the action.
;;;
;;; const gchar *stock_id;
;;;     The stock id for the action, or the name of an icon from the icon theme.
;;;
;;; const gchar *label;
;;;     The label for the action. This field should typically be marked for
;;;     translation, see gtk_action_group_set_translation_domain(). If label is
;;;     NULL, the label of the stock item with id stock_id is used.
;;;
;;; const gchar *accelerator;
;;;     The accelerator for the action, in the format understood by
;;;     gtk_accelerator_parse().
;;;
;;; const gchar *tooltip;
;;;     The tooltip for the action. This field should typically be marked for
;;;     translation, see gtk_action_group_set_translation_domain().
;;;
;;; GCallback callback;
;;;     The function to call when the action is activated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_actions ()
;;;
;;; void gtk_action_group_add_actions (GtkActionGroup *action_group,
;;;                                    const GtkActionEntry *entries,
;;;                                    guint n_entries,
;;;                                    gpointer user_data);
;;;
;;; This is a convenience function to create a number of actions and add them to
;;; the action group.
;;;
;;; The "activate" signals of the actions are connected to the callbacks and
;;; their accel paths are set to <Actions>/group-name/action-name.
;;;
;;; action_group :
;;;     the action group
;;;
;;; entries :
;;;     an array of action descriptions
;;;
;;; n_entries :
;;;     the number of entries
;;;
;;; user_data :
;;;     data to pass to the action callbacks
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_actions_full ()
;;;
;;; void gtk_action_group_add_actions_full (GtkActionGroup *action_group,
;;;                                         const GtkActionEntry *entries,
;;;                                         guint n_entries,
;;;                                         gpointer user_data,
;;;                                         GDestroyNotify destroy);
;;;
;;; This variant of gtk_action_group_add_actions() adds a GDestroyNotify
;;; callback for user_data.
;;;
;;; action_group :
;;;     the action group
;;;
;;; entries :
;;;     an array of action descriptions
;;;
;;; n_entries :
;;;     the number of entries
;;;
;;; user_data :
;;;     data to pass to the action callbacks
;;;
;;; destroy :
;;;     destroy notification callback for user_data
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleActionEntry
;;;
;;; struct GtkToggleActionEntry {
;;;   const gchar     *name;
;;;   const gchar     *stock_id;
;;;   const gchar     *label;
;;;   const gchar     *accelerator;
;;;   const gchar     *tooltip;
;;;   GCallback  callback;
;;;   gboolean   is_active;
;;; };
;;;
;;; GtkToggleActionEntry structs are used with
;;; gtk_action_group_add_toggle_actions() to construct toggle actions.
;;;
;;; const gchar *name;
;;;     The name of the action.
;;;
;;; const gchar *stock_id;
;;;     The stock id for the action, or the name of an icon from the icon theme.
;;;
;;; const gchar *label;
;;;     The label for the action. This field should typically be marked for
;;;     translation, see gtk_action_group_set_translation_domain().
;;;
;;; const gchar *accelerator;
;;;     The accelerator for the action, in the format understood by
;;;     gtk_accelerator_parse().
;;;
;;; const gchar *tooltip;
;;;     The tooltip for the action. This field should typically be marked for
;;;     translation, see gtk_action_group_set_translation_domain().
;;;
;;; GCallback callback;
;;;     The function to call when the action is activated.
;;;
;;; gboolean is_active;
;;;     The initial state of the toggle action.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_toggle_actions ()
;;;
;;; void gtk_action_group_add_toggle_actions
;;;                                        (GtkActionGroup *action_group,
;;;                                         const GtkToggleActionEntry *entries,
;;;                                         guint n_entries,
;;;                                         gpointer user_data);
;;;
;;; This is a convenience function to create a number of toggle actions and add
;;; them to the action group.
;;;
;;; The "activate" signals of the actions are connected to the callbacks and
;;; their accel paths are set to <Actions>/group-name/action-name.
;;;
;;; action_group :
;;;     the action group
;;;
;;; entries :
;;;     an array of toggle action descriptions
;;;
;;; n_entries :
;;;     the number of entries
;;;
;;; user_data :
;;;     data to pass to the action callbacks
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_toggle_actions_full ()
;;;
;;; void gtk_action_group_add_toggle_actions_full
;;;                                        (GtkActionGroup *action_group,
;;;                                         const GtkToggleActionEntry *entries,
;;;                                         guint n_entries,
;;;                                         gpointer user_data,
;;;                                         GDestroyNotify destroy);
;;;
;;; This variant of gtk_action_group_add_toggle_actions() adds a GDestroyNotify
;;; callback for user_data.
;;;
;;; action_group :
;;;     the action group
;;;
;;; entries :
;;;     an array of toggle action descriptions
;;;
;;; n_entries :
;;;     the number of entries
;;;
;;; user_data :
;;;     data to pass to the action callbacks
;;;
;;; destroy :
;;;     destroy notification callback for user_data
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioActionEntry
;;;
;;; struct GtkRadioActionEntry {
;;;   const gchar *name;
;;;   const gchar *stock_id;
;;;   const gchar *label;
;;;   const gchar *accelerator;
;;;   const gchar *tooltip;
;;;   gint   value;
;;; };
;;;
;;; GtkRadioActionEntry structs are used with
;;; gtk_action_group_add_radio_actions() to construct groups of radio actions.
;;;
;;; const gchar *name;
;;;     The name of the action.
;;;
;;; const gchar *stock_id;
;;;     The stock id for the action, or the name of an icon from the icon theme.
;;;
;;; const gchar *label;
;;;     The label for the action. This field should typically be marked for
;;;     translation, see gtk_action_group_set_translation_domain().
;;;
;;; const gchar *accelerator;
;;;     The accelerator for the action, in the format understood by
;;;     gtk_accelerator_parse().
;;;
;;; const gchar *tooltip;
;;;     The tooltip for the action. This field should typically be marked for
;;;     translation, see gtk_action_group_set_translation_domain().
;;;
;;; gint value;
;;;     The value to set on the radio action. See
;;;     gtk_radio_action_get_current_value().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_radio_actions ()
;;;
;;; void gtk_action_group_add_radio_actions (GtkActionGroup *action_group,
;;;                                          const GtkRadioActionEntry *entries,
;;;                                          guint n_entries,
;;;                                          gint value,
;;;                                          GCallback on_change,
;;;                                          gpointer user_data);
;;;
;;; This is a convenience routine to create a group of radio actions and add
;;; them to the action group.
;;;
;;; The "changed" signal of the first radio action is connected to the on_change
;;; callback and the accel paths of the actions are set to
;;; <Actions>/group-name/action-name.
;;;
;;; action_group :
;;;     the action group
;;;
;;; entries :
;;;     an array of radio action descriptions
;;;
;;; n_entries :
;;;     the number of entries
;;;
;;; value :
;;;     the value of the action to activate initially, or -1 if no action should
;;;     be activated
;;;
;;; on_change :
;;;     the callback to connect to the changed signal
;;;
;;; user_data :
;;;     data to pass to the action callbacks
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_radio_actions_full ()
;;;
;;; void gtk_action_group_add_radio_actions_full
;;;                                         (GtkActionGroup *action_group,
;;;                                          const GtkRadioActionEntry *entries,
;;;                                          guint n_entries,
;;;                                          gint value,
;;;                                          GCallback on_change,
;;;                                          gpointer user_data,
;;;                                          GDestroyNotify destroy);
;;;
;;; This variant of gtk_action_group_add_radio_actions() adds a GDestroyNotify
;;; callback for user_data.
;;;
;;; action_group :
;;;     the action group
;;;
;;; entries :
;;;     an array of radio action descriptions
;;;
;;; n_entries :
;;;     the number of entries
;;;
;;; value :
;;;     the value of the action to activate initially, or -1 if no action should
;;;     be activated
;;;
;;; on_change :
;;;     the callback to connect to the changed signal
;;;
;;; user_data :
;;;     data to pass to the action callbacks
;;;
;;; destroy :
;;;     destroy notification callback for user_data
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTranslateFunc ()
;;;
;;; gchar * (*GtkTranslateFunc) (const gchar *path, gpointer func_data);
;;; ----------------------------------------------------------------------------

(defcallback gtk-translate-func-cb (:string :free-to-foreign nil
                                            :free-from-foreign nil)
    ((path (:string :free-from-foreign nil)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               path)
   (return-untranslated () path)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_translate_func ()
;;;
;;; void gtk_action_group_set_translate_func (GtkActionGroup *action_group,
;;;                                           GtkTranslateFunc func,
;;;                                           gpointer data,
;;;                                           GDestroyNotify notify);
;;;
;;; Sets a function to be used for translating the label and tooltip of
;;; GtkActionGroupEntrys added by gtk_action_group_add_actions().
;;;
;;; If you're using gettext(), it is enough to set the translation domain with
;;; gtk_action_group_set_translation_domain().
;;;
;;; action_group :
;;;     a GtkActionGroup
;;;
;;; func :
;;;     a GtkTranslateFunc
;;;
;;; data :
;;;     data to be passed to func and notify
;;;
;;; notify :
;;;     a GDestroyNotify function to be called when action_group is destroyed
;;;     and when the translation function is changed again
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_set_translate_func"
          %gtk-action-group-set-translate-func) :void
  (action-group g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-action-group-set-translate-func (action-group func)
  (%gtk-action-group-set-translate-func
                                   action-group
                                   (callback gtk-translate-func-cb)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-action-group-set-translate-func)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_translation_domain ()
;;;
;;; void gtk_action_group_set_translation_domain (GtkActionGroup *action_group,
;;;                                               const gchar *domain);
;;;
;;; Sets the translation domain and uses g_dgettext() for translating the label
;;; and tooltip of GtkActionEntrys added by gtk_action_group_add_actions().
;;;
;;; If you're not using gettext() for localization, see
;;; gtk_action_group_set_translate_func().
;;;
;;; action_group :
;;;     a GtkActionGroup
;;;
;;; domain :
;;;     the translation domain to use for g_dgettext() calls, or NULL to use the
;;;     domain set with textdomain()
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_set_translation_domain"
          gtk-action-group-set-translation-domain) :void
  (action-group g-object)
  (domain :string))

(export 'gtk-action-group-set-translation-domain)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_translate_string ()
;;;
;;; const gchar * gtk_action_group_translate_string
;;;                                               (GtkActionGroup *action_group,
;;;                                                const gchar *string);
;;;
;;; Translates a string using the specified translate_func(). This is mainly
;;; intended for language bindings.
;;;
;;; action_group :
;;;     a GtkActionGroup
;;;
;;; string :
;;;     a string
;;;
;;; Returns :
;;;     the translation of string
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_translate_string" gtk-action-group-translate-string)
    (:string :free-from-foreign nil)
  (action-group g-object)
  (string (:string :free-to-foreign nil)))

(export 'gtk-action-group-translate-string)

;;; --- End of file gtk.action-group.lisp --------------------------------------
