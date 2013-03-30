;;; ----------------------------------------------------------------------------
;;; gtk.action-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkActionGroup
;;;
;;; A group of actions
;;;
;;; Synopsis
;;;
;;;    GtkActionGroup
;;;
;;;    gtk_action_group_new
;;;    gtk_action_group_get_name
;;;    gtk_action_group_get_sensitive
;;;    gtk_action_group_set_sensitive
;;;    gtk_action_group_get_visible
;;;    gtk_action_group_set_visible
;;;    gtk_action_group_get_accel_group
;;;    gtk_action_group_set_accel_group
;;;    gtk_action_group_get_action
;;;    gtk_action_group_list_actions
;;;    gtk_action_group_add_action
;;;    gtk_action_group_add_action_with_accel
;;;    gtk_action_group_remove_action
;;;
;;;    GtkActionEntry
;;;
;;;    gtk_action_group_add_actions
;;;    gtk_action_group_add_actions_full
;;;
;;;    GtkToggleActionEntry
;;;
;;;    gtk_action_group_add_toggle_actions
;;;    gtk_action_group_add_toggle_actions_full
;;;
;;;    GtkRadioActionEntry
;;;
;;;    gtk_action_group_add_radio_actions
;;;    gtk_action_group_add_radio_actions_full
;;;    gtk_action_group_set_translate_func
;;;    gtk_action_group_set_translation_domain
;;;    gtk_action_group_translate_string
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class gtk-action-group
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
    "visible" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-action-group 'type)
 "@version{2013-2-5}
  @begin{short}
    Actions are organised into groups. An action group is essentially a map from
    names to @class{gtk-action} objects.
  @end{short}

  All actions that would make sense to use in a particular context should be
  in a single group. Multiple action groups may be used for a particular user
  interface. In fact, it is expected that most nontrivial applications will
  make use of multiple groups. For example, in an application that can edit
  multiple documents, one group holding global actions (e.g. quit, about,
  new), and one group per document holding actions that act on that document
  (eg. save, cut/copy/paste, etc). Each window's menus would be constructed
  from a combination of two action groups.

  Accelerators are handled by the GTK+ accelerator map. All actions are
  assigned an accelerator path (which normally has the form
  <Actions>/group-name/action-name) and a shortcut is associated with this
  accelerator path. All menuitems and toolitems take on this accelerator path.
  The GTK+ accelerator map code makes sure that the correct shortcut is
  displayed next to the menu item.

  @heading{GtkActionGroup as GtkBuildable}
  The @sym{gtk-action-group} implementation of the @class{gtk-buildable}
  interface accepts @class{gtk-action} objects as @code{<child>} elements in UI
  definitions.

  Note that it is probably more common to define actions and action groups in
  the code, since they are directly related to what the code can do.

  The @sym{gtk-action-group} implementation of the @class{gtk-buildable}
  interface supports a custom @code{<accelerator>} element, which has attributes
  named key and modifiers and allows to specify accelerators. This is similar to
  the @code{<accelerator>} element of @class{gtk-widget}, the main difference is
  that it doesn't allow you to specify a signal.

  Example 82. A GtkDialog UI definition fragment.
  @begin{pre}
 <object class=\"GtkActionGroup\" id=\"actiongroup\">
   <child>
     <object class=\"GtkAction\" id=\"About\">
       <property name=\"name\">About</property>
       <property name=\"stock_id\">gtk-about</property>
       <signal handler=\"about_activate\" name=\"activate\"/>
     </object>
     <accelerator key=\"F1\" modifiers=\"GDK_CONTROL_MASK | GDK_SHIFT_MASK\"/>
   </child>
 </object>
  @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"connect-proxy\" signal}
      The ::connect-proxy signal is emitted after connecting a proxy to an
      action in the group. Note that the proxy may have been connected to a
      different action before.
      This is intended for simple customizations for which a custom action class
      would be too clumsy, e.g. showing tooltips for menuitems in the statusbar.
      GtkUIManager proxies the signal and provides global notification just
      before any action is connected to a proxy, which is probably more
      convenient to use.
      @begin{pre}
 void user_function (GtkActionGroup *action_group,
                     GtkAction      *action,
                     GtkWidget      *proxy,
                     gpointer        user_data)
      @end{pre}
      @begin[code]{table}
        @entry[action_group]{the group}
        @entry[action]{the action}
        @entry[proxy]{the proxy}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 2.4

      @subheading{The \"disconnect-proxy\" signal}
        The ::disconnect-proxy signal is emitted after disconnecting a proxy
        from an action in the group.
        GtkUIManager proxies the signal and provides global notification just
        before any action is connected to a proxy, which is probably more
        convenient to use.
        @begin{pre}
 void user_function (GtkActionGroup *action_group,
                     GtkAction      *action,
                     GtkWidget      *proxy,
                     gpointer        user_data)
        @end{pre}
        @begin[code]{table}
          @entry[action_group]{the group}
          @entry[action]{the action}
          @entry[proxy]{the proxy}
          @entry[user_data]{user data set when the signal handler was
            connected.}
        @end{table}
        Since 2.4

      @subheading{The \"post-activate\" signal}
        The ::post-activate signal is emitted just after the action in the
        action_group is activated.
        This is intended for GtkUIManager to proxy the signal and provide global
        notification just after any action is activated.
        @begin{pre}
 void user_function (GtkActionGroup *action_group,
                     GtkAction      *action,
                     gpointer        user_data)
        @end{pre}
        @begin[code]{table}
          @entry{action_group]{the group}
          @entry[action]{the action}
          @entry[user_data]{user data set when the signal handler was
            connected.}
        @end{table}
        Since 2.4

      @subheading{The \"pre-activate\" signal}
        The ::pre-activate signal is emitted just before the action in the
        action_group is activated.
        This is intended for GtkUIManager to proxy the signal and provide global
        notification just before any action is activated.
        @begin{pre}
 void user_function (GtkActionGroup *action_group,
                     GtkAction      *action,
                     gpointer        user_data)
        @end{pre}
        @begin[code]{table}
          @entry[action_group]{the group}
          @entry[action]{the action}
          @entry[user_data]{user data set when the signal handler was
            connected.}
        @end{table}
        Since 2.4
  @end{dictionary}
  @see-slot{gtk-action-group-name}
  @see-slot{gtk-action-group-sensitive}
  @see-slot{gtk-action-group-visible}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-action-group) 't)
 "The @code{\"name\"} property of type @code{gchar*}
  (Read / Write / Construct)@br{}
  A name for the action group.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sensitive"
                                               'gtk-action-group) 't)
 "The @code{\"sensitive\"} property of type @code{gboolean} (Read / Write)@br{}
  Whether the action group is enabled.@br{}
  Default value: @arg{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-action-group) 't)
 "The @code{\"visible\"} property of type @code{gboolean} (Read / Write)@br{}
  Whether the action group is visible.@br{}
  Default value: @arg{true}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-name 'function)
 "@version{2013-2-5}
  @begin{short}
    Accessor of the slot @code{\"name\"} of the @class{gtk-action-group} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-sensitive 'function)
 "@version{2013-2-5}
  @begin{short}
    Accessor of the slot @code{\"sensitive\"} of the @class{gtk-action-group}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-visible 'function)
 "@version{2013-2-5}
  @begin{short}
    Accessor of the slot @code{\"visible\"} of the @class{gtk-action-group}
    class.
  @end{short}")

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
;;; gtk-action-group-get-action
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_get_action" gtk-action-group-get-action) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-2-5}
  @argument[action-group]{the action group}
  @argument[action-name]{the name of the action}
  @return{The action, or @code{nil} if no action by that name exists.}
  @short{Looks up an action in the action group by name.}@br{}
  Since 2.4"
  (action-group (g-object gtk-action-group))
  (action-name :string))

(export 'gtk-action-group-get-action)

;;; ----------------------------------------------------------------------------
;;; gtk-action-group-list-actions
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_list_actions" gtk-action-group-list-actions)
    (g-list g-object :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-5}
  @argument[action-group]{the action group}
  @return{An allocated list of the action objects in the action group.}
  @short{Lists the actions in the action group.}@break{}
  Since 2.4"
  (action-group (g-object gtk-action-group)))

(export 'gtk-action-group-list-actions)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action ()
;;; ----------------------------------------------------------------------------

(defun gtk-action-group-add-action (action-group action &key accelerator)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-9}
  @argument[action-group]{the action group}
  @argument[action]{an action}
  @begin{short}
    Adds an action object to the action group. Note that this function does not
    set up the accel path of the action, which can lead to problems if a user
    tries to modify the accelerator of a menuitem associated with the action.
  @end{short}
  Therefore you must either set the accel path yourself with
  @fun{gtk-action-set-accel-path}, or use
  @fun{gtk-action-group-add-action-with-accel}.

  Since 2.4"
  (gtk-action-group-add-action-with-accel action-group
                                          action
                                          (if accelerator
                                              accelerator
                                              (null-pointer))))

(export 'gtk-action-group-add-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action_with_accel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_add_action_with_accel"
           gtk-action-group-add-action-with-accel) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-9}
  @argument[action-group]{the action group}
  @argument[action]{the action to add}
  @argument[accelerator]{the accelerator for the action, in the format
    understood by @fun{gtk-accelerator-parse}, or \"\" for no accelerator, or
    @code{nil} to use the stock accelerator}
  @begin{short}
    Adds an action object to the action group and sets up the accelerator.
  @end{short}

  If accelerator is @code{nil}, attempts to use the accelerator associated with
  the @code{stock_id} of the action.

  Accel paths are set to <Actions>/group-name/action-name.

  Since 2.4"
  (action-group (g-object gtk-action-group))
  (action (g-object gtk-action))
  (accelerator :string))

(export 'gtk-action-group-add-action-with-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_remove_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_remove_action" gtk-action-group-remove-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-9}
  @argument[action-group]{the action group}
  @argument[action]{an action}
  @short{Removes an action object from the action group.}
  Since 2.4"
  (action-group (g-object gtk-action-group))
  (action (g-object gtk-action)))

(export 'gtk-action-group-remove-action)

;;; ----------------------------------------------------------------------------
;;; struct GtkActionEntry
;;; ----------------------------------------------------------------------------

(defcstruct gtk-action-entry
  (name :string)
  (stock-id :string)
  (label :string)
  (accelerator :string)
  (tooltip :string)
  (callback :pointer))

(export 'gtk-action-entry)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-entry atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-action-entry atdoc:*external-symbols*)
 "@version{2013-3-29}
  @begin{short}
    @sym{gtk-action-entry} structs are used with
    @fun{gtk-action-group-add-actions} to construct actions.
  @end{short}
  @begin{pre}
(defcstruct gtk-action-entry
  (name :string)
  (stock-id :string)
  (label :string)
  (accelerator :string)
  (tooltip :string)
  (callback :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[name]{The name of the action.}
    @entry[stock-id]{The stock id for the action, or the name of an icon from
      the icon theme.}
    @entry[label]{The label for the action. This field should typically be
      marked for translation, see @fun{gtk-action-group-set-translation-domain}.
      If label is @code{nil}, the label of the stock item with id
      @code{stock-id} is used.}
    @entry[accelerator]{The accelerator for the action, in the format
      understood by @fun{gtk-accelerator-parse}.}
    @entry[tooltip]{The tooltip for the action. This field should typically be
      marked for translation, see
      @fun{gtk-action-group-set-translation-domain}.}
    @entry[callback]{The function to call when the action is activated.}
  @end{table}")

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
      (funcall (glib::get-stable-pointer-value data)
               path)
   (return-untranslated () path)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_translate_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_set_translate_func"
          %gtk-action-group-set-translate-func) :void
  (action-group (g-object gtk-action-group))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-action-group-set-translate-func (action-group func)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[action-group]{a @class{gtk-action-group} object}
  @argument[func]{a @code{GtkTranslateFunc}}
  @begin{short}
    Sets a function to be used for translating the label and tooltip of
    @symbol{gtk-action-entry}'s added by @fun{gtk-action-group-add-actions}.
  @end{short}

  If you're using @code{gettext()}, it is enough to set the translation domain
  with @fun{gtk-action-group-set-translation-domain}.

  Since 2.4
  @see-function{gtk-action-group-add-actions}
  @see-function{gtk-action-group-set-translation-domain}"
  (%gtk-action-group-set-translate-func
                             action-group
                             (callback gtk-translate-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-action-group-set-translate-func)

;;; ----------------------------------------------------------------------------
;;; gtk-action-group-set-translation-domain
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_set_translation_domain"
          gtk-action-group-set-translation-domain) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-5}
  @argument[action-group]{a @class{gtk-action-group} instance}
  @argument[domain]{the translation domain to use for @code{g_dgettext()} calls,
    or @code{nil} to use the domain set with @code{textdomain()}}
  @begin{short}
    Sets the translation domain and uses @code{g_dgettext()} for translating the
    label and tooltip of @code{GtkActionEntrys} added by
    @code{gtk_action_group_add_actions()}.@break{}
  @end{short}
  If you're not using @code{gettext()} for localization, see
  @fun{gtk-action-group-set-translate-func}.@break{}
  Since 2.4"
  (action-group (g-object gtk-action-group))
  (domain :string))

(export 'gtk-action-group-set-translation-domain)

;;; ----------------------------------------------------------------------------
;;; gtk-action-group-translate-string
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_translate_string" gtk-action-group-translate-string)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-5}
  @argument[action-group]{a @class{gtk-action-group} instance}
  @argument[string]{a string}
  @return{The translation of string.}
  @begin{short}
    Translates a string using the specified @code{translate_func()}.
  @end{short}
  This is mainly intended for language bindings.@break{}
  Since 2.6"
  (action-group (g-object gtk-action-group))
  (string (:string :free-to-foreign nil)))

(export 'gtk-action-group-translate-string)

;;; --- End of file gtk.action-group.lisp --------------------------------------
