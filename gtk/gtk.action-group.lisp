;;; ----------------------------------------------------------------------------
;;; gtk.action-group.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     A group of actions
;;;
;;; Types and Values
;;;
;;;     GtkActionGroup
;;;
;;;     GtkActionEntry
;;;     GtkToggleActionEntry
;;;     GtkRadioActionEntry
;;;
;;; Functions
;;;
;;;     gtk_action_group_new
;;;     gtk_action_group_get_name                          Accessor
;;;     gtk_action_group_get_sensitive                     Accessor
;;;     gtk_action_group_set_sensitive                     Accessor
;;;     gtk_action_group_get_visible                       Accessor
;;;     gtk_action_group_set_visible                       Accessor
;;;     gtk_action_group_get_accel_group                   Accessor
;;;     gtk_action_group_set_accel_group                   Accessor
;;;     gtk_action_group_get_action
;;;     gtk_action_group_list_actions
;;;     gtk_action_group_add_action
;;;     gtk_action_group_add_action_with_accel             not exported
;;;     gtk_action_group_remove_action
;;;     gtk_action_group_add_actions
;;;     gtk_action_group_add_actions_full                  not implemented
;;;     gtk_action_group_add_toggle_actions
;;;     gtk_action_group_add_toggle_actions_full           not implemented
;;;     gtk_action_group_add_radio_actions
;;;     gtk_action_group_add_radio_actions_full            not implemented
;;;
;;;     GtkTranslateFunc                                   Lisp callback
;;;
;;;     gtk_action_group_set_translate_func
;;;     gtk_action_group_set_translation_domain
;;;     gtk_action_group_translate_string
;;;
;;; Properties
;;;
;;; GtkAccelGroup*   accel-group    Read / Write
;;;         gchar*   name           Read / Write / Construct Only
;;;      gboolean    sensitive      Read / Write
;;;      gboolean    visible        Read / Write
;;;
;;; Signals
;;;
;;;          void    connect-proxy
;;;          void    disconnect-proxy
;;;          void    post-activate
;;;          void    pre-activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkActionGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkActionGroup implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class GtkActionGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkActionGroup" gtk-action-group
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_action_group_get_type")
  ((accel-group
    gtk-action-group-accel-group
    "accel-group" "GtkAccelGroup" t t)
   (name
    gtk-action-group-name
    "name" "gchararray" t nil)
   (sensitive
    gtk-action-group-sensitive
    "sensitive" "gboolean" t t)
   (visible
    gtk-action-group-visible
    "visible" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-action-group 'type)
 "@version{2020-1-11}
  @begin{short}
    Actions are organised into groups. An action group is essentially a map
    from names to @class{gtk-action} objects.
  @end{short}

  All actions that would make sense to use in a particular context should be
  in a single group. Multiple action groups may be used for a particular user
  interface. In fact, it is expected that most nontrivial applications will
  make use of multiple groups. For example, in an application that can edit
  multiple documents, one group holding global actions, e. g. quit, about,
  new, and one group per document holding actions that act on that document,
  e. g. save, cut, copy, paste. Each window's menus would be constructed
  from a combination of two action groups.

  @subheading{Accelerators}
  Accelerators are handled by the GTK+ accelerator map. All actions are
  assigned an accelerator path, which normally has the form
  @code{<Actions>/group-name/action-name}, and a shortcut is associated with
  this accelerator path. All menuitems and toolitems take on this accelerator
  path. The GTK+ accelerator map code makes sure that the correct shortcut is
  displayed next to the menu item.
  @begin[Warning]{dictionary}
    @sym{gtk-action-group} has been deprecated since version 3.10 and should
    not be used in newly-written code.
  @end{dictionary}
  @begin[GtkActionGroup as GtkBuildable]{dictionary}
    The @sym{gtk-action-group} implementation of the @class{gtk-buildable}
    interface accepts @class{gtk-action} objects as @code{<child>} elements in
    UI definitions.

    Note that it is probably more common to define actions and action groups in
    the code, since they are directly related to what the code can do.

    The @sym{gtk-action-group} implementation of the @class{gtk-buildable}
    interface supports a custom @code{<accelerator>} element, which has
    attributes named @code{key} and @code{modifiers} and allows to specify
    accelerators. This is similar to the @code{<accelerator>} element of
    @class{gtk-widget}, the main difference is that it doesn't allow you to
    specify a signal.

    @b{Example:} A @sym{gtk-action-group} UI definition fragment.
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
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"connect-proxy\" signal}
      @begin{pre}
 lambda (action-group action proxy)
      @end{pre}
      The \"connect-proxy\" signal is emitted after connecting a proxy to an
      action in the group. Note that the proxy may have been connected to a
      different action before.
      This is intended for simple customizations for which a custom action class
      would be too clumsy, e. g. showing tooltips for menuitems in the
      statusbar. @class{gtk-ui-manager} proxies the signal and provides global
      notification just before any action is connected to a proxy, which is
      probably more convenient to use.
      @begin[code]{table}
        @entry[action-group]{The @sym{gtk-action-group} object.}
        @entry[action]{The @class{gtk-action} object.}
        @entry[proxy]{The @class{gtk-widget} proxy.}
      @end{table}
      @subheading{The \"disconnect-proxy\" signal}
        @begin{pre}
 lambda (action-group action proxy)
        @end{pre}
        The \"disconnect-proxy\" signal is emitted after disconnecting a proxy
        from an action in the group.
        @class{gtk-ui-manager} proxies the signal and provides global
        notification just before any action is connected to a proxy, which is
        probably more convenient to use.
        @begin[code]{table}
          @entry[action-group]{The @sym{gtk-action-group} object.}
          @entry[action]{The @class{gtk-action} object.}
          @entry[proxy]{The @class{gtk-widget} proxy.}
        @end{table}
      @subheading{The \"post-activate\" signal}
        @begin{pre}
 lambda (action-group action)
        @end{pre}
        The \"post-activate\" signal is emitted just after the action in the
        action group is activated.
        This is intended for @class{gtk-ui-manager} to proxy the signal and
        provide global notification just after any action is activated.
        @begin[code]{table}
          @entry[action-group]{The @sym{gtk-action-group} object.}
          @entry[action]{The @class{gtk-action} object.}
        @end{table}
      @subheading{The \"pre-activate\" signal}
        @begin{pre}
 lambda (action-group action)
        @end{pre}
        The \"pre-activate\" signal is emitted just before the action in the
        action group is activated.
        This is intended for @class{gtk-ui-manager} to proxy the signal and
        provide global notification just before any action is activated.
        @begin[code]{table}
          @entry[action-group]{The @sym{gtk-action-group} object.}
          @entry[action]{The @class{gtk-action} object.}
        @end{table}
  @end{dictionary}
  @see-slot{gtk-action-group-accel-group}
  @see-slot{gtk-action-group-name}
  @see-slot{gtk-action-group-sensitive}
  @see-slot{gtk-action-group-visible}
  @see-class{gtk-action}
  @see-class{gtk-ui-manager}
  @see-class{gtk-accel-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-action-group-accel-group -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-group"
                                               'gtk-action-group) 't)
 "The @code{accel-group} property of type @class{gtk-accel-group}
  (Read / Write) @br{}
  The accelerator group the actions of this group should use.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-accel-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-accel-group 'function)
 "@version{2020-1-11}
  @syntax[]{(gtk-action-group-accel-group object) => accel-group}
  @syntax[]{(setf (gtk-action-group-accel-group object) accel-group)}
  @argument[object]{a @class{gtk-action-group} object}
  @argument[accel-group]{a @class{gtk-accel-group} object to set or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-action-group]{accel-group} slot of the
    @class{gtk-action-group} class.
  @end{short}

  The @sym{gtk-action-group-accel-group} slot access function
  gets the accelerator group associated with this action group or @code{nil} if
  there is none.

  The @sym{(setf gtk-action-group-accel-group)} slot access function
  sets the accelerator group to be used by every action in this group.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-accel-group} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-class{gtk-accel-group}")

;;; --- gtk-action-group-name --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-action-group) 't)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct) @br{}
  A name for the action group. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-name 'function)
 "@version{2020-1-11}
  @syntax[]{(gtk-action-group-name object) => name}
  @argument[object]{a @class{gtk-action-group} object}
  @return{The name of type @code{:string} of the action group.}
  @begin{short}
    Accessor of the @slot[gtk-action-group]{name} slot of the
    @class{gtk-action-group} class.
  @end{short}

  The @sym{gtk-action-group-name} slot access function
  gets the name of the action group.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-name} has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}")

;;; --- gtk-action-group-sensitive ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sensitive"
                                               'gtk-action-group) 't)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action group is enabled. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-sensitive 'function)
 "@version{2020-1-11}
  @syntax[]{(gtk-action-group-sensitive object) => sensitive}
  @syntax[]{(setf (gtk-action-group-sensitive object) sensitive)}
  @argument[object]{a @class{gtk-action-group} object}
  @argument[sensitive]{sensitivity of type @code{:boolean}}
  @begin{short}
    Accessor of the @slot[gtk-action-group]{sensitive} slot of the
    @class{gtk-action-group} class.
  @end{short}

  The @sym{gtk-action-group-sensitive} slot access function
  returns @arg{true} if the action group is sensitive.
  The constituent actions can only be logically sensitive, see the function
  @fun{gtk-action-is-sensitive}, if they are sensitive, see the function
  @fun{gtk-action-sensitive}, and their action group is sensitive.

  The @sym{(setf gtk-action-group-sensitive)} slot access function
  changes the sensitivity of the action group.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-sensitive} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-sensitive}
  @see-function{gtk-action-is-sensitive}")

;;; --- gtk-action-group-visible -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-action-group) 't)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action group is visible. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-group-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-group-visible 'function)
 "@version{2020-1-28}
  @syntax[]{(gtk-action-group-visible object) => visible}
  @syntax[]{(setf (gtk-action-group-visible object) visible)}
  @argument[object]{a @class{gtk-action-group} object}
  @argument[visible]{visibility of type @code{:boolean}}
  @begin{short}
    Accessor of the @slot[gtk-action-group]{visible} slot of the
    @class{gtk-action-group} class.
  @end{short}

  The @sym{gtk-action-group-sensitive} slot access function
  returns @arg{true} if the action group is visible.
  The constituent actions can only be logically visible, see the function
  @fun{gtk-action-is-visible}, if they are visible, see the function
  @fun{gtk-action-visible}, and their action group is visible.

  The @sym{(setf gtk-action-group-sensitive)} slot access function
  changes the visibility of the action group.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-visible} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-visible}
  @see-function{gtk-action-is-visible}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-group-new))

(defun gtk-action-group-new (name)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-28}
  @argument[name]{a @code{:string} with the name of the action group}
  @return{The new @class{gtk-action-group} object.}
  @begin{short}
    Creates a new @class{gtk-action-group} object. The name of the action
    group is used when associating keybindings with the actions.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-new} has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}"
  (make-instance 'gtk-action-group
                 :name name))

(export 'gtk-action-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_get_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_get_action" gtk-action-group-get-action) g-object
 #+cl-cffi-gtk-documentation
 "@version{2020-1-11}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[action-name]{the name of type @code{:string} of the action}
  @return{The @class{gtk-action} object, or @code{nil} if no action by that name
    exists.}
  @short{Looks up an action in the action group by name.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-get-action} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-action}"
  (action-group (g-object gtk-action-group))
  (action-name :string))

(export 'gtk-action-group-get-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_list_actions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_list_actions" gtk-action-group-list-actions)
    (g-list g-object :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-11}
  @argument[action-group]{the @class{gtk-action-group} object}
  @return{A list of the @class{gtk-action} objects in the action group.}
  @short{Lists the actions in the action group.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-list-actions} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-action}"
  (action-group (g-object gtk-action-group)))

(export 'gtk-action-group-list-actions)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action ()
;;; ----------------------------------------------------------------------------

(defun gtk-action-group-add-action (action-group action &optional accelerator)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-28}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[action]{the @class{gtk-action} object to add}
  @argument[accelerator]{a @code{:string} with the optional accelerator for the
    action, in the format understood by the function
    @fun{gtk-accelerator-parse}, or \"\" for no accelerator, or @code{nil} to
    use the stock accelerator}
  @begin{short}
    Adds an action object to the action group and sets up the accelerator.
  @end{short}

  If @arg{accelerator} is @code{nil}, this is the default value, attempts to
  use the accelerator associated with the stock ID of the action.

  Accel paths are set to @code{<Actions>/group-name/action-name}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-add-action} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-accelerator-parse}"
  (%gtk-action-group-add-action-with-accel action-group
                                           action
                                           (if accelerator
                                               accelerator
                                               (null-pointer))))

(export 'gtk-action-group-add-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action_with_accel ()
;;; ----------------------------------------------------------------------------

;; This function is called from gtk-action-group-add-action and not exported.

(defcfun ("gtk_action_group_add_action_with_accel"
          %gtk-action-group-add-action-with-accel) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-11}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[action]{the @class{gtk-action} object to add}
  @argument[accelerator]{the accelerator for the action, in the format
    understood by the @fun{gtk-accelerator-parse} function, or \"\" for no
    accelerator, or @code{nil} to use the stock accelerator}
  @begin{short}
    Adds an action object to the action group and sets up the accelerator.
  @end{short}
  If @arg{accelerator} is @code{nil}, attempts to use the accelerator associated
  with the @code{stock-id} of the action.

  Accel paths are set to <Actions>/group-name/action-name.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-add-action-with-accel} has been
    deprecated since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-action}
  @see-function{gtk-accelerator-parse}"
  (action-group (g-object gtk-action-group))
  (action (g-object gtk-action))
  (accelerator :string))

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_remove_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_remove_action" gtk-action-group-remove-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-11}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[action]{a @class{gtk-action} object}
  @short{Removes an action object from the action group.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-remove-action} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-class{gtk-action}
  @see-function{gtk-action-group-add-action}"
  (action-group (g-object gtk-action-group))
  (action (g-object gtk-action)))

(export 'gtk-action-group-remove-action)

;;; ----------------------------------------------------------------------------
;;; struct GtkActionEntry
;;; ----------------------------------------------------------------------------

;; This structure is not used in the Lisp binding and not exported.

(defcstruct gtk-action-entry
  (name :string)
  (stock-id :string)
  (label :string)
  (accelerator :string)
  (tooltip :string)
  (callback :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-entry atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-action-entry atdoc:*external-symbols*)
 "@version{2013-6-2}
  @begin{short}
    @sym{gtk-action-entry} structures are used with the
    @fun{gtk-action-group-add-actions} function to construct actions.
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
    @entry[stock-id]{The stock ID for the action, or the name of an icon from
      the icon theme.}
    @entry[label]{The label for the action. This field should typically be
      marked for translation, see the
      @fun{gtk-action-group-set-translation-domain} function. If label is
      @code{nil}, the label of the stock item with ID @code{stock-id} is used.}
    @entry[accelerator]{The accelerator for the action, in the format
      understood by the @fun{gtk-accelerator-parse} function.}
    @entry[tooltip]{The tooltip for the action. This field should typically be
      marked for translation, see the
      @fun{gtk-action-group-set-translation-domain} function.}
    @entry[callback]{The function to call when the action is activated.}
  @end{table}
  @see-function{gtk-action-group-set-translation-domain}
  @see-function{gtk-accelerator-parse}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_actions ()
;;; ----------------------------------------------------------------------------

(defun gtk-action-group-add-actions (action-group entries)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-11}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[entries]{a list of action descriptions}
  @begin{short}
    This is a convenience function to create a number of actions and add them
    to the action group.
  @end{short}

  The \"activate\" signals of the actions are connected to the callbacks and
  their accel paths are set to @code{<Actions>/group-name/action-name}.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((group (gtk-action-group-new \"AppWindowActions\"))
      (actions '((\"Open\"            ; name
                  \"gtk-stock-open\"  ; stock-id
                  \"_Open\"           ; label
                  \"<ctrl>o\"         ; accelerator
                  \"Open a file\"     ; tooltip
                  nil               ; callback function
                ))))
  (gtk-action-group-add-actions group actions)
  ... )
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-add-actions} has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-class{gtk-action}
  @see-function{gtk-action-group-add-action}"
  (dolist (entry entries)
    (let ((action (make-instance 'gtk-action
                                 :name (first entry)
                                 :stock-id (if (second entry)
                                               (second entry)
                                               (null-pointer))
                                 :label (if (third entry)
                                            (third entry)
                                            (null-pointer))
                                 :tooltip (if (fifth entry)
                                              (fifth entry)
                                              (null-pointer)))))
      (gtk-action-group-add-action action-group action (fourth entry))
      (let ((func (sixth entry)))
        (when func
          (g-signal-connect action "activate" func))))))

(export 'gtk-action-group-add-actions)

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
;;; ----------------------------------------------------------------------------

(defun gtk-action-group-add-toggle-actions (action-group entries)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-11}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[entries]{a list of toggle action descriptions}
  @begin{short}
    This is a convenience function to create a number of toggle actions and add
    them to the action group.
  @end{short}

  The \"activate\" signals of the actions are connected to the callbacks and
  their accel paths are set to @code{<Actions>/group-name/action-name}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-add-toggle-actions} has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-class{gtk-toggle-action}
  @see-function{gtk-action-group-add-action}"
  (dolist (entry entries)
    (let ((action (make-instance 'gtk-toggle-action
                                 :name (first entry)
                                 :stock-id (if (second entry)
                                               (second entry)
                                               (null-pointer))
                                 :label (if (third entry)
                                            (third entry)
                                            (null-pointer))
                                 :tooltip (if (fifth entry)
                                              (fifth entry)
                                              (null-pointer))
                                 :active (seventh entry))))
      (gtk-action-group-add-action action-group action (fourth entry))
      (let ((func (sixth entry)))
        (when func
          (g-signal-connect action "activate" func))))))

(export 'gtk-action-group-add-toggle-actions)

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
;;; ----------------------------------------------------------------------------

;; TODO: This Lisp implementation is not complete.
;;       See the C code for missing features.

(defun gtk-action-group-add-radio-actions (action-group entries value on-change)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-28}
  @argument[action-group]{the @class{gtk-action-group} object}
  @argument[entries]{a list of radio action descriptions}
  @argument[value]{the value of type @code{:int} of the action to activate
    initially, or -1 if no action should be activated}
  @argument[on-change]{the callback to connect to the changed signal}
  @begin{short}
    This is a convenience function to create a group of radio actions and add
    them to the action group.
  @end{short}

  The \"changed\" signal of the first radio action is connected to the
  @arg{on-change} callback and the accel paths of the actions are set to
  @code{<Actions>/group-name/action-name}.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((group (gtk-action-group-new \"AppWindowActions\"))
      (actions (list
                 (list \"Red\" nil                      ; name, stock id
                       \"_Red\" \"<control>R\"            ; label, accelerator
                       \"Blood\" 0)                     ; tooltip, value
                 (list \"Green\" nil                    ; name, stock id
                       \"_Green\" \"<control>G\"          ; label, accelerator
                       \"Grass\" 1)                     ; tooltip, value
                 (list \"Blue\" nil                     ; name, stock id
                       \"_Blue\" \"<control>B\"           ; label, accelerator
                       \"Sky\" 2))))
    (gtk-action-group-add-radio-actions group actions 0 nil)
    ... )
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-add-radio-actions} has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-class{gtk-radio-action}
  @see-function{gtk-action-group-add-action}"
  (let ((last-action nil)
        (first-action nil))
    (dolist (entry entries)
      (let ((action (make-instance 'gtk-radio-action
                                   :name (first entry)
                                   :stock-id (if (second entry)
                                                 (second entry)
                                                 (null-pointer))
                                   :label (if (third entry)
                                              (third entry)
                                              (null-pointer))
                                   :tooltip (if (fifth entry)
                                                (fifth entry)
                                                (null-pointer))
                                   :value (sixth entry)
                                   :active nil)))
        (unless first-action
          (setf first-action action))
        (gtk-radio-action-join-group action last-action)
        (setf last-action action)
        (if (eql value (sixth entry))
            (setf (gtk-toggle-action-active action) t)
            (setf (gtk-toggle-action-active action) nil))
        (gtk-action-group-add-action action-group action (fourth entry))))
    (when on-change
      (g-signal-connect first-action "changed" on-change))))

(export 'gtk-action-group-add-radio-actions)

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
 "@version{2020-1-11}
  @argument[action-group]{a @class{gtk-action-group} object}
  @argument[func]{a @code{GtkTranslateFunc}}
  @begin{short}
    Sets a function to be used for translating the label and tooltip of
    action entires added by the function @fun{gtk-action-group-add-actions}.
  @end{short}

  If you are using @code{gettext()}, it is enough to set the translation domain
  with the @fun{gtk-action-group-set-translation-domain} function.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-set-translate-func} has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-actions}
  @see-function{gtk-action-group-set-translation-domain}"
  (%gtk-action-group-set-translate-func
                              action-group
                              (callback gtk-translate-func-cb)
                              (glib:allocate-stable-pointer func)
                              (callback glib:stable-pointer-destroy-notify-cb)))

(export 'gtk-action-group-set-translate-func)

;;; ----------------------------------------------------------------------------
;;; gtk-action-group-set-translation-domain
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_set_translation_domain"
          gtk-action-group-set-translation-domain) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-28}
  @argument[action-group]{a @class{gtk-action-group} object}
  @argument[domain]{the translation domain of type @code{:string} to use for
    @code{g_dgettext()} calls, or @code{nil} to use the domain set with
    @code{textdomain()}}
  @begin{short}
    Sets the translation domain and uses @code{g_dgettext()} for translating
    the label and tooltip of action entries added by the function
    @fun{gtk-action-group-add-actions}.
  @end{short}

  If you are not using @code{gettext()} for localization, see the
  @fun{gtk-action-group-set-translate-func} function.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-set-translate-domain} has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-actions}
  @see-function{gtk-action-group-set-translate-func}"
  (action-group (g-object gtk-action-group))
  (domain :string))

(export 'gtk-action-group-set-translation-domain)

;;; ----------------------------------------------------------------------------
;;; gtk-action-group-translate-string
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_group_translate_string" gtk-action-group-translate-string)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-28}
  @argument[action-group]{a @class{gtk-action-group} object}
  @argument[string]{a @code{:string}}
  @return{The translation of @arg{string}.}
  @begin{short}
    Translates a string using the specified @code{translate_func()}.
  @end{short}
  This is mainly intended for language bindings.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-group-translate-string} has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-action-group}"
  (action-group (g-object gtk-action-group))
  (string (:string :free-to-foreign nil)))

(export 'gtk-action-group-translate-string)

;;; --- End of file gtk.action-group.lisp --------------------------------------
