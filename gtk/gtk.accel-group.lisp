;;; ----------------------------------------------------------------------------
;;; gtk.accel-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; Accelerator Groups
;;;
;;; Groups of global keyboard accelerators for an entire GtkWindow
;;;
;;; Synopsis
;;;
;;;     GtkAccelGroup
;;;
;;;     gtk_accel_group_new
;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;     gtk_accel_group_disconnect
;;;     gtk_accel_group_disconnect_key
;;;     gtk_accel_group_activate
;;;     gtk_accel_group_lock
;;;     gtk_accel_group_unlock
;;;     gtk_accel_group_get_is_locked
;;;     gtk_accel_group_from_accel_closure
;;;     gtk_accel_group_get_modifier_mask
;;;     gtk_accel_groups_activate
;;;     gtk_accel_groups_from_object
;;;     gtk_accel_group_find
;;;
;;;     GtkAccelKey
;;;
;;;     gtk_accelerator_valid
;;;     gtk_accelerator_parse
;;;     gtk_accelerator_name
;;;     gtk_accelerator_get_label
;;;     gtk_accelerator_parse_with_keycode
;;;     gtk_accelerator_name_with_keycode
;;;     gtk_accelerator_get_label_with_keycode
;;;     gtk_accelerator_set_default_mod_mask
;;;     gtk_accelerator_get_default_mod_mask
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelGroup" gtk-accel-group
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_accel_group_get_type")
  ((is-locked
    gtk-accel-group-is-locked
    "is-locked" "gboolean" t nil)
   (modifier-mask
    gtk-accel-group-modifier-mask
    "modifier-mask" "GdkModifierType" t nil)))

;;; --- gtk-accel-group --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-accel-group 'type)
 "@version{2013-4-22}
  @begin{short}
    A @sym{gtk-accel-group} object represents a group of keyboard accelerators,
    typically attached to a toplevel @class{gtk-window} widget with the function
    @fun{gtk-window-add-accel-group}. Usually you will not need to create a
    @sym{gtk-accel-group} object directly; instead, when using
    @class{gtk-ui-manager}, GTK+ automatically sets up the accelerators for your
    menus in the UI manager's @sym{gtk-accel-group} object.
  @end{short}

  Note that accelerators are different from mnemonics. Accelerators are
  shortcuts for activating a menu item; they appear alongside the menu item
  they are a shortcut for. For example \"Ctrl+Q\" might appear alongside the
  \"Quit\" menu item. Mnemonics are shortcuts for GUI elements such as text
  entries or buttons; they appear as underlined characters, see the function
  @fun{gtk-label-new-with-mnemonic}. Menu items can have both accelerators and
  mnemonics, of course.
  @begin[Signal Details]{dictionary}
    @subheading{The \"accel-activate\" signal}
      @begin{pre}
 lambda (accel-group acceleratable keyval modifier)   : Has Details
      @end{pre}
      The \"accel-activate\" signal is an implementation detail of
      @sym{gtk-accel-group} and not meant to be used by applications.
      @begin[arg]{table}
        @entry[accel-group]{The @sym{gtk-accel-group} object which received the
          signal.}
        @entry[acceleratable]{The @class{g-object} on which the accelerator was
          activated.}
        @entry[keyval]{The accelerator keyval of type @code{:uint}.}
        @entry[modifier]{The modifier combination of type
          @symbol{gdk-modifier-type} of the accelerator.}
        @entry[Returns]{@arg{True} if the accelerator was activated.}
      @end{table}
    @subheading{The \"accel-changed\" signal}
      @begin{pre}
 lambda (accel-group keyval modifier accel-closure)   : Has Details
      @end{pre}
      The \"accel-changed\" signal is emitted when an entry is added to or
      removed from the accel group.
      Widgets like @class{gtk-accel-label} which display an associated
      accelerator should connect to this signal, and rebuild their visual
      representation if the @arg{accel-closure} is theirs.
      @begin[arg]{table}
        @entry[accel-group]{The @sym{gtk-accel-group} object which received the
          signal.}
        @entry[keyval]{The accelerator keyval of type @code{:uint}.}
        @entry[modifier]{The modifier combination of type
          @symbol{gdk-modifier-type} of the accelerator.}
        @entry[accel-closure]{The @symbol{g-closure} callback of the
          accelerator.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-accel-group-is-locked}
  @see-slot{gtk-accel-group-modifier-mask}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-accel-group-is-locked ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-locked"
                                               'gtk-accel-group) 't)
 "@version{2013-4-17}
  The @code{\"is-locked\"} property of type @code{:boolean} (Read)@br{}
  Is the accel group locked.@br{}
  Default value: @code{nil}")

;;; --- gtk-accel-group-modifier-mask ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "modifier-mask"
                                               'gtk-accel-group) 't)
 "@version{2013-4-17}
  The @code{\"modifier-mask\"} property of type @symbol{gdk-modifier-type}
  (Read)@br{}
  The modifier mask.@br{}
  Default value: @code{'(:shift-mask :control-mask :mod1-mask :super-mask
                         :hyper-mask :meta-mask)}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-accel-group-is-locked ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-group-is-locked atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-group-is-locked 'function)
 "@version{2013-4-17}
  Accessor of the slot @code{\"is-locked\"} of the @class{gtk-accel-group}
  class.")

;;; --- gtk-accel-group-modifier-mask ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-group-modifier-mask atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-group-modifier-mask 'function)
 "@version{2013-4-17}
  Accessor of the slot @code{\"modifer-mask\"} of the @class{gtk-accel-group}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-accel-group-new))

(defun gtk-accel-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-4-17}
  @return{A new @class{gtk-accel-group} object.}
  Creates a new @class{gtk-accel-group} object."
  (make-instance 'gtk-accel-group))

(export 'gtk-accel-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_connect ()
;;;
;;; void gtk_accel_group_connect (GtkAccelGroup *accel_group,
;;;                               guint accel_key,
;;;                               GdkModifierType accel_mods,
;;;                               GtkAccelFlags accel_flags,
;;;                               GClosure *closure);
;;;
;;; Installs an accelerator in this group. When accel_group is being activated
;;; in response to a call to gtk_accel_groups_activate(), closure will be
;;; invoked if the accel_key and accel_mods from gtk_accel_groups_activate()
;;; match those of this connection.
;;;
;;; The signature used for the closure is that of GtkAccelGroupActivate.
;;;
;;; Note that, due to implementation details, a single closure can only be
;;; connected to one accelerator group.
;;;
;;; accel_group :
;;;     the accelerator group to install an accelerator in
;;;
;;; accel_key :
;;;     key value of the accelerator
;;;
;;; accel_mods :
;;;     modifier combination of the accelerator
;;;
;;; accel_flags :
;;;     a flag mask to configure this accelerator
;;;
;;; closure :
;;;     closure to be executed upon accelerator activation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_connect_by_path ()
;;;
;;; void gtk_accel_group_connect_by_path (GtkAccelGroup *accel_group,
;;;                                       const gchar *accel_path,
;;;                                       GClosure *closure);
;;;
;;; Installs an accelerator in this group, using an accelerator path to look up
;;; the appropriate key and modifiers (see gtk_accel_map_add_entry()). When
;;; accel_group is being activated in response to a call to
;;; gtk_accel_groups_activate(), closure will be invoked if the accel_key and
;;; accel_mods from gtk_accel_groups_activate() match the key and modifiers for
;;; the path.
;;;
;;; The signature used for the closure is that of GtkAccelGroupActivate.
;;;
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first with
;;; g_intern_static_string().
;;;
;;; accel_group :
;;;     the accelerator group to install an accelerator in
;;;
;;; accel_path :
;;;     path used for determining key and modifiers
;;;
;;; closure :
;;;     closure to be executed upon accelerator activation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkAccelGroupActivate ()
;;;
;;; gboolean (*GtkAccelGroupActivate) (GtkAccelGroup *accel_group,
;;;                                    GObject *acceleratable,
;;;                                    guint keyval,
;;;                                    GdkModifierType modifier);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkAccelGroupFindFunc ()
;;;
;;; gboolean (*GtkAccelGroupFindFunc) (GtkAccelKey *key,
;;;                                    GClosure *closure,
;;;                                    gpointer data);
;;;
;;; data :
;;;     . [closure]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_disconnect ()
;;;
;;; gboolean gtk_accel_group_disconnect (GtkAccelGroup *accel_group,
;;;                                      GClosure *closure);
;;;
;;; Removes an accelerator previously installed through
;;; gtk_accel_group_connect().
;;;
;;; Since 2.20 closure can be NULL.
;;;
;;; accel_group :
;;;     the accelerator group to remove an accelerator from
;;;
;;; closure :
;;;     the closure to remove from this accelerator group, or NULL to remove all
;;;     closures
;;;
;;; Returns :
;;;     TRUE if the closure was found and got disconnected
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_disconnect_key ()
;;;
;;; gboolean gtk_accel_group_disconnect_key (GtkAccelGroup *accel_group,
;;;                                          guint accel_key,
;;;                                          GdkModifierType accel_mods);
;;;
;;; Removes an accelerator previously installed through
;;; gtk_accel_group_connect().
;;;
;;; accel_group :
;;;     the accelerator group to install an accelerator in
;;;
;;; accel_key :
;;;     key value of the accelerator
;;;
;;; accel_mods :
;;;     modifier combination of the accelerator
;;;
;;; Returns :
;;;     TRUE if there was an accelerator which could be removed, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_group_activate" gtk-accel-group-activate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-22}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  @argument[accel-quark]{the quark for the accelerator name}
  @argument[acceleratable]{the @class{g-object}, usually a @class{gtk-window}
    widget, on which to activate the accelerator}
  @argument[accel-key]{accelerator keyval from a key event}
  @argument[accel-mods]{keyboard state mask from a key event}
  @return{@em{True} if an accelerator was activated and handled this keypress.}
  Finds the first accelerator in @arg{accel-group} that matches @arg{accel-key}
  and @arg{accel-mods}, and activates it."
  (accel-group (g-object gtk-accel-group))
  (accel-quark g-quark)
  (acceleratable g-object)
  (accel-key :uint)
  (accel-mods gdk-modifier-type))

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_lock ()
;;;
;;; void gtk_accel_group_lock (GtkAccelGroup *accel_group);
;;;
;;; Locks the given accelerator group.
;;;
;;; Locking an acelerator group prevents the accelerators contained within it to
;;; be changed during runtime. Refer to gtk_accel_map_change_entry() about
;;; runtime accelerator changes.
;;;
;;; If called more than once, accel_group remains locked until
;;; gtk_accel_group_unlock() has been called an equivalent number of times.
;;;
;;; accel_group :
;;;     a GtkAccelGroup
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_unlock ()
;;;
;;; void gtk_accel_group_unlock (GtkAccelGroup *accel_group);
;;;
;;; Undoes the last call to gtk_accel_group_lock() on this accel_group.
;;;
;;; accel_group :
;;;     a GtkAccelGroup
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_get_is_locked ()
;;;
;;; gboolean gtk_accel_group_get_is_locked (GtkAccelGroup *accel_group);
;;;
;;; Locks are added and removed using gtk_accel_group_lock() and
;;; gtk_accel_group_unlock().
;;;
;;; accel_group :
;;;     a GtkAccelGroup
;;;
;;; Returns :
;;;     TRUE if there are 1 or more locks on the accel_group, FALSE otherwise.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_from_accel_closure ()
;;;
;;; GtkAccelGroup * gtk_accel_group_from_accel_closure (GClosure *closure);
;;;
;;; Finds the GtkAccelGroup to which closure is connected; see
;;; gtk_accel_group_connect().
;;;
;;; closure :
;;;     a GClosure
;;;
;;; Returns :
;;;     the GtkAccelGroup to which closure is connected, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_get_modifier_mask ()
;;;
;;; GdkModifierType gtk_accel_group_get_modifier_mask
;;;                                                (GtkAccelGroup *accel_group);
;;;
;;; Gets a GdkModifierType representing the mask for this accel_group. For
;;; example, GDK_CONTROL_MASK, GDK_SHIFT_MASK, etc.
;;;
;;; accel_group :
;;;     a GtkAccelGroup
;;;
;;; Returns :
;;;     the modifier mask for this accel group.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_groups_activate ()
;;;
;;; gboolean gtk_accel_groups_activate (GObject *object,
;;;                                     guint accel_key,
;;;                                     GdkModifierType accel_mods);
;;;
;;; Finds the first accelerator in any GtkAccelGroup attached to object that
;;; matches accel_key and accel_mods, and activates that accelerator.
;;;
;;; object :
;;;     the GObject, usually a GtkWindow, on which to activate the accelerator
;;;
;;; accel_key :
;;;     accelerator keyval from a key event
;;;
;;; accel_mods :
;;;     keyboard state mask from a key event
;;;
;;; Returns :
;;;     TRUE if an accelerator was activated and handled this keypress
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_groups_from_object ()
;;;
;;; GSList * gtk_accel_groups_from_object (GObject *object);
;;;
;;; Gets a list of all accel groups which are attached to object.
;;;
;;; object :
;;;     a GObject, usually a GtkWindow
;;;
;;; Returns :
;;;     a list of all accel groups which are attached to object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_find ()
;;;
;;; GtkAccelKey * gtk_accel_group_find (GtkAccelGroup *accel_group,
;;;                                     GtkAccelGroupFindFunc find_func,
;;;                                     gpointer data);
;;;
;;; Finds the first entry in an accelerator group for which find_func returns
;;; TRUE and returns its GtkAccelKey.
;;;
;;; accel_group :
;;;     a GtkAccelGroup
;;;
;;; find_func :
;;;     a function to filter the entries of accel_group with
;;;
;;; data :
;;;     data to pass to find_func
;;;
;;; Returns :
;;;     the key of the first entry passing find_func. The key is owned by GTK+
;;;     and must not be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelKey
;;;
;;; struct GtkAccelKey {
;;;   guint           accel_key;
;;;   GdkModifierType accel_mods;
;;;   guint           accel_flags : 16;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_valid ()
;;;
;;; gboolean gtk_accelerator_valid (guint keyval, GdkModifierType modifiers);
;;;
;;; Determines whether a given keyval and modifier mask constitute a valid
;;; keyboard accelerator. For example, the GDK_KEY_a keyval plus
;;; GDK_CONTROL_MASK is valid - this is a "Ctrl+a" accelerator. But, you can't,
;;; for instance, use the GDK_KEY_Control_L keyval as an accelerator.
;;;
;;; keyval :
;;;     a GDK keyval
;;;
;;; modifiers :
;;;     modifier mask
;;;
;;; Returns :
;;;     TRUE if the accelerator is valid
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accelerator_parse" %gtk-accelerator-parse) :void
  (accelerator :string)
  (accelerator-key (:pointer :uint))
  (accelerator-mods (:pointer gdk-modifier-type)))

(defun gtk-accelerator-parse (accelerator)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-22}
  @argument[accelerator]{a string representing an accelerator}
  @begin{return}
    @code{accelerator-key} -- an accelerator keyval, or @code{nil} @br{}
    @code{accelerator-mods} --an accelerator modifier mask, or @code{nil}
  @end{return}
  @begin{short}
    Parses a string representing an accelerator. The format looks like
    \"<Control>a\" or \"<Shift><Alt>F1\" or \"<Release>z\" (the last one is for
    key release).
  @end{short}

  The parser is fairly liberal and allows lower or upper case, and also
  abbreviations such as \"<Ctl>\" and \"<Ctrl>\". Key names are parsed using
  the function @fun{gdk-keyval-from-name}. For character keys the name is not
  the symbol, but the lowercase name, e. g. one would use \"<Ctrl>minus\"
  instead of \"<Ctrl>-\".

  If the parse fails, @arg{accelerator-key} and @arg{accelerator-mods} will be
  set to 0.
  @begin[Examples]{dictionary}
    @begin{pre}
 (gtk-accelerator-parse \"<Control>a\")
=> 97
=> (:CONTROL-MASK)

 (gtk-accelerator-parse \"<Shift><Alt>F1\")
=> 65470
=> (:SHIFT-MASK :MOD1-MASK)
    @end{pre}
  @end{dictionary}
  @see-function{gdk-keyval-from-name}"
  (with-foreign-objects ((accelerator-key :uint)
                         (accelerator-mods 'gdk-modifier-type))
    (%gtk-accelerator-parse accelerator accelerator-key accelerator-mods)
    (values (mem-ref accelerator-key :uint)
            (mem-ref accelerator-mods 'gdk-modifier-type))))

(export 'gtk-accelerator-parse)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_name ()
;;;
;;; gchar * gtk_accelerator_name (guint accelerator_key,
;;;                               GdkModifierType accelerator_mods);
;;;
;;; Converts an accelerator keyval and modifier mask into a string parseable by
;;; gtk_accelerator_parse(). For example, if you pass in GDK_KEY_q and
;;; GDK_CONTROL_MASK, this function returns "<Control>q".
;;;
;;; If you need to display accelerators in the user interface, see
;;; gtk_accelerator_get_label().
;;;
;;; accelerator_key :
;;;     accelerator keyval
;;;
;;; accelerator_mods :
;;;     accelerator modifier mask
;;;
;;; Returns :
;;;     a newly-allocated accelerator name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_label ()
;;;
;;; gchar * gtk_accelerator_get_label (guint accelerator_key,
;;;                                    GdkModifierType accelerator_mods);
;;;
;;; Converts an accelerator keyval and modifier mask into a string which can be
;;; used to represent the accelerator to the user.
;;;
;;; accelerator_key :
;;;     accelerator keyval
;;;
;;; accelerator_mods :
;;;     accelerator modifier mask
;;;
;;; Returns :
;;;     a newly-allocated string representing the accelerator.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_parse_with_keycode ()
;;;
;;; void gtk_accelerator_parse_with_keycode (const gchar *accelerator,
;;;                                          guint *accelerator_key,
;;;                                          guint **accelerator_codes,
;;;                                          GdkModifierType *accelerator_mods);
;;;
;;; Parses a string representing an accelerator, similarly to
;;; gtk_accelerator_parse() but handles keycodes as well. This is only useful
;;; for system-level components, applications should use gtk_accelerator_parse()
;;; instead.
;;;
;;; If a keycode is present in the accelerator and no accelerator_codes is
;;; given, the parse will fail.
;;;
;;; If the parse fails, accelerator_key, accelerator_mods and accelerator_codes
;;; will be set to 0 (zero).
;;;
;;; accelerator :
;;;     string representing an accelerator
;;;
;;; accelerator_key :
;;;     return location for accelerator keyval, or NULL
;;;
;;; accelerator_codes :
;;;     return location for accelerator keycodes, or NULL
;;;
;;; accelerator_mods :
;;;     return location for accelerator modifier mask, NULL
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_name_with_keycode ()
;;;
;;; gchar * gtk_accelerator_name_with_keycode (GdkDisplay *display,
;;;                                            guint accelerator_key,
;;;                                            guint keycode,
;;;                                            GdkModifierType accelerator_mods)
;;;
;;; Converts an accelerator keyval and modifier mask into a string parseable by
;;; gtk_accelerator_parse_full(), similarly to gtk_accelerator_name() but
;;; handling keycodes. This is only useful for system-level components,
;;; applications should use gtk_accelerator_parse() instead.
;;;
;;; display :
;;;     a GdkDisplay or NULL to use the default display
;;;
;;; accelerator_key :
;;;     accelerator keyval
;;;
;;; accelerator_mods :
;;;     accelerator modifier mask
;;;
;;; Returns :
;;;     a newly allocated accelerator name.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_label_with_keycode ()
;;;
;;; gchar * gtk_accelerator_get_label_with_keycode
;;;                                          (GdkDisplay *display,
;;;                                           guint accelerator_key,
;;;                                           guint keycode,
;;;                                           GdkModifierType accelerator_mods);
;;;
;;; Converts an accelerator keyval and modifier mask into a (possibly
;;; translated) string that can be displayed to a user, similarly to
;;; gtk_accelerator_get_label(), but handling keycodes.
;;;
;;; This is only useful for system-level components, applications should use
;;; gtk_accelerator_parse() instead.
;;;
;;; display :
;;;     a GdkDisplay or NULL to use the default display
;;;
;;; accelerator_key :
;;;     accelerator keyval
;;;
;;; accelerator_mods :
;;;     accelerator modifier mask
;;;
;;; Returns :
;;;     a newly-allocated string representing the accelerator.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_set_default_mod_mask ()
;;;
;;; void gtk_accelerator_set_default_mod_mask
;;;                                          (GdkModifierType default_mod_mask);
;;;
;;; Sets the modifiers that will be considered significant for keyboard
;;; accelerators. The default mod mask is GDK_CONTROL_MASK | GDK_SHIFT_MASK |
;;; GDK_MOD1_MASK | GDK_SUPER_MASK | GDK_HYPER_MASK | GDK_META_MASK, that is,
;;; Control, Shift, Alt, Super, Hyper and Meta. Other modifiers will by default
;;; be ignored by GtkAccelGroup. You must include at least the three modifiers
;;; Control, Shift and Alt in any value you pass to this function.
;;;
;;; The default mod mask should be changed on application startup, before using
;;; any accelerator groups.
;;;
;;; default_mod_mask :
;;;     accelerator modifier mask
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_default_mod_mask ()
;;;
;;; GdkModifierType gtk_accelerator_get_default_mod_mask (void);
;;;
;;; Gets the value set by gtk_accelerator_set_default_mod_mask().
;;;
;;; Returns :
;;;     the default accelerator modifier mask
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.accel-group.lisp ---------------------------------------
