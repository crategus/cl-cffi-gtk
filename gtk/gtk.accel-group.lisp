;;; ----------------------------------------------------------------------------
;;; gtk.accel-group.lisp
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
;;; Accelerator Groups
;;;
;;;     Groups of global keyboard accelerators for an entire GtkWindow
;;;
;;; Types and Values
;;;
;;;     GtkAccelGroup
;;;     GtkAccelFlags
;;;     GtkAccelKey
;;;
;;; Functions
;;;
;;;     gtk_accel_group_new
;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;
;;;     GtkAccelGroupActivate
;;;     GtkAccelGroupFindFunc
;;;
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
;;;     gtk_accelerator_valid
;;;     gtk_accelerator_parse
;;;     gtk_accelerator_name
;;;     gtk_accelerator_get_label
;;;     gtk_accelerator_parse_with_keycode       * not implemented *
;;;     gtk_accelerator_name_with_keycode        * not implemented *
;;;     gtk_accelerator_get_label_with_keycode   * not implemented *
;;;     gtk_accelerator_set_default_mod_mask
;;;     gtk_accelerator_get_default_mod_mask
;;;
;;; Properties
;;;
;;;            gboolean    is-locked         Read
;;;     GdkModifierType    modifier-mask     Read
;;;
;;; Signals
;;;
;;;            gboolean    accel-activate    Has Details
;;;                void    accel-changed     Has Details
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAccelGroup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkAccelFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkAccelFlags" gtk-accel-flags
  (:export t
   :type-initializer "gtk_accel_flags_get_type")
  (:visible 1)
  (:locked 2)
  (:mask 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-accel-flags atdoc:*external-symbols*)
 "@version{2020-8-23}
  @begin{short}
    Accelerator flags used with the function @fun{gtk-accel-group-connect}.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkAccelFlags\" gtk-accel-flags
  (:export t
   :type-initializer \"gtk_accel_flags_get_type\")
  (:visible 1)
  (:locked 2)
  (:mask 7))
  @end{pre}
  @begin[code]{table}
    @entry[:visible]{Accelerator is visible.}
    @entry[:locked]{Accelerator not removable.}
    @entry[:mask]{Mask.}
  @end{table}
  @see-class{gtk-accel-group}
  @see-function{gtk-accel-group-connect}")

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-accel-group 'type)
 "@version{2020-8-23}
  @begin{short}
    A @sym{gtk-accel-group} object represents a group of keyboard accelerators,
    typically attached to a toplevel @class{gtk-window} widget with the function
    @fun{gtk-window-add-accel-group}.
  @end{short}
  Usually you will not need to create a @sym{gtk-accel-group} object directly.
  Instead, when using @class{gtk-ui-manager}, GTK+ automatically sets up the
  accelerators for your menus in the UI manager's @sym{gtk-accel-group} object.

  Note that accelerators are different from mnemonics. Accelerators are
  shortcuts for activating a menu item. They appear alongside the menu item
  they are a shortcut for. For example \"Ctrl+Q\" might appear alongside the
  \"Quit\" menu item. Mnemonics are shortcuts for GUI elements such as text
  entries or buttons. They appear as underlined characters, see the function
  @fun{gtk-label-new-with-mnemonic}. Menu items can have both accelerators and
  mnemonics, of course.
  @begin[Signal Details]{dictionary}
    @subheading{The \"accel-activate\" signal}
      @begin{pre}
 lambda (accel-group acceleratable keyval modifier)    : Has Details
      @end{pre}
      The \"accel-activate\" signal is an implementation detail of
      @sym{gtk-accel-group} and not meant to be used by applications.
      @begin[code]{table}
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
 lambda (accel-group keyval modifier accel-closure)    : Has Details
      @end{pre}
      The \"accel-changed\" signal is emitted when an entry is added to or
      removed from the accel group. Widgets like @class{gtk-accel-label} which
      display an associated accelerator should connect to this signal, and
      rebuild their visual representation if the @arg{accel-closure} is theirs.
      @begin[code]{table}
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
  @see-slot{gtk-accel-group-modifier-mask}
  @see-class{gtk-window}
  @see-class{gtk-ui-manager}
  @see-symbol{gdk-modifier-type}
  @see-function{gtk-accel-map-change-entry}
  @see-function{gtk-window-add-accel-group}
  @see-function{gtk-label-new-with-menmonic}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-accel-group-is-locked ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-locked"
                                               'gtk-accel-group) 't)
 "The @code{is-locked} property of type @code{:boolean} (Read) @br{}
  Is the accel group locked. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-group-is-locked atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-group-is-locked 'function)
 "@version{2020-8-23}
  @syntax[]{(gtk-accel-group-is-locked object) => is-locked}
  @argument[object]{a @class{gtk-accel-group} object}
  @argument[is-locked]{a @code{:boolean} wether the accel group is locked}
  @begin{short}
    Accessor of the @slot[gtk-accel-group]{is-locked} slot of the
    @class{gtk-accel-group} class.
  @end{short}

  The slot access function @sym{gtk-accel-group-is-locked} returns @em{true} if
  there are 1 or more locks on the accel-group, @em{false} otherwise.

  Locks are added and removed using the functions @fun{gtk-accel-group-lock}
  and @fun{gtk-accel-group-unlock}.
  @see-class{gtk-accel-group}
  @see-function{gtk-accel-group-lock}
  @see-function{gtk-accel-group-unlock}")

;;; --- gtk-accel-group-modifier-mask ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "modifier-mask"
                                               'gtk-accel-group) 't)
 "The @code{modifier-mask} property of type @symbol{gdk-modifier-type} (Read)
  @br{}
  The modifier mask. @br{}
  Default value: @code{'(:shift-mask :control-mask :mod1-mask :super-mask
                         :hyper-mask :meta-mask)}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-group-modifier-mask atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-accel-group-modifier-mask 'function)
 "@version{2020-8-23}
  @syntax[]{(gtk-accelerator-group object) => modifier-mask}
  @argument[object]{a @class{gtk-accel-group} object}
  @argument[modifier-mask]{a modifier mask of type @symbol{gdk-modifier-mask}}
  @begin{short}
    Accessor of the @slot[gtk-accel-group]{modifier-mask} slot of the
    @class{gtk-accel-group} class.
  @end{short}

  The slot access function @sym{gtk-accel-group-modifer-mask} gets the modifier
  mask for this accel group. For example, @code{:control-mask},
  @code{:shift-mask}, etc.
  @see-class{gtk-accel-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-accel-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @return{A new @class{gtk-accel-group} object.}
  @begin{short}
    Creates a new accel group.
  @end{short}
  @see-class{gtk-accel-group}"
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
  @argument[accel-key]{accelerator keyval of type @code{:uint} from a key event}
  @argument[accel-mods]{keyboard state mask of type @symbol{gdk-modifier-type}
    from a key event}
  @return{@em{True} if an accelerator was activated and handled this keypress.}
  @begin{short}
    Finds the first accelerator in @arg{accel-group} that matches
    @arg{accel-key} and @arg{accel-mods}, and activates it.
  @end{short}
  @see-class{gtk-accel-group}
  @see-class{gtk-window}
  @see-symbol{gdk-modifier-type}"
  (accel-group (g-object gtk-accel-group))
  (accel-quark g-quark)
  (acceleratable g-object)
  (accel-key :uint)
  (accel-mods gdk-modifier-type))

(export 'gtk-accel-group-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_lock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_group_lock" gtk-accel-group-lock) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-24}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  @begin{short}
    Locks the given accelerator group.
  @end{short}

  Locking an acelerator group prevents the accelerators contained within it to
  be changed during runtime. Refer to the @fun{gtk-accel-map-change-entry}
  function about runtime accelerator changes.

  If called more than once, @arg{accel-group} remains locked until the
  @fun{gtk-accel-group-unlock} function has been called an equivalent number of
  times.
  @see-class{gtk-accel-group}
  @see-function{gtk-accel-group-unlock}
  @see-function{gtk-accel-map-change-entry}"
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-accel-group-lock)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_group_unlock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_group_unlock" gtk-accel-group-unlock) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-24}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  @begin{short}
    Undoes the last call to the @fun{gtk-accel-group-lock} function on this
    @arg{accel-group}.
  @end{short}
  @see-class{gtk-accel-group}
  @see-function{gtk-accel-group-lock}"
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-accel-group-unlock)

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

(defcstruct gtk-accel-key
  (accel-key :uint)
  (accel-mods gdk-modifier-type)
  (accel-flags :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-accel-key atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-accel-key atdoc:*external-symbols*)
 "@version{2013-11-29}
  @short{}
  @begin{pre}
(defcstruct gtk-accel-key
  (accel-key :uint)
  (accel-mods gdk-modifier-type)
  (accel-flags :uint))
  @end{pre}
  @see-class{gtk-accel-group}")

(export 'gtk-accel-key)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accelerator_valid" gtk-accelerator-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @argument[keyval]{a @code{:uint} with a GDK keyval}
  @argument[modifiers]{modifier mask of type @symbol{gdk-modifier-type}}
  @return{@em{True} if the accelerator is valid.}
  @begin{short}
    Determines whether a given @arg{keyval} and modifier mask constitute a
    valid keyboard accelerator.
  @end{short}
  For example, the @code{GDK_KEY_a} keyval plus @code{GDK_CONTROL_MASK} is
  valid - this is a \"Ctrl+a\" accelerator. But, you cannot, for instance, use
  the @code{GDK_KEY_Control_L} keyval as an accelerator.
  @see-symbol{gdk-modifier-type}"
  (keyval :uint)
  (modifiers gdk-modifier-type))

(export 'gtk-accelerator-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accelerator_parse" %gtk-accelerator-parse) :void
  (accelerator :string)
  (accelerator-key (:pointer :uint))
  (accelerator-mods (:pointer gdk-modifier-type)))

(defun gtk-accelerator-parse (accelerator)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @argument[accelerator]{a string representing an accelerator}
  @begin{return}
    @code{accelerator-key}  -- a @code{:uint} with an accelerator keyval @br{}
    @code{accelerator-mods} -- an accelerator modifier mask of type
                               @symbol{gdk-modifier-type}, or @code{nil}
  @end{return}
  @begin{short}
    Parses a string representing an accelerator.
  @end{short}
  The format looks like \"<Control>a\" or \"<Shift><Alt>F1\" or \"<Release>z\".
  The last one is for key release.

  The parser is fairly liberal and allows lower or upper case, and also
  abbreviations such as \"<Ctl>\" and \"<Ctrl>\". Key names are parsed using
  the function @fun{gdk-keyval-from-name}. For character keys the name is not
  the symbol, but the lowercase name, e.g. one would use \"<Ctrl>minus\"
  instead of \"<Ctrl>-\".

  If the parse fails, @arg{accelerator-key} will be set to 0.
  @begin[Examples]{dictionary}
    @begin{pre}
 (gtk-accelerator-parse \"<Control>a\")
=> 97
=> (:CONTROL-MASK)

 (gtk-accelerator-parse \"<Shift><Alt>F1\")
=> 65470
=> (:SHIFT-MASK :MOD1-MASK)

 (gtk-accelerator-parse \"<Release>z\")
=> 122
=> (:RELEASE-MASK)

 (gtk-accelerator-parse \"<Control>minus\")
=> 45
=> (:CONTROL-MASK)

 (gtk-accelerator-parse \"not valid\")
=> 0
=> NIL
    @end{pre}
  @end{dictionary}
  @see-symbol{gdk-modifier-type}
  @see-function{gdk-keyval-from-name}"
  (with-foreign-objects ((accelerator-key :uint)
                         (accelerator-mods 'gdk-modifier-type))
    (%gtk-accelerator-parse accelerator accelerator-key accelerator-mods)
    (values (mem-ref accelerator-key :uint)
            (mem-ref accelerator-mods 'gdk-modifier-type))))

(export 'gtk-accelerator-parse)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accelerator_name" gtk-accelerator-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @argument[accelerator-key]{accelerator keyval of type @code{:uint}}
  @argument[accelerator-mods]{accelerator modifier mask of type
    @symbol{gdk-modifier-type}}
  @return{A @code{:string} with the accelerator name.}
  @begin{short}
    Converts an accelerator keyval and modifier mask into a string parseable by
    the function @fun{gtk-accelerator-parse}.
  @end{short}

  If you need to display accelerators in the user interface, see the function
  @fun{gtk-accelerator-label}.
  @begin[Example]{dictionary}
    @begin{pre}
 (gtk-accelerator-name 65470 '(:shift-mask :mod1-mask))
=> \"<Shift><Alt>F1\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk-accel-group}
  @see-function{gtk-accelerator-parse}
  @see-function{gtk-accelerator-label}"
  (accelerator-key :uint)
  (accelerator-mods gdk-modifier-type))

(export 'gtk-accelerator-name)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_label () -> gtk-accelerator-label
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accelerator_get_label" gtk-accelerator-label) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @argument[accelerator-key]{accelerator keyval of type @code{:uint}}
  @argument[accelerator-mods]{accelerator modifier mask of type
    @symbol{gdk-modifier-type}}
  @return{A @code{:string} representing the accelerator.}
  @begin{short}
    Converts an accelerator keyval and modifier mask into a string which can be
    used to represent the accelerator to the user.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (gtk-accelerator-label 65470 '(:shift-mask :mod1-mask))
=> \"Umschalt+Alt+F1\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk-accel-group}
  @see-function{gtk-accelerator-parse}
  @see-function{gtk-accelerator-name}"
  (accelerator-key :uint)
  (accelerator-mods gdk-modifier-type))

(export 'gtk-accelerator-label)

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
;;; gchar *
;;; gtk_accelerator_get_label_with_keycode
;;;                               (GdkDisplay *display,
;;;                                guint accelerator_key,
;;;                                guint keycode,
;;;                                GdkModifierType accelerator_mods);
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
;;; keycode :
;;;     accelerator keycode
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
;;; gtk_accelerator_get_default_mod_mask () -> gtk-accelerator-default-mod-mask
;;; ----------------------------------------------------------------------------

(defun (setf gtk-accelerator-default-mod-mask) (default-mod-mask)
  (foreign-funcall "gtk_accelerator_set_default_mod_mask"
                   gdk-modifier-type default-mod-mask
                   :void)
  default-mod-mask)

(defcfun ("gtk_accelerator_get_default_mod_mask"
           gtk-accelerator-default-mod-mask) gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @syntax[]{(gtk-accelerator-default-mod-mask) => default-mod-mask}
  @syntax[]{(setf (gtk-accelerator-default-mod-mask) default-mod-mask)}
  @argument[default-mod-mask]{accelerator modifier mask of type
    @symbol{gdk-modifier-type}}
  @begin{short}
     Accessor of the default accelerator modifier mask.
  @end{short}

  The function @sym{gtk-accelerator-default-mod-mask} gets the default
  accelerator modifier mask. The function
  @sym{(setf gtk-accelerator-default-mod-mask)} sets the modifiers that will
  be considered significant for keyboard accelerators.

  The default mod mask is @code{'(:control-mask :shift-mask :mod1-mask
  :super-mask :hyper-mask :meta-mask)}, that is, Control, Shift, Alt, Super,
  Hyper and Meta. Other modifiers will by default be ignored by the
  accel group. You must include at least the three modifiers Control, Shift and
  Alt in any value you pass to this function.

  The default mod mask should be changed on application startup, before using
  any accelerator groups.
  @see-class{gtk-accel-group}
  @see-symbol{gdk-modifier-type}")

(export 'gtk-accelerator-default-mod-mask)

;;; --- End of file gtk.accel-group.lisp ---------------------------------------
