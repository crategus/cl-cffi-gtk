;;; ----------------------------------------------------------------------------
;;; gtk.window.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkWindow
;;;
;;;     Toplevel which can contain other widgets
;;;
;;; Types and Values
;;;
;;;     GtkWindow
;;;     GtkWindowType                                  --> gtk.enumerations.lisp
;;;     GtkWindowPosition                              --> gtk.enumerations.lisp
;;;
;;; Functions
;;;
;;;     gtk_window_new
;;;     gtk_window_set_title                               Accessor
;;;     gtk_window_set_wmclass
;;;     gtk_window_set_resizable                           Accessor
;;;     gtk_window_get_resizable                           Accessor
;;;     gtk_window_add_accel_group
;;;     gtk_window_remove_accel_group
;;;     gtk_window_activate_focus
;;;     gtk_window_activate_default
;;;     gtk_window_set_modal                               Accessor
;;;     gtk_window_set_default_size
;;;     gtk_window_set_default_geometry
;;;     gtk_window_set_geometry_hints
;;;     gtk_window_set_gravity                             Accessor
;;;     gtk_window_get_gravity                             Accessor
;;;     gtk_window_set_position
;;;     gtk_window_set_transient_for                       Accessor
;;;     gtk_window_set_attached_to                         Accessor
;;;     gtk_window_set_destroy_with_parent                 Accessor
;;;     gtk_window_set_hide_titlebar_when_maximized        Accessor
;;;     gtk_window_set_screen                              Accessor
;;;     gtk_window_get_screen                              Accessor
;;;     gtk_window_is_active
;;;     gtk_window_is_maximized                            Accessor
;;;     gtk_window_has_toplevel_focus
;;;     gtk_window_list_toplevels
;;;     gtk_window_add_mnemonic
;;;     gtk_window_remove_mnemonic
;;;     gtk_window_mnemonic_activate
;;;     gtk_window_activate_key
;;;     gtk_window_propagate_key_event
;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_get_default_widget
;;;     gtk_window_set_default
;;;     gtk_window_present
;;;     gtk_window_present_with_time
;;;     gtk_window_close
;;;     gtk_window_iconify
;;;     gtk_window_deiconify
;;;     gtk_window_stick
;;;     gtk_window_unstick
;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor ()
;;;     gtk_window_unfullscreen
;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below
;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag
;;;     gtk_window_set_decorated                           Accessor
;;;     gtk_window_set_deletable                           Accessor
;;;     gtk_window_set_mnemonic_modifier
;;;     gtk_window_set_type_hint                           Accessor
;;;     gtk_window_set_skip_taskbar_hint                   Accessor
;;;     gtk_window_set_skip_pager_hint                     Accessor
;;;     gtk_window_set_urgency_hint                        Accessor
;;;     gtk_window_set_accept_focus                        Accessor
;;;     gtk_window_set_focus_on_map                        Accessor
;;;     gtk_window_set_startup_id                          Accessor
;;;     gtk_window_set_role                                Accessor
;;;     gtk_window_get_decorated                           Accessor
;;;     gtk_window_get_deletable                           Accessor
;;;     gtk_window_get_default_icon_list
;;;     gtk_window_get_default_icon_name
;;;     gtk_window_get_default_size
;;;     gtk_window_get_destroy_with_parent                 Accessor
;;;     gtk_window_get_hide_titlebar_when_maximized        Accessor
;;;     gtk_window_get_icon                                Accessor
;;;     gtk_window_get_icon_list
;;;     gtk_window_get_icon_name                           Accessor
;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_get_modal                               Accessor
;;;     gtk_window_get_position
;;;     gtk_window_get_role                                Accessor
;;;     gtk_window_get_size
;;;     gtk_window_get_title                               Accessor
;;;     gtk_window_get_transient_for                       Accessor
;;;     gtk_window_get_attached_to                         Accessor
;;;     gtk_window_get_type_hint                           Accessor
;;;     gtk_window_get_skip_taskbar_hint                   Accessor
;;;     gtk_window_get_skip_pager_hint                     Accessor
;;;     gtk_window_get_urgency_hint                        Accessor
;;;     gtk_window_get_accept_focus                        Accessor
;;;     gtk_window_get_focus_on_map                        Accessor
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_get_window_type
;;;     gtk_window_move
;;;     gtk_window_parse_geometry
;;;     gtk_window_reshow_with_initial_size
;;;     gtk_window_resize
;;;     gtk_window_resize_to_geometry
;;;     gtk_window_set_default_icon_list
;;;     gtk_window_set_default_icon
;;;     gtk_window_set_default_icon_from_file
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_set_icon                                Accessor
;;;     gtk_window_set_icon_list
;;;     gtk_window_set_icon_from_file
;;;     gtk_window_set_icon_name                           Accessor
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity
;;;     gtk_window_get_mnemonics_visible                   Accessor
;;;     gtk_window_set_mnemonics_visible                   Accessor
;;;     gtk_window_get_focus_visible                       Accessor
;;;     gtk_window_set_focus_visible                       Accessor
;;;     gtk_window_set_has_resize_grip                     Accessor
;;;     gtk_window_get_has_resize_grip                     Accessor
;;;     gtk_window_resize_grip_is_visible
;;;     gtk_window_get_resize_grip_area
;;;     gtk_window_get_application                         Accessor
;;;     gtk_window_set_application                         Accessor
;;;     gtk_window_set_has_user_ref_count
;;;     gtk_window_set_titlebar
;;;     gtk_window_get_titlebar
;;;     gtk_window_set_interactive_debugging
;;
;;; Properties
;;;
;;;           gboolean   accept-focus                   Read / Write
;;;     GtkApplication*  application                    Read / Write
;;;          GtkWidget*  attached-to                    Read / Write / Construct
;;;           gboolean   decorated                      Read / Write
;;;               gint   default-height                 Read / Write
;;;               gint   default-width                  Read / Write
;;;           gboolean   deletable                      Read / Write
;;;           gboolean   destroy-with-parent            Read / Write
;;;           gboolean   focus-on-map                   Read / Write
;;;           gboolean   focus-visible                  Read / Write
;;;         GdkGravity   gravity                        Read / Write
;;;           gboolean   has-resize-grip                Read / Write
;;;           gboolean   has-toplevel-focus             Read
;;;           gboolean   hide-titlebar-when-maximized   Read / Write
;;;          GdkPixbuf*  icon                           Read / Write
;;;              gchar*  icon-name                      Read / Write
;;;           gboolean   is-active                      Read
;;;           gboolean   is-maximized                   Read
;;;           gboolean   mnemonics-visible              Read / Write
;;;           gboolean   modal                          Read / Write
;;;           gboolean   resizable                      Read / Write
;;;           gboolean   resize-grip-visible            Read
;;;              gchar*  role                           Read / Write
;;;          GdkScreen*  screen                         Read / Write
;;;           gboolean   skip-pager-hint                Read / Write
;;;           gboolean   skip-taskbar-hint              Read / Write
;;;              gchar*  startup-id                     Write
;;;              gchar*  title                          Read / Write
;;;          GtkWindow*  transient-for                  Read / Write / Construct
;;;      GtkWindowType   type                           Read / Write / Construct
;;;  GdkWindowTypeHint   type-hint                      Read / Write
;;;           gboolean   urgency-hint                   Read / Write
;;;  GtkWindowPosition   window-position                Read / Write
;;;
;;; Style Properties
;;;
;;;              gchar*  decoration-button-layout    Read
;;;               gint   decoration-resize-handle    Read / Write
;;;
;;; Signals
;;;
;;;               void   activate-default            Action
;;;               void   activate-focus              Action
;;;           gboolean   enable-debugging            Action
;;;               void   keys-changed                Run First
;;;               void   set-focus                   Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ├── GtkDialog
;;;                         ├── GtkApplicationWindow
;;;                         ├── GtkAssistant
;;;                         ├── GtkOffscreenWindow
;;;                         ├── GtkPlug
;;;                         ╰── GtkShortcutsWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkWindow implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindow
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkWindow" 'gtk-window))

(define-g-object-class "GtkWindow" gtk-window
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_window_get_type")
  ((accept-focus
    gtk-window-accept-focus
    "accept-focus" "gboolean" t t)
   (application
    gtk-window-application
    "application" "GtkApplication" t t)
   (attached-to
    gtk-window-attached-to
    "attached-to" "GtkWidget" t t)
   (decorated
    gtk-window-decorated
    "decorated" "gboolean" t t)
   (default-height
    gtk-window-default-height
    "default-height" "gint" t t)
   (default-width
    gtk-window-default-width
    "default-width" "gint" t t)
   (deletable
    gtk-window-deletable
    "deletable" "gboolean" t t)
   (destroy-with-parent
    gtk-window-destroy-with-parent
    "destroy-with-parent" "gboolean" t t)
   (focus-on-map
    gtk-window-focus-on-map
    "focus-on-map" "gboolean" t t)
   (focus-visible
    gtk-window-visible
    "focus-visible" "gboolean" t t)
   (gravity
    gtk-window-gravity
    "gravity" "GdkGravity" t t)
   (has-resize-grip
    gtk-window-has-resize-grip
    "has-resize-grip" "gboolean" t t)
   (has-toplevel-focus
     gtk-window-has-toplevel-focus
     "has-toplevel-focus" "gboolean" t nil)
   (hide-titlebar-when-maximized
    gtk-window-hide-titlebar-when-maximized
    "hide-titlebar-when-maximized" "gboolean" t t)
   (icon
    gtk-window-icon
    "icon" "GdkPixbuf" t t)
   (icon-name
    gtk-window-icon-name
    "icon-name" "gchararray" t t)
   (is-active
    gtk-window-is-active
    "is-active" "gboolean" t nil)
   #+gtk-3-12
   (is-maximized
    gtk-window-is-maximized
    "is-maximized" "gboolean" t nil)
   (mnemonics-visible
    gtk-window-mnemonics-visible
    "mnemonics-visible" "gboolean" t t)
   (modal
    gtk-window-modal
    "modal" "gboolean" t t)
   (opacity
    gtk-window-opacity
    "opacity" "gdouble" t t)
   (resizable
    gtk-window-resizable
    "resizable" "gboolean" t t)
   (resize-grip-visible
    gtk-window-resize-grip-visible
    "resize-grip-visible" "gboolean" t nil)
   (role
    gtk-window-role
    "role" "gchararray" t t)
   (screen
    gtk-window-screen
    "screen" "GdkScreen" t t)
   (skip-pager-hint
    gtk-window-skip-pager-hint
    "skip-pager-hint" "gboolean" t t)
   (skip-taskbar-hint
    gtk-window-skip-taskbar-hint
    "skip-taskbar-hint" "gboolean" t t)
   (startup-id
    gtk-window-startup-id
    "startup-id" "gchararray" nil t)
   (title
    gtk-window-title
    "title" "gchararray"  t t)
   (transient-for
    gtk-window-transient-for
    "transient-for" "GtkWindow" t t)
   (type
    gtk-window-type
    "type" "GtkWindowType" t nil)
   (type-hint
    gtk-window-type-hint
    "type-hint" "GdkWindowTypeHint" t t)
   ;; "ubuntu-no-proxy" is not documented. Special for Ubuntu.
   #+ubuntu
   (ubuntu-no-proxy
    gtk-window-ubuntu-no-proxy
    "ubuntu-no-proxy" "gboolean" t nil)
   (urgency-hint
    gtk-window-urgency-hint
    "urgency-hint" "gboolean" t t)
   (window-position
    gtk-window-window-position
    "window-position" "GtkWindowPosition" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-window 'type)
 "@version{2013-7-30}
  @begin{short}
    A @sym{gtk-window} is a toplevel window which can contain other widgets.
  @end{short}

  @image[window]{}

  Windows normally have decorations that are under the control of the
  windowing system and allow the user to manipulate the window, e. g. to
  resize it, move it, or close it.
  @begin[GtkWindow as GtkBuildable]{dictionary}
    The @sym{gtk-window} implementation of the @class{gtk-buildable} interface
    supports a custom @code{<accel-groups>} element, which supports any number
    of @code{<group>} elements representing the @class{gtk-accel-group} objects
    you want to add to your window. This is synonymous with the
    @fun{gtk-window-add-accel-group} function.

    @b{Example:} A UI definition fragment with accel groups
  @begin{pre}
 <object class=\"GtkWindow\">
   <accel-groups>
     <group name=\"accelgroup1\"/>
   </accel-groups>
 </object>
 <!-- -->
   ...
 <!-- -->
 <object class=\"GtkAccelGroup\" id=\"accelgroup1\"/>
    @end{pre}
    The @sym{gtk-window} implementation of the @class{gtk-buildable} interface
    supports setting a child as the titlebar by specifying @code{\"titlebar\"}
    as the @code{\"type\"} attribute of a @code{<child>} element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 window.background
 ├── decoration
 ├── <titlebar child>.titlebar [.default-decoration]
 ╰── <child>
    @end{pre}
    @sym{gtk-window} has a main CSS node with name @code{window} and style class
    @code{.background}, and a subnode with name @code{decoration}.

    Style classes that are typically used with the main CSS node are
    @code{.csd}, when client-side decorations are in use, @code{.solid-csd}, for
    client-side decorations without invisible borders, @code{.ssd}, used by
    mutter when rendering server-side decorations. @sym{gtk-window} also
    represents window states with the following style classes on the main node:
    @code{.tiled}, @code{.maximized}, @code{.fullscreen}. Specialized types of
    window often add their own discriminating style classes, such as
    @code{.popup} or @code{.tooltip}.

    @sym{gtk-window} adds the @code{.titlebar} and @code{.default-decoration}
    style classes to the widget that is added as a titlebar child.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[decoration-button-layout]{entry}
        The @code{decoration-button-layout} style property of type
        @code{:string} (Read) @br{}
        Decorated button layout. @br{}
        Default value: @code{\"menu:close\"}
      @end{entry}
      @begin[decoration-resize-handle]{entry}
        The @code{decoration-resize-handle} style property of type @code{:int}
        (Read / Write) @br{}
        Decoration resize handle size. @br{}
        Allowed values: >= 0 @br{}
        Default value: 20 @br{}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-default\" signal}
      @begin{pre}
 lambda (window)    : Action
      @end{pre}
      The \"activate-default\" signal is a keybinding signal which gets emitted
      when the user activates the default widget of @arg{window}.
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
    @subheading{The \"activate-focus\" signal}
      @begin{pre}
 lambda (window)    : Action
      @end{pre}
      The \"activate-focus\" signal is a keybinding signal which gets emitted
      when the user activates the currently focused widget of @arg{window}.
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
    @subheading{The \"enable-debugging\" signal}
      @begin{pre}
 lambda (window toggle)    : Action
      @end{pre}
      The \"enable-debugging\" signal is a keybinding signal which gets emitted
      when the user enables or disables interactive debugging. When @arg{toggle}
      is @em{true}, interactive debugging is toggled on or off, when it is
      @code{nil}, the debugger will be pointed at the widget under the pointer.
      The default bindings for this signal are Ctrl-Shift-I and Ctrl-Shift-D.
      @begin[code]{table}
        @entry[window]{The @class{gdk-window} object on which the signal is
          emitted.}
        @entry[toggle]{A @code{:boolean} which toggles the debugger.}
        @entry[Return]{A @code{:boolean} which is @em{true} if the key binding
          was handled.}
      @end{table}
    @subheading{The \"keys-changed\" signal}
      @begin{pre}
 lambda (window)    : Run First
      @end{pre}
      The \"keys-changed\" signal gets emitted when the set of accelerators or
      mnemonics that are associated with @arg{window} changes.
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
    @subheading{The \"set-focus\" signal}
      @begin{pre}
 lambda (window widget)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-window-accept-focus}
  @see-slot{gtk-window-application}
  @see-slot{gtk-window-attached-to}
  @see-slot{gtk-window-decorated}
  @see-slot{gtk-window-default-height}
  @see-slot{gtk-window-default-width}
  @see-slot{gtk-window-deletable}
  @see-slot{gtk-window-destroy-with-parent}
  @see-slot{gtk-window-focus-on-map}
  @see-slot{gtk-window-focus-visible}
  @see-slot{gtk-window-gravity}
  @see-slot{gtk-window-has-resize-grip}
  @see-slot{gtk-window-has-toplevel-focus}
  @see-slot{gtk-window-hide-titlebar-when-maximized}
  @see-slot{gtk-window-icon}
  @see-slot{gtk-window-icon-name}
  @see-slot{gtk-window-is-active}
  @see-slot{gtk-window-mnemonics-visible}
  @see-slot{gtk-window-modal}
  @see-slot{gtk-window-opacity}
  @see-slot{gtk-window-resizable}
  @see-slot{gtk-window-resize-grip-visible}
  @see-slot{gtk-window-role}
  @see-slot{gtk-window-screen}
  @see-slot{gtk-window-skip-pager-hint}
  @see-slot{gtk-window-skip-taskbar-hint}
  @see-slot{gtk-window-startup-id}
  @see-slot{gtk-window-title}
  @see-slot{gtk-window-transient-for}
  @see-slot{gtk-window-type}
  @see-slot{gtk-window-type-hint}
  @see-slot{gtk-window-urgency-hint}
  @see-slot{gtk-window-window-position}
  @see-class{gtk-buildable}
  @see-class{gtk-accel-group}
  @see-class{gtk-application}
  @see-class{gtk-menu}
  @see-class{gtk-combo-box}
  @see-class{gtk-entry}
  @see-class{gtk-tree-view}
  @see-class{gtk-icon-theme}
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-gravity}
  @see-symbol{gtk-window-type}
  @see-symbol{gdk-window-type-hint}
  @see-symbol{gtk-window-position}
  @see-function{gtk-window-add-accel-group}
  @see-function{g-application-hold}
  @see-function{gtk-window-move}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-window-accept-focus ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accept-focus" 'gtk-window) 't)
 "The @code{accept-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the window should receive the input focus. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-accept-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-accept-focus 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-accept-focus object) => accept-focus}
  @syntax[]{(setf (gtk-window-accept-focus object) accept-focus)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to let this window receive input focus}
  @begin{short}
    Accessor of the @slot[gtk-window]{accept-focus} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-accept-focus} slot access function gets the value of
  the @slot[gtk-window]{accept-focus} property.

  Windows may set a hint asking the desktop environment not to receive the
  input focus. The @sym{(setf gtk-window-accept-focus)} slot access function
  sets this hint.
  @see-class{gtk-window}")

;;; --- gtk-window-application -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "application" 'gtk-window) 't)
 "The @code{application} property of type @class{gtk-application}
  (Read / Write) @br{}
  The @class{gtk-application} associated with the window. The application will
  be kept alive for at least as long as it has any windows associated with it.
  See the @fun{g-application-hold} function for a way to keep it alive without
  windows. Normally, the connection between the application and the window will
  remain until the window is destroyed, but you can explicitly remove it by
  setting the @code{application} property to @code{nil}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-application atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-application 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-application object) => application}
  @syntax[]{(setf (gtk-window-application object) application)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[application]{a @class{gtk-application}, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-window]{application} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-application} slot access function gets the
  @class{gtk-application} associated with the window, if any.

  The @sym{(setf gtk-window-application)} slot access function sets or unsets
  the @class{gtk-application} associated with the window.

  The application will be kept alive for at least as long as the window is open.
  @see-class{gtk-window}
  @see-class{gtk-application}")

;;; --- gtk-window-attached-to -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attached-to" 'gtk-window) 't)
 "The @code{attached-to} property of type @class{gtk-widget}
  (Read / Write / Construct) @br{}
  The widget to which this window is attached. See the
  @fun{gtk-window-attached-to} function. Examples of places where specifying
  this relation is useful are for instance a @class{gtk-menu} created by a
  @class{gtk-combo-box}, a completion popup window created by @class{gtk-entry}
  or a typeahead search entry created by @class{gtk-tree-view}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-attached-to atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-attached-to 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-attached-to object) => attach-widget}
  @syntax[]{(setf (gtk-window-attached-to object) attach-widget)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[attach-widget]{a @class{gtk-widget} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-window]{attached-to} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-attached-to} slot access function returns the widget where
  the window is attached, or @code{nil} if the window is not attached to any
  widget.

  The @sym{(setf gtk-window-attached-to)} slot access function marks the window
  as attached to @arg{attach-widget}. This creates a logical binding between
  the window and the widget it belongs to, which is used by GTK+ to propagate
  information such as styling or accessibility to the window as if it was a
  children of @arg{attach-widget}.

  Examples of places where specifying this relation is useful are for instance
  a @class{gtk-menu} created by a @class{gtk-combo-box}, a completion popup
  window created by @class{gtk-entry} or a typeahead search entry created by
  @class{gtk-tree-view}.

  Note that this function should not be confused with the
  @fun{gtk-window-transient-for} function, which specifies a window manager
  relation between two toplevels instead.

  Passing @code{nil} for @arg{attach-widget} detaches the window.
  @see-class{gtk-window}
  @see-class{gtk-widget}
  @see-function{gtk-window-transient-for}")

;;; --- gtk-window-decorated ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "decorated" 'gtk-window) 't)
 "The @code{decorated} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should be decorated by the window manager. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-decorated atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-decorated 'function)
 "@version{2014-2-9}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to decorate the window}
  @syntax[]{(gtk-window-decorated object) => setting}
  @syntax[]{(setf (gtk-window-decorated object) setting)}
  @begin{short}
    Accessor of the @slot[gtk-window]{decorated} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-decorated} slot access function returns whether the window
  has been set to have decorations such as a title bar.

  By default, windows are decorated with a title bar, resize controls, etc.
  Some window managers allow GTK+ to disable these decorations, creating a
  borderless window. If you set the @slot[gtk-window]{decorated} property to
  @code{nil} using this function, GTK+ will do its best to convince the window
  manager not to decorate the window. Depending on the system, this function may
  not have any effect when called on a window that is already visible, so you
  should call it before calling the @fun{gtk-widget-show} function.

  On Windows, this function always works, since there is no window manager
  policy involved.
  @see-class{gtk-window}
  @see-function{gtk-widget-show}")

;;; --- gtk-window-default-height ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-height" 'gtk-window) 't)
 "The @code{default-height} property of type @code{:int} (Read / Write) @br{}
  The default height of the window, used when initially showing the
  window. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-default-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-default-height 'function)
 "@version{2014-2-6}
  @begin{short}
    Accessor of the @slot[gtk-window]{default-height} slot of the
    @class{gtk-window} class.
  @end{short}
  See the @fun{gtk-window-default-size} function.
  @see-class{gtk-window}
  @see-function{gtk-window-default-size}")

;;; --- gtk-window-default-width -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-width" 'gtk-window) 't)
 "The @code{default-width} property of type @code{:int} (Read / Write) @br{}
  The default width of the window, used when initially showing the window. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-default-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-default-width 'function)
 "@version{2014-2-6}
  @begin{short}
    Accessor of the @slot[gtk-window]{default-width} slot of the
    @class{gtk-window} class.
  @end{short}
  See the @fun{gtk-window-default-size} function.
  @see-class{gtk-window}
  @see-function{gtk-window-default-size}")

;;; --- gtk-window-deletable ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "deletable" 'gtk-window) 't)
 "The @code{deletable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window frame should have a close button. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-deletable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-deletable 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-deletable object) => setting}
  @syntax[]{(setf (gtk-window-deletable object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to decorate the window as deletable}
  @begin{short}
    Accessor of the @slot[gtk-window]{deletable} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-deletable} slot access function returns whether the window
  has been set to have a close button.

  By default, windows have a close button in the window frame. Some window
  managers allow GTK+ to disable this button. If you set the
  @slot[gtk-window]{deletable} property to @code{nil} using this function, GTK+
  will do its best to convince the window manager not to show a close button.

  Depending on the system, this function may not have any effect when called on
  a window that is already visible, so you should call it before calling
  the @fun{gtk-widget-show} function.

  On Windows, this function always works, since there is no window manager
  policy involved.
  @see-class{gtk-window}
  @see-function{gtk-widget-show}")

;;; --- gtk-window-destroy-with-parent -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "destroy-with-parent"
                                               'gtk-window) 't)
 "The @code{destroy-with-parent} property of type @code{:boolean}
  (Read / Write) @br{}
  If this window should be destroyed when the parent is destroyed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-destroy-with-parent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-destroy-with-parent 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-destroy-with-parent object) => setting}
  @syntax[]{(setf (gtk-window-destroy-with-parent object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{whether to destroy window with its transient parent}
  @begin{short}
    Accessor of the @slot[gtk-window]{destroy-with-parent} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-destroy-with-parent} slot access function returns whether
  the window will be destroyed with its transient parent.

  The @sym{(setf gtk-window-destroy-with-parent)} slot access function sets
  whether to destroy the window with its transient parent. If @arg{setting} is
  @arg{true}, then destroying the transient parent of the window will also
  destroy the window itself.

  This is useful for dialogs that should not persist beyond the lifetime of the
  main window they are associated with, for example.
  @see-class{gtk-window}")

;;; --- gtk-window-focus-on-map ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-map" 'gtk-window) 't)
 "The @code{focus-on-map} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the window should receive the input focus when mapped. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-focus-on-map atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-focus-on-map 'function)
 "@version{2013-7-30}
  @syntax[]{(gtk-window-focus-on-map object) => setting}
  @syntax[]{(setf (gtk-window-focus-on-map object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to let this window receive input focus on map}
  @begin{short}
    Accessor of the @slot[gtk-window]{focus-on-map} slot of the
    @class{gtk-window} class.
  @end{short}

  Windows may set a hint asking the desktop environment not to receive the
  input focus when the window is mapped.
  @see-class{gtk-window}")

;;; --- gtk-window-focus-visible -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-visible" 'gtk-window) 't)
 "The @code{focus-visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether \"focus rectangles\" are currently visible in this window. This
  property is maintained by GTK+ based on the
  @slot[gtk-settings]{gtk-visible-focus} setting and user input and should not
  be set by applications. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-focus-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-focus-visible 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-focus-visible object) => setting}
  @syntax[]{(setf (gtk-window-focus-visible object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{a @code{:boolean} value}
  @begin{short}
    Accessor of the @slot[gtk-window]{focus-visible} slot of the
    @class{gtk-window} class.
  @end{short}

  See the @slot[gtk-window]{focus-visible} property for more details.
  @see-class{gtk-window}")

;;; --- gtk-window-gravity -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gravity" 'gtk-window) 't)
 "The @code{gravity} property of type @symbol{gdk-gravity} (Read / Write)
  @br{}
  The window gravity of the window. See the @fun{gtk-window-move} function and
  the @symbol{gdk-gravity} enumeration for more details about window gravity.
  @br{}
  Default value: @code{:north-west}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-gravity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-gravity 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-gravity object) => gravity}
  @syntax[]{(setf (gtk-window-gravity object) gravity)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[gravity]{a @symbol{gdk-gravity} window gravity}
  @begin{short}
    Accessor of the @slot[gtk-window]{gravity} slot of the @class{gtk-window}
    class.
  @end{short}

  Window gravity defines the meaning of coordinates passed to the
  @fun{gtk-window-move} function. See the @fun{gtk-window-move} function and
  the @symbol{gdk-gravity} enumeration for more details.

  The default window gravity is @code{:north-west} which will typically
  \"do what you mean\".
  @see-class{gtk-window}
  @see-symbol{gdk-gravity}
  @see-function{gtk-window-move}")

;;; --- gtk-window-has-resize-grip ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-resize-grip"
                                               'gtk-window) 't)
 "The @code{has-resize-grip} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the window has a corner resize grip. Note that the resize grip is
  only shown if the window is actually resizable and not maximized. Use the
  @code{resize-grip-visible} property to find out if the resize grip is
  currently shown. @br{}
  @em{Warning:} The @code{has-resize-grip} property has been deprecated since
  version 3.14 and should not be used in newly-written code. Resize grips have
  been removed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-has-resize-grip atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-has-resize-grip 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-has-resize-grip object) => value}
  @syntax[]{(setf (gtk-window-has-resize-grip object) value)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[value]{@em{true} to allow a resize grip}
  @begin{short}
    Accessor of the @slot[gtk-window]{has-resize-grip} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-has-resize-grip} slot access function determines whether
  the window may have a resize grip.

  The @sym{(setf gtk-window-has-resize-grip)} slot access function sets whether
  @arg{window} has a corner resize grip.

  Note that the resize grip is only shown if the window is actually resizable
  and not maximized. Use the @fun{gtk-window-resize-grip-is-visible} function
  to find out if the resize grip is currently shown.
  @begin[Warning]{dictionary}
    The @sym{gtk-window-has-resize-grip} slot access function has been
    deprecated since version 3.14 and should not be used in newly-written code.
    Resize grips have been removed.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-window-resize-grip-is-visible}")

;;; --- gtk-window-has-toplevel-focus ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-toplevel-focus"
                                               'gtk-window) 't)
 "The @code{has-toplevel-focus} property of type @code{:boolean} (Read) @br{}
  Whether the input focus is within the window. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-has-toplevel-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-has-toplevel-focus 'function)
 "@version{2013-7-30}
  @argument[object]{a @class{gtk-window} widget}
  @return{@em{True} if the input focus is within this window.}
  @begin{short}
    Returns whether the input focus is within this window.
  @end{short}
  For real toplevel windows, this is identical to the
  @fun{gtk-window-is-active} function, but for embedded windows, like
  @class{gtk-plug}, the results will differ.
  @see-class{gtk-window}
  @see-class{gtk-plug}
  @see-function{gtk-window-is-active}")

;;; --- gtk-window-hide-titlebar-when-maximized --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hide-titlebar-when-maximized"
                                               'gtk-window) 't)
 "The @code{hide-titlebar-when-maximized} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the titlebar should be hidden during maximization. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-hide-titlebar-when-maximized
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-hide-titlebar-when-maximized 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-hide-titlebar-when-maximized object) => setting}
  @syntax[]{(setf (gtk-window-hide-titlebar-when-maximized object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{whether to hide the titlebar when window is maximized}
  @begin{short}
    Accessor of the @slot[gtk-window]{hide-titlebar-when-maximized} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-hide-titlebar-when-maximized} slot access functions
  returns whether the window has requested to have its titlebar hidden when
  maximized.

  The @sym{(setf gtk-window-hide-titlebar-when-maximized)} slot access function
  sets whether to hide the titlebar when the window is maximized. If
  @arg{setting} is @em{true}, then the window will request that it's titlebar
  should be hidden when maximized.

  This is useful for windows that do not convey any information other than the
  application name in the titlebar, to put the available screen space to better
  use. If the underlying window system does not support the request, the setting
  will not have any effect.
  @see-class{gtk-window}")

;;; --- gtk-window-icon --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon" 'gtk-window) 't)
 "The @code{icon} property of type @class{gdk-pixbuf} (Read / Write) @br{}
  Icon for the window.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-icon 'function)
 "@version{2013-7-30}
  @syntax[]{(gtk-window-icon object) => icon}
  @syntax[]{(setf (gtk-window-icon object) icon)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[icon]{icon image, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-window]{icon} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-icon} slot access function gets the icon or if you have
  called the @fun{gtk-window-set-icon-list} function, gets the first icon in
  the icon list.

  The @sym{(setf gtk-window-icon)} slot access function sets up the icon
  representing a @class{gtk-window} widget. This icon is used when the window
  is minimized, also known as iconified. Some window managers or desktop
  environments may also place it in the window frame, or display it in other
  contexts.

  The icon should be provided in whatever size it was naturally drawn;
  that is, do not scale the image before passing it to GTK+. Scaling is
  postponed until the last minute, when the desired final size is known, to
  allow best quality.

  If you have your icon hand drawn in multiple sizes, use the
  @fun{gtk-window-set-icon-list} function. Then the best size will be used.

  This function is equivalent to calling the @fun{gtk-window-set-icon-list}
  function with a 1-element list.

  See also the @fun{gtk-window-set-default-icon-list} function to set the icon
  for all windows in your application in one go.
  @see-class{gtk-window}
  @see-function{gtk-window-set-icon-list}
  @see-function{gtk-window-set-default-icon-list}")

;;; --- gtk-window-icon-name ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-window) 't)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The @code{icon-name} property specifies the name of the themed icon to
  use as the window icon. See the @class{gtk-icon-theme} class for more details.
  @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-icon-name 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-icon-name object) => icon-name}
  @syntax[]{(setf (gtk-window-icon-name object) icon-name)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[name]{the name of the themed icon}
  @begin{short}
    Accessor of the @slot[gtk-window]{icon-name} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-icon-name} slot access function returns the name of the
  themed icon for the window.

  The @sym{(setf gtk-window-icon-name)} slot access function sets the icon for
  the window from a named themed icon. See the @class{gtk-icon-theme} class for
  more details.

  Note that this has nothing to do with the @code{WM_ICON_NAME} property which
  is mentioned in the Inter-Client Communication Conventions Manual (ICCCM).
  @see-class{gtk-window}
  @see-class{gtk-icon-theme}")

;;; --- gtk-window-is-active ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-active" 'gtk-window) 't)
 "The @code{is-active} property of type @code{:boolean} (Read) @br{}
  Whether the toplevel is the current active window. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-is-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-is-active 'function)
 "@version{2013-7-30}
  @argument[object]{a @class{gtk-window} widget}
  @return{@em{True} if the window is part of the current active window.}
  @begin{short}
    Returns whether the window is part of the current active toplevel.
  @end{short}
  That is, the toplevel window receiving keystrokes. The return value is
  @em{true} if the window is active toplevel itself, but also if it is, say, a
  @class{gtk-plug} embedded in the active toplevel. You might use this function
  if you wanted to draw a widget differently in an active window from a widget
  in an inactive window. See the @fun{gtk-window-has-toplevel-focus} function.
  @see-class{gtk-window}
  @see-class{gtk-plug}
  @see-function{gtk-window-has-toplevel-focus}")

;;; --- gtk-window-is-maximized ------------------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "is-maximized" 'gtk-window) 't)
 "The @code{is-maximized} property of type @code{:boolean} (Read) @br{}
  Whether the window is maximized. @br{}
  Default value: @code{nil}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-window-is-maximized atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-is-maximized 'function)
 "@version{2019-3-9}
  @argument[object]{a @class{gtk-window} widget}
  @return{@em{True} if the window has a maximized state.}
  @begin{short}
    Retrieves the current maximized state of @arg{window}.
  @end{short}
  Note that since maximization is ultimately handled by the window manager and
  happens asynchronously to an application request, you shouldn't assume the
  return value of this function changing immediately (or at all), as an effect
  of calling the @fun{gtk-window-maximize} or
  @fun{gtk-window-unmaximize} functions.

  Since 3.12
  @see-class{gtk-window}
  @see-function{gtk-window-maximize}
  @see-function{gtk-window-unmaximize}")

;;; --- gtk-window-mnemonics-visible -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mnemonics-visible"
                                               'gtk-window) 't)
 "The @code{mnemonics-visible} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether mnemonics are currently visible in this window. This property is
  maintained by GTK+ based on the @slot[gtk-settings]{gtk-auto-mnemonics}
  setting and user input, and should not be set by applications. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-mnemonics-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-mnemonics-visible 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-mnemonics-visible object) => setting}
  @syntax[]{(setf (gtk-window-mnemonics-visible object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{the new value}
  @begin{short}
    Accessor of the @slot[gtk-window]{mnemonics-visible} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-mnemonics-visible} slot access function gets the value of
  the @slot[gtk-window]{mnemonics-visible} property.

  The @sym{(setf gtk-window-mnemonics-visible)} slot access function sets the
  @slot[gtk-window]{mnemonics-visible} property.
  @see-class{gtk-window}")

;;; --- gtk-window-modal -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "modal" 'gtk-window) 't)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, the window is modal. Other windows are not usable while this
  one is up. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-modal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-modal 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-modal object) => setting}
  @syntax[]{(setf (gtk-window-modal object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[modal]{whether the window is modal}
  @begin{short}
    Accessor of the @slot[gtk-window]{modal} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-modal} slot access function returns @em{true} if the
  window is set to be modal and establishes a grab when shown.

  The @sym{(setf gtk-window-modal)} slot access function sets a window modal or
  non-modal.

  Modal windows prevent interaction with other windows in the same application.
  To keep modal dialogs on top of main application windows, use the
  @fun{gtk-window-transient-for} function to make the dialog transient for the
  parent; most window managers will then disallow lowering the dialog below the
  parent.
  @see-class{gtk-window}
  @see-function{gtk-window-transient-for}")

;;; --- gtk-window-opacity -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "opacity" 'gtk-window) 't)
 "The @code{opacity} property of type @code{:double} (Read / Write) @br{}
  The requested opacity of the window. See the @fun{gtk-window-opacity} function
  for more details about window opacity. @br{}
  Allowed values: [0,1] @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-opacity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-opacity 'function)
 "@version{2014-2-9}
  @argument[object]{a @class{gtk-window} widget}
  @argument[opacity]{desired opacity, between 0 and 1}
  @syntax[]{(gtk-window-opacity object) => opacity}
  @syntax[]{(setf (gtk-window-opacity object) opacity)}
  @begin{short}
    Accessor of the @slot[gtk-window]{opacity} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-opacity} slot access function fetches the requested
  opacity for this window.

  The @sym{(setf gtk-window-opacity)} slot access function request the windowing
  system to make window partially transparent, with @arg{opacity} 0 being fully
  transparent and 1 fully opaque.

  Values of the opacity parameter are clamped to the [0,1] range. On X11 this
  has any effect only on X screens with a compositing manager running. See the
  @fun{gtk-widget-is-composited} function. On Windows it should work always.

  Note that setting a window's opacity after the window has been shown causes
  it to flicker once on Windows.
  @begin[Warning]{dictionary}
    The @sym{gtk-window-opacity} slot access function has been deprecated since
    version 3.8 and should not be used in newly-written code. Use the
    @fun{gtk-widget-opacity} function instead.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-widget-is-composited}")

;;; --- gtk-window-resizable ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resizable" 'gtk-window) 't)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, users can resize the window. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-resizable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-resizable 'function)
 "@version{2014-2-9}
  @argument[object]{a @class{gtk-window} widget}
  @argument[resizable]{@em{true} if the user can resize this window}
  @syntax[]{(gtk-window-resizable object) => resizable}
  @syntax[]{(setf (gtk-window-resizable object) resizable)}
  @begin{short}
    Accessor of the @slot[gtk-window]{resizable} slot of the @class{gtk-window}
    class.
  @end{short}

  Sets whether the user can resize a window. Windows are user resizable by
  default.
  @see-class{gtk-window}")

;;; --- gtk-window-resize-grip-visible -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resize-grip-visible"
                                               'gtk-window) 't)
 "The @code{resize-grip-visible} property of type @code{:boolean} (Read) @br{}
  Whether a corner resize grip is currently shown. @br{}
  @em{Warning:} The @code{has-resize-grip-visible} property has been deprecated
  since version 3.14 and should not be used in newly-written code. Resize grips
  have been removed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-resize-grip-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-resize-grip-visible 'function)
 "@version{2014-2-9}
  Accessor of the @slot[gtk-window]{resize-grip-visible} slot of the
  @class{gtk-window} class.
  @see-class{gtk-window}
  @see-function{gtk-window-resize-grip-is-visible}")

;;; --- gtk-window-role --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "role" 'gtk-window) 't)
 "The @code{role} property of type @code{:string} (Read / Write) @br{}
  Unique identifier for the window to be used when restoring a session. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-role atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-role 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-role object) => role}
  @syntax[]{(setf (gtk-window-role object) role)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[role]{unique identifier for the window to be used when restoring a
    session}
  @begin{short}
    Accessor of the @slot[gtk-window]{role} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-role} slot access function returns the role of the window
  if set, or @code{nil}.

  The @sym{(setf gtk-window-role)} slot access function sets an unique
  identifier for the window to be used when restoring a session. This function
  is only useful on X11, not with other GTK+ targets.

  In combination with the window title, the window role allows a window
  manager to identify \"the same\" window when an application is restarted. So
  for example you might set the \"toolbox\" role on your app's toolbox window,
  so that when the user restarts their session, the window manager can put the
  toolbox back in the same place.

  If a window already has a unique title, you do not need to set the role,
  since the window manager can use the title to identify the window when
  restoring the session.
  @see-class{gtk-window}")

;;; --- gtk-window-screen ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-window) 't)
 "The @code{screen} property of type @class{gdk-screen} (Read / Write) @br{}
  The screen where this window will be displayed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-screen 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-screen object) => screen}
  @syntax[]{(setf (gtk-window-screen object) screen)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[screen]{a @class{gdk-screen}}
  @begin{short}
    Accessor of the @slot[gtk-window]{screen} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-screen} slot access function returns the
  @class{gdk-screen} associated with window.

  The @sym{(setf gtk-window-screen)} slot access function sets the
  @class{gdk-screen} where the window is displayed; if the window is already
  mapped, it will be unmapped, and then remapped on the new screen.
  @see-class{gtk-window}
  @see-class{gdk-screen}")

;;; --- gtk-window-skip-pager-hint ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "skip-pager-hint"
                                               'gtk-window) 't)
 "The @code{skip-pager-hint} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the window should not be in the pager. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-skip-pager-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-skip-pager-hint 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-skip-pager-hint object) => setting}
  @syntax[]{(setf (gtk-window-skip-pager-hint object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to keep this window from appearing in the pager}
  @begin{short}
    Accessor of the @slot[gtk-window]{skip-pager-hint} slot of the
    @class{gtk-window} class.
  @end{short}

  Windows may set a hint asking the desktop environment not to display the
  window in the pager. This function sets this hint.

  A \"pager\" is any desktop navigation tool such as a workspace switcher that
  displays a thumbnail representation of the windows on the screen.
  @see-class{gtk-window}")

;;; --- gtk-window-skip-taskbar-hint -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "skip-taskbar-hint"
                                               'gtk-window) 't)
 "The @code{skip-taskbar-hint} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the window should not be in the task bar. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-skip-taskbar-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-skip-taskbar-hint 'function)
 "@version{2014-2-9}
  @syntax[]{(gtk-window-skip-taskbar-hint object) => setting}
  @syntax[]{(setf (gtk-window-skip-taskbar-hint object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to keep this window from appearing in the task
    bar}
  @begin{short}
    Accessor of the @slot[gtk-window]{skip-taskbar-hint} slot of the
    @class{gtk-window} class.
  @end{short}

  Windows may set a hint asking the desktop environment not to display the
  window in the task bar. This function sets this hint.
  @see-class{gtk-window}")

;;; --- gtk-window-startup-id --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "startup-id" 'gtk-window) 't)
 "The @code{startup-id} property of type @code{:string} (Write) @br{}
  The @code{startup-id} is a write-only property for setting window's
  startup notification identifier. See the @fun{gtk-window-startup-id} function
  for more details. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-startup-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-startup-id 'function)
 "@version{2013-7-30}
  @argument[object]{a @class{gtk-window} widget}
  @begin{short}
    Accessor of the @slot[gtk-window]{startup-id} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{(setf gtk-window-start-up-id} slot access function sets a string with
  startup notification identifier.

  Startup notification identifiers are used by the desktop environment to track
  application startup, to provide user feedback and other features. This
  function changes the corresponding property on the underlying
  @class{gdk-window}. Normally, startup identifier is managed automatically and
  you should only use this function in special cases like transferring focus
  from other processes. You should use this function before calling the
  @fun{gtk-window-present} function or any equivalent function generating a
  window map event.

  This function is only useful on X11, not with other GTK+ targets.
  @see-class{gtk-window}
  @see-function{gtk-window-present}")

;;; --- gtk-window-title -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-window) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the window. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-title 'function)
 "@version{2014-2-6}
  @syntax[]{(gtk-window-title object) => title}
  @syntax[]{(setf (gtk-window-title object) title)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[title]{title of the window}
  @begin{short}
    Accessor of the @slot[gtk-window]{title} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-title} slot access function retrieves the title of the
  window.

  The @sym{(setf gtk-window-title)} slot access function sets the title of the
  window.

  The title of a window will be displayed in its title bar; on the X11 Window
  System, the title bar is rendered by the window manager, so exactly how the
  title appears to users may vary according to a user's exact configuration.
  The title should help a user distinguish this window from other windows they
  may have open. A good title might include the application name and current
  document filename, for example.
  @see-class{gtk-window}")

;;; --- gtk-window-transient-for -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transient-for" 'gtk-window) 't)
 "The @code{transient-for} property of type @sym{gtk-window}
  (Read / Write / Construct) @br{}
  The transient parent of the window. See the @fun{gtk-window-transient-for}
  slot access function for more details about transient windows.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-transient-for atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-transient-for 'function)
 "@version{2014-2-10}
  @syntax[]{(gtk-window-transient-for object) => parent}
  @syntax[]{(setf (gtk-window-transient-for object) parent)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[parent]{parent window, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-window]{transient-for} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{gtk-window-transient-for} slot access function returns the transient
  parent for this window, or @code{nil} if no transient parent has been set.

  The @sym{(setf gtk-window-transient-for)} slot access function sets the parent
  window.

  Dialog windows should be set transient for the main application window they
  were spawned from. This allows window managers to e. g. keep the dialog on
  top of the main window, or center the dialog over the main window. The
  @fun{gtk-dialog-new-with-buttons} function and other convenience functions
  in GTK+ will sometimes call the @sym{gtk-window-transient-for} function on
  your behalf.

  Passing @code{nil} for parent unsets the current transient window.

  On Windows, this function puts the child window on top of the parent, much
  as the window manager would have done on X11.
  @see-class{gtk-window}
  @see-function{gtk-dialog-new-with-buttons}")

;;; --- gtk-window-type --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "type" 'gtk-window) 't)
 "The @code{type} property of type @symbol{gtk-window-type}
  (Read / Write / Construct) @br{}
  The type of the window. @br{}
  Default value: @code{:toplevel}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-type 'function)
 "@version{2014-2-10}
  Accessor of the @slot[gtk-window]{type} slot of the @class{gtk-window} class.
  @see-class{gtk-window}")

;;; --- gtk-window-type-hint ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "type-hint" 'gtk-window) 't)
 "The @code{type-hint} property of type @symbol{gdk-window-type-hint}
  (Read / Write) @br{}
  Hint to help the desktop environment understand what kind of window this is
  and how to treat it. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-type-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-type-hint 'function)
 "@version{2019-3-8}
  @syntax[]{(gtk-window-type-hint object) => hint}
  @syntax[]{(setf (gtk-window-type-hint object) hint)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[hint]{the window type}
  @begin{short}
    Accessor of the @slot[gtk-window]{type-hint} slot of the @class{gtk-window}
    class.
  @end{short}

  The @sym{gtk-window-type-hint} slot access function returns the type hint for
  the window.

  The @sym{(setf gtk-window-type-hint)} slot access function returns the window
  type of type @symbol{gdk-window-type-hint}. By setting the type hint for the
  window, you allow the window manager to decorate and handle the window in a
  way which is suitable to the function of the window in your application.

  This function should be called before the window becomes visible.

  The @fun{gtk-dialog-new-with-buttons} function and other convenience
  functions in GTK+ will sometimes call the @sym{gtk-window-type-hint}
  slot access function on your behalf.
  @see-class{gtk-window}
  @see-function{gtk-dialog-new-with-button}")

;;; --- gtk-window-urgency-hint ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "urgency-hint" 'gtk-window) 't)
 "The @code{urgency-hint} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the window should be brought to the user's attention. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-urgency-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-urgency-hint 'function)
 "@version{2013-7-30}
  @syntax[]{(gtk-window-urgency-hint object) => setting}
  @syntax[]{(setf (gtk-window-urgency-hint object) setting)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to mark this window as urgent}
  @begin{short}
    Accessor of the @slot[gtk-window]{urgency-hint} slot of the
    @class{gtk-window} class.
  @end{short}

  Windows may set a hint asking the desktop environment to draw the users
  attention to the window. This function sets this hint.
  @see-class{gtk-window}")

;;; --- gtk-window-window-position ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-position"
                                               'gtk-window) 't)
 "The @code{window-position} property of type @symbol{gtk-window-position}
  (Read / Write) @br{}
  The initial position of the window. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-window-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-window-position 'function)
 "@version{2014-2-10}
  @syntax[]{(gtk-window-window-position object) => position}
  @syntax[]{(setf (gtk-window-window-position object) position)}
  @argument[object]{a @class{gtk-window} widget}
  @argument[position]{a position constraint}
  @begin{short}
    Accessor of the @slot[gtk-window]{window-position} slot of the
    @class{gtk-window} class.
  @end{short}

  The @sym{(setf gtk-window-window-position)} slot access function sets a
  position contraint of type @symbol{gtk-window-position} for this window. If
  the old or new constraint is the value @code{:center-always} of the
  @symbol{gtk-window-position} enumeration, this will also cause the window to
  be repositioned to satisfy the new constraint.
  @see-class{gtk-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-new))

(defun gtk-window-new (type)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[type]{type of the window, a value of the @symbol{gtk-window-type}
    enumeration}
  @return{A new @class{gtk-window} widget.}
  @begin{short}
    Creates a new @class{gtk-window} widget, which is a toplevel window that
    can contain other widgets.
  @end{short}
  Nearly always, the type of the window should be @code{:toplevel} from the
  @symbol{gtk-window-type} enumeration. If you are implementing something like
  a popup menu from scratch, which is a bad idea, just use the
  @class{gtk-menu} class, you might use the type @code{:popup}. The type
  @code{:popup} is not for dialogs, though in some other toolkits dialogs are
  called \"popups\". In GTK+, the type @code{:popup} means a pop-up menu or
  pop-up tooltip. On X11, popup windows are not controlled by the window
  manager.

  If you simply want an undecorated window with no window borders, use the
  @fun{gtk-window-decorated} function, do not use the type @code{:popup}.
  @see-class{gtk-window}
  @see-symbol{gtk-window-type}
  @see-function{gtk-window-decorated}"
  (make-instance 'gtk-window :type type))

(export 'gtk-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_wmclass ()
;;;
;;; void gtk_window_set_wmclass (GtkWindow *window,
;;;                              const gchar *wmclass_name,
;;;                              const gchar *wmclass_class);
;;;
;;; Don't use this function. It sets the X Window System "class" and "name"
;;; hints for a window. According to the ICCCM, you should always set these to
;;; the same value for all windows in an application, and GTK+ sets them to that
;;; value by default, so calling this function is sort of pointless. However,
;;; you may want to call gtk_window_set_role() on each window in your
;;; application, for the benefit of the session manager. Setting the role allows
;;; the window manager to restore window positions when loading a saved session.
;;;
;;; gtk_window_set_wmclass has been deprecated since version 3.22 and should not
;;; be used in newly-written code.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; wmclass_name :
;;;     window name hint
;;;
;;; wmclass_class :
;;;     window class hint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_accel_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_add_accel_group" gtk-window-add-accel-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{the window to attach accelerator group to}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  Associate @arg{accel-group} with @arg{window}, such that calling the
  @fun{gtk-accel-group-activate} function on @arg{window} will activate
  accelerators in @arg{accel-group}.
  @see-class{gtk-window}
  @see-function{gtk-accel-group-activate}"
  (window (g-object gtk-window))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-window-add-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_accel_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_remove_accel_group" gtk-window-remove-accel-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  Reverses the effects of the @fun{gtk-window-add-accel-group} function.
  @see-class{gtk-window}
  @see-function{gtk-window-add-accel-group}"
  (window (g-object gtk-window))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-window-remove-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_focus" gtk-window-activate-focus) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if a widget got activated.}
  Activates the current focused widget within the window.
  @see-class{gtk-window}
  @see-function{gtk-window-activate-default}"
  (window (g-object gtk-window)))

(export 'gtk-window-activate-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_default" gtk-window-activate-default) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if a widget got activated.}
  @begin{short}
    Activates the default widget for the window, unless the current focused
    widget has been configured to receive the default action.
  @end{short}
  See the @fun{gtk-widget-set-receives-default} function, in which case the
  focused widget is activated.
  @see-class{gtk-window}
  @see-function{gtk-window-activate-focus}
  @see-function{gtk-widget-set-receives-default}"
  (window (g-object gtk-window)))

(export 'gtk-window-activate-default)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_size ()
;;; ----------------------------------------------------------------------------

(defgeneric (setf gtk-window-default-size) (size window)
  (:method (size (window gtk-window))
    (destructuring-bind (width height) size
      (values (setf (gtk-window-default-width window) width)
              (setf (gtk-window-default-height window) height)))))

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_geometry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_geometry" gtk-window-set-default-geometry)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @argument[width]{width in resize increments, or -1 to unset the default width}
  @argument[height]{height in resize increments, or -1 to unset the default
    height}
  @begin{short}
    Like the @fun{gtk-window-default-size} function, but @arg{width} and
    @arg{height} are interpreted in terms of the base size and increment set
    with the @fun{gtk-window-set-geometry-hints} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-window-set-default-geometry} function has been deprecated since
    version 3.20 and should not be used in newly-written code.
    This function does nothing. If you want to set a default size, use
    the @fun{gtk-window-set-default-size} function instead.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-window-default-size}
  @see-function{gtk-window-set-geometry-hints}"
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-set-default-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_geometry_hints ()
;;; ----------------------------------------------------------------------------

;; TODO: The implementation is changed to pass the geometry as a CStruct.
;; The case nil for geometry is not implemented. Can we do it better?

(defcfun ("gtk_window_set_geometry_hints" %gtk-window-set-geometry-hints) :void
  (window (g-object gtk-window))
  (widget (g-object gtk-widget))
  (geometry :pointer)
  (geometry-mask gdk-window-hints))

(defun gtk-window-set-geometry-hints (window widget geometry geometry-mask)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-26}
  @argument[window]{a @class{gtk-window} widget}
  @argument[widget]{a @class{gtk-widget} object the geometry hints will
    be applied to or @code{nil}. Since 3.20 this argument is ignored and GTK
    behaves as if @code{nil} was set.}
  @argument[geometry]{a @class{gdk-geometry} structure containing geometry
    information}
  @argument[geometry-mask]{mask of type @symbol{gdk-window-hints} indicating
    which structure fields should be paid attention to}
  @begin{short}
    This function sets up hints about how a window can be resized by the user.
  @end{short}
  You can set a minimum and maximum size; allowed resize increments, e. g. for
  xterm, you can only resize by the size of a character; aspect ratios; and
  more. See the @class{gdk-geometry} structure.
  @see-class{gtk-window}
  @see-class{gdk-geometry}
  @see-symbol{gdk-window-hints}"
  (with-foreign-object (ptr '(:struct gdk::gdk-geometry-cstruct))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::min-width)
          (gdk-geometry-min-width geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::min-height)
          (gdk-geometry-min-height geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::max-width)
          (gdk-geometry-max-width geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::max-height)
          (gdk-geometry-max-height geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::base-width)
          (gdk-geometry-base-width geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::base-height)
          (gdk-geometry-base-height geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::width-increment)
          (gdk-geometry-width-increment geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::height-increment)
          (gdk-geometry-height-increment geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::min-aspect)
          (gdk-geometry-min-aspect geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::max-aspect)
          (gdk-geometry-max-aspect geometry))
    (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct)
                                  'gdk::win-gravity)
          (gdk-geometry-win-gravity geometry))
    (%gtk-window-set-geometry-hints window widget ptr geometry-mask)))

(export 'gtk-window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_position ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-position))

(defun gtk-window-set-position (window position)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[window]{a @class{gtk-window} widget}
  @argument[position]{a position constraint of type
    @symbol{gtk-window-position}}
  @begin{short}
    Sets a position constraint for this @arg{window}.
  @end{short}
  If the old or new constraint is @code{:center-always} of the
  @symbol{gtk-window-position}, this will also cause the @arg{window} to be
  repositioned to satisfy the new constraint.
  @see-class{gtk-window}
  @see-symbol{gtk-window-position}
  @see-function{gtk-window-get-position}"
  (setf (gtk-window-window-position window) position))

(export 'gtk-window-set-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_list_toplevels ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_list_toplevels" gtk-window-list-toplevels)
    (g-list (g-object gtk-window) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @return{List of toplevel widgets.}
  @begin{short}
    Returns a list of all existing toplevel windows.
  @end{short}
  The widgets in the list are not individually referenced.
  @see-class{gtk-window}")

(export 'gtk-window-list-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_add_mnemonic" gtk-window-add-mnemonic) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @argument[keyval]{the mnemonic}
  @argument[target]{the widget that gets activated by the mnemonic}
  Adds a mnemonic to this window.
  @see-class{gtk-window}
  @see-function{gtk-window-remove-mnemonic}
  @see-function{gtk-window-activate-mnemonic}"
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object gtk-widget)))

(export 'gtk-window-add-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_remove_mnemonic" gtk-window-remove-mnemonic) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @argument[keyval]{the mnemonic}
  @argument[target]{the widget that gets activated by the mnemonic}
  Removes a mnemonic from this window.
  @see-class{gtk-window}
  @see-function{gtk-window-add-mnemonic}"
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object gtk-widget)))

(export 'gtk-window-remove-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_mnemonic_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_mnemonic_activate" gtk-window-mnemonic-activate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2014-2-13}
  @argument[window]{a @class{gtk-window} widget}
  @argument[keyval]{the mnemonic}
  @argument[modifier]{the modifiers}
  @return{@em{True} if the activation is done.}
  Activates the targets associated with the mnemonic.
  @see-class{gtk-window}
  @see-function{gtk-window-add-mnemonic}"
  (window (g-object gtk-window))
  (keyval :uint)
  (modifier gdk-modifier-type))

(export 'gtk-window-mnemonic-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_key" gtk-window-activate-key) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[event]{a @class{gdk-event-key}}
  @return{@em{True} if a mnemonic or accelerator was found and activated.}
  @begin{short}
    Activates mnemonics and accelerators for this @arg{window}.
  @end{short}
  This is normally called by the default @code{key_press_event} handler for
  toplevel windows, however in some cases it may be useful to call this directly
  when overriding the standard key handling for a toplevel window.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-window-activate-key)

;;; ----------------------------------------------------------------------------
;;; gtk_window_propagate_key_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_propagate_key_event" gtk-window-propagate-key-event)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[event]{a @class{gdk-event-key}}
  @return{@em{True} if a widget in the focus chain handled the event.}
  @begin{short}
    Propagate a key press or release event to the focus widget and up the focus
    container chain until a widget handles event.
  @end{short}
  This is normally called by the default @code{key_press_event} and
  @code{key_release_event} handlers for toplevel windows, however in some cases
  it may be useful to call this directly when overriding the standard key
  handling for a toplevel window.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-window-propagate-key-event)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_focus" gtk-window-get-focus) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The currently focused widget, or @code{nil} if there is none.}
  @begin{short}
    Retrieves the current focused widget within the window.
  @end{short}
  Note that this is the widget that would have the focus if the toplevel window
  focused; if the toplevel window is not focused then
  @code{(gtk-widget-has-focus widget)} will not be @em{true} for the widget.
  @see-class{gtk-window}
  @see-function{gtk-widget-has-focus}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_focus" gtk-window-set-focus) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[focus]{widget to be the new focus widget, or @code{nil} to unset any
    focus widget for the toplevel window}
  @begin{short}
    If @arg{focus} is not the current focus widget, and is focusable, sets it as
    the focus widget for the @arg{window}.
  @end{short}
  If @arg{focus} is @code{nil}, unsets the focus widget for this @arg{window}.
  To set the focus to a particular widget in the toplevel, it is usually more
  convenient to use the @fun{gtk-widget-grab-focus} function instead of this
  function.
  @see-class{gtk-window}
  @see-function{gtk-widget-grab-focus}"
  (window (g-object gtk-window))
  (focus (g-object gtk-widget)))

(export 'gtk-window-set-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_widget" gtk-window-get-default-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The default widget, or @code{nil} if there is none.}
  @begin{short}
    Returns the default widget for @arg{window}. See the
    @fun{gtk-window-set-default} function for more details.
  @end{short}
  @see-class{gtk-window}
  @see-function{gtk-window-set-default}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-default-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default" gtk-window-set-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-7}
  @argument[window]{a @class{gtk-window} widget}
  @argument[default-widget]{widget to be the default, or @code{nil} to unset
    the default widget for the toplevel}
  @begin{short}
    The default widget is the widget that is activated when the user presses
    Enter in a dialog for example.
  @end{short}
  This function sets or unsets the default widget for a @class{gtk-window}.
  When setting, rather than unsetting, the default widget it is generally easier
  to call the @fun{gtk-widget-grab-focus} function on the widget. Before making
  a widget the default widget, you must call the @fun{gtk-widget-can-default}
  function on the widget you would like to make the default.
  @see-class{gtk-window}
  @see-function{gtk-widget-can-default}
  @see-function{gtk-widget-grab-focus}"
  (window (g-object gtk-window))
  (default-widget (g-object gtk-widget)))

(export 'gtk-window-set-default)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_present" gtk-window-present) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Presents a window to the user.
  @end{short}
  This may mean raising the window in the stacking order, deiconifying it,
  moving it to the current desktop, and/or giving it the keyboard focus,
  possibly dependent on the user's platform, window manager, and preferences.

  If the window is hidden, this function calls the @fun{gtk-widget-show}
  function as well.

  This function should be used when the user tries to open a window that is
  already open. Say for example the preferences dialog is currently open, and
  the user chooses Preferences from the menu a second time; use the
  @sym{gtk-window-present} function to move the already open dialog where the
  user can see it.

  If you are calling this function in response to a user interaction, it is
  preferable to use the @fun{gtk-window-present-with-time} function.
  @see-class{gtk-window}
  @see-function{gtk-widget-show}
  @see-function{gtk-window-present-with-time}"
  (window (g-object gtk-window)))

(export 'gtk-window-present)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present_with_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_present_with_time" gtk-window-present-with-time) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[timestamp]{the timestamp of the user interaction, typically a button
    or key press event, which triggered this call}
  @begin{short}
    Presents a window to the user in response to a user interaction.
  @end{short}
  If you need to present a window without a timestamp, use the
  @fun{gtk-window-present} function.
  @see-class{gtk-window}
  @see-function{gtk-window-present}"
  (window (g-object gtk-window))
  (timestamp :uint32))

(export 'gtk-window-present-with-time)

;;; ----------------------------------------------------------------------------
;;; gtk_window_close ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_window_close" gtk-window-close) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Requests that the window is closed, similar to what happens when a window
    manager close button is clicked.
  @end{short}

  This function can be used with close buttons in custom titlebars.

  Since 3.10
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

#+gtk-3-10
(export 'gtk-window-close)

;;; ----------------------------------------------------------------------------
;;; gtk_window_iconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_iconify" gtk-window-iconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to iconify, i. e. minimize, the specified window.
  @end{short}
  Note that you should not assume the window is definitely iconified afterward,
  because other entities, e. g. the user or window manager, could deiconify it
  again, or there may not be a window manager in which case iconification is not
  possible, etc. But normally the window will end up iconified. Just do not
  write code that crashes if not.

  It is permitted to call this function before showing a window, in which case
  the window will be iconified before it ever appears onscreen.

  You can track iconification via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-iconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_deiconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_deiconify" gtk-window-deiconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to deiconify, i. e. unminimize, the specified @arg{window}.
  @end{short}
  Note that you should not assume the window is definitely deiconified
  afterward, because other entities, e. g. the user or window manager, could
  iconify it again before your code which assumes deiconification gets to run.

  You can track iconification via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_stick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_stick" gtk-window-stick) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to stick the window, which means that it will appear on all user
    desktops.
  @end{short}
  Note that you should not assume the window is definitely stuck afterward,
  because other entities, e. g. the user or window manager, could unstick it
  again, and some window managers do not support sticking windows. But normally
  the window will end up stuck. Just do not write code that crashes if not.

  It's permitted to call this function before showing a window.

  You can track stickiness via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-stick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unstick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unstick" gtk-window-unstick) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to unstick the window, which means that it will appear on only one of
    the user's desktops.
  @end{short}
  Note that you should not assume the window is definitely unstuck afterward,
  because other entities, e. g. the user or window manager, could stick it
  again. But normally the window will end up stuck. Just do not write code that
  crashes if not.

  You can track stickiness via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-unstick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_maximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_maximize" gtk-window-maximize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to maximize the window, so that it becomes full screen.
  @end{short}
  Note that you should not assume the window is definitely maximized afterward,
  because other entities, e. g. the user or window manager, could unmaximize it
  again, and not all window managers support maximization. But normally the
  window will end up maximized. Just don't write code that crashes if not.

  It is permitted to call this function before showing a window, in which case
  the window will be maximized when it appears onscreen initially.

  You can track maximization via the \"window-state-event\" signal on
  @class{gtk-widget}, or by listening to notifications on the
  @slot[gtk-window]{is-maximized} property.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-maximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unmaximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unmaximize" gtk-window-unmaximize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to unmaximize the window.
  @end{short}
  Note that you should not assume the window is definitely unmaximized
  afterward, because other entities, e. g. the user or window manager, could
  maximize it again, and not all window managers honor requests to unmaximize.
  But normally the window will end up unmaximized. Just don't write code that
  crashes if not.

  You can track maximization via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_fullscreen" gtk-window-fullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to place the window in the fullscreen state.
  @end{short}
  Note that you should not assume the window is definitely full screen
  afterward, because other entities, e. g. the user or window manager, could
  unfullscreen it again, and not all window managers honor requests to
  fullscreen windows. But normally the window will end up fullscreen. Just do
  not write code that crashes if not.

  You can track the fullscreen state via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen_on_monitor ()
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defcfun ("gtk_window_fullscreen_on_monitor" gtk-window-fullscreen-on-monitor)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[screen]{a @class{gdk-screen} object to draw on}
  @argument[monitor]{an integer which monitor to go fullscreen on}
  @begin{short}
    Asks to place the window in the fullscreen state.
  @end{short}
  Note that you shouldn't assume the window is definitely full screen afterward.

  You can track the fullscreen state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Since 3.18
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (screen (g-object gdk-screen))
  (monitor :int))

#+gtk-3-18
(export 'gtk-window-fullscreen-on-monitor)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unfullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unfullscreen" gtk-window-unfullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to toggle off the fullscreen state for the window.
  @end{short}
  Note that you should not assume the window is definitely not full screen
  afterward, because other entities, e. g. the user or window manager, could
  fullscreen it again, and not all window managers honor requests to
  unfullscreen windows. But normally the window will end up restored to its
  normal state. Just do not write code that crashes if not.

  You can track the fullscreen state via the \"window-state-event\" signal on
  @class{gtk-widget}.
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_keep_above ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_keep_above" gtk-window-set-keep-above) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[setting]{whether to keep @arg{window} above other windows}
  @begin{short}
    Asks to keep @arg{window} above, so that it stays on top.
  @end{short}
  Note that you should not assume the @arg{window} is definitely above
  afterward, because other entities (e. g. the user or window manager) could not
  keep it above, and not all window managers support keeping windows above. But
  normally the window will end kept above. Just do not write code that crashes
  if not.

  It is permitted to call this function before showing a window, in which case
  the window will be kept above when it appears onscreen initially.

  You can track the above state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Note that, according to the Extended Window Manager Hints specification, the
  above state is mainly meant for user preferences and should not be used by
  applications e. g. for drawing attention to their dialogs.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (setting :boolean))

(export 'gtk-window-set-keep-above)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_keep_below ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_keep_below" gtk-window-set-keep-below) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} widget}
  @argument[setting]{whether to keep @arg{window} below other windows}
  @begin{short}
    Asks to keep @arg{window} below, so that it stays in bottom.
  @end{short}
  Note that you should not assume the @arg{window} is definitely below
  afterward, because other entities (e. g. the user or window manager) could not
  keep it below, and not all window managers support putting windows below. But
  normally the window will be kept below. Just do not write code that crashes if
  not.

  It is permitted to call this function before showing a window, in which case
  the window will be kept below when it appears onscreen initially.

  You can track the below state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Note that, according to the Extended Window Manager Hints specification,
  the above state is mainly meant for user preferences and should not be used
  by applications e. g. for drawing attention to their dialogs.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (setting :boolean))

(export 'gtk-window-set-keep-below)

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_resize_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_begin_resize_drag" gtk-window-begin-resize-drag) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[edge]{@symbol{gdk-window-edge} position of the resize control}
  @argument[button]{mouse button that initiated the drag}
  @argument[root-x]{X position where the user clicked to initiate the drag,
    in root window coordinates}
  @argument[root-y]{Y position where the user clicked to initiate the drag}
  @argument[timestamp]{timestamp from the click event that initiated the drag}
  @begin{short}
    Starts resizing a window.
  @end{short}
  This function is used if an application has window resizing controls. When
  GDK can support it, the resize will be done using the standard mechanism for
  the window manager or windowing system. Otherwise, GDK will try to emulate
  window resizing, potentially not all that well, depending on the windowing
  system.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_move_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_begin_move_drag" gtk-window-begin-move-drag) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[button]{mouse button that initiated the drag}
  @argument[root-x]{X position where the user clicked to initiate the drag,
    in root window coordinates}
  @argument[root-y]{Y position where the user clicked to initiate the drag}
  @argument[timestamp]{timestamp from the click event that initiated the drag}
  @begin{short}
    Starts moving a window.
  @end{short}
  This function is used if an application has window movement grips. When GDK
  can support it, the window movement will be done using the standard mechanism
  for the window manager or windowing system. Otherwise, GDK will try to emulate
  window movement, potentially not all that well, depending on the windowing
  system.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_mnemonic_modifier ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_mnemonic_modifier" gtk-window-set-mnemonic-modifier)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[modifier]{the modifier mask used to activate mnemonics on this
    @arg{window}}
  @short{Sets the mnemonic modifier for this @arg{window}.}
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (modifier gdk-modifier-type))

(export 'gtk-window-set-mnemonic-modifier)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_list" gtk-window-get-default-icon-list)
    (g-list (g-object gdk-pixbuf))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @return{Copy of default icon list.}
  @begin{short}
    Gets the value set by the @fun{gtk-window-set-default-icon-list} function.
  @end{short}
  The list is a copy and should be freed with @code{g_list_free()}, but the
  pixbufs in the list have not had their reference count incremented.
  @see-class{gtk-window}")

(export 'gtk-window-get-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_name" gtk-window-get-default-icon-name)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @return{The fallback icon name for windows.}
  @begin{short}
    Returns the fallback icon name for windows that has been set with the
    @fun{gtk-window-set-default-icon-name} function.
  @end{short}
  The returned string is owned by GTK+ and should not be modified. It is only
  valid until the next call to the @fun{gtk-window-set-default-icon-name}
  function.
  @see-class{gtk-window}")

(export 'gtk-window-get-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_size ()
;;; ----------------------------------------------------------------------------

(defgeneric gtk-window-default-size (window)
  (:method ((window gtk-window))
    (values (gtk-window-default-width window)
            (gtk-window-default-height window))))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-window-default-size 'function)
 "@version{2014-2-6}
  @argument[window]{a @class{gtk-window} widget}
  @syntax[]{(gtk-window-default-size window) => width, height}
  @syntax[]{(setf (gtk-window-default-size window) (list width height))}
  @begin{short}
    The @sym{gtk-window-default-size} slot access function gets the default size
    of the window. The @sym{(setf gtk-window-default-size)} slot access
    function sets the default size of the window.
  @end{short}
  A value of -1 for the width or height indicates that a default size has not
  been explicitly set for that dimension, so the \"natural\" size of the window
  will be used.

  If the window's \"natural\" size, its size request, is larger than the
  default, the default will be ignored. More generally, if the default size
  does not obey the geometry hints for the window, the
  @fun{gtk-window-set-geometry-hints} function can be used to set these
  explicitly, the default size will be clamped to the nearest permitted size.

  Unlike the @fun{gtk-widget-size-request} function, which sets a size
  request for a widget and thus would keep users from shrinking the window,
  this function only sets the initial size, just as if the user had resized the
  window themselves. Users can still shrink the window again as they normally
  would. Setting a default size of -1 means to use the \"natural\" default
  size, the size request of the window.

  For more control over a window's initial size and how resizing works,
  investigate the @fun{gtk-window-set-geometry-hints} function.

  For some uses, the @fun{gtk-window-resize} function is a more appropriate
  function. The @fun{gtk-window-resize} function changes the current size of the
  window, rather than the size to be used on initial display. The
  @fun{gtk-window-resize} function always affects the window itself, not the
  geometry widget.

  The default size of a window only affects the first time a window is shown;
  if a window is hidden and re-shown, it will remember the size it had prior
  to hiding, rather than using the default size.

  Windows cannot actually be 0 x 0 in size, they must be at least 1 x 1, but
  passing 0 for width and height is OK, resulting in a 1 x 1 default size.
  @begin[Example]{dictionary}
   @begin{pre}
 (let ((window (make-instance 'gtk-window)))
   (setf (gtk-window-default-size window) '(300 200))
   (gtk-window-default-size window))
=> 300, 200
    @end{pre}
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-window-resize}
  @see-function{gtk-window-default-width}
  @see-function{gtk-window-default-height}
  @see-function{gtk-widget-size-request}
  @see-function{gtk-window-set-geometry-hints}")

(export 'gtk-window-default-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_icon_list" gtk-window-get-icon-list)
    (g-list (g-object gdk-pixbuf) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{Copy of @arg{window}'s icon list.}
  @begin{short}
    Retrieves the list of icons set by the @fun{gtk-window-set-icon-list}
    function.
  @end{short}
  The list is copied, but the reference count on each member won't be
  incremented.
  @see-class{gtk-window}
  @see-function{gtk-window-set-icon-list}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_mnemonic_modifier ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_mnemonic_modifier" gtk-window-get-mnemonic-modifier)
    gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The modifier mask used to activate mnemonics on this @arg{window}.}
  @begin{short}
    Returns the mnemonic modifier for the window.
  @end{short}
  See the @fun{gtk-window-set-mnemonic-modifier} function.
  @see-class{gtk-window}
  @see-function{gtk-window-set-mnemonic-modifier}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-mnemonic-modifier)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_position" %gtk-window-get-position) :void
  (window (g-object gtk-window))
  (root-x (:pointer :int))
  (root-y (:pointer :int)))

;; The Lisp implementation returns the position as a value list.

(defun gtk-window-get-position (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[window]{a @class{gtk-window} widget}
  @begin{return}
    @code{root-x} -- x coordinate of gravity-determined reference point
    or @code{nil} @br{}
    @code{root-y} -- y coordinate of gravity-determined reference point,
    or @code{nil}
  @end{return}
  @begin{short}
    This function returns the position you need to pass to the
    @fun{gtk-window-move} function to keep window in its current position.
  @end{short}
  This means that the meaning of the returned value varies with window gravity.
  See the @fun{gtk-window-move} function for more details.

  If you have not changed the window gravity, its gravity will be
  @code{:north-west} of the @symbol{gdk-gravity} enumeration. This means that
  the @sym{gtk-window-get-position} function gets the position of the top-left
  corner of the window manager frame for the window. The @fun{gtk-window-move}
  function sets the position of this same top-left corner.

  The @sym{gtk-window-get-position} function is not 100 % reliable because the
  X Window System does not specify a way to obtain the geometry of the
  decorations placed on a window by the window manager. Thus GTK+ is using a
  \"best guess\" that works with most window managers.

  Moreover, nearly all window managers are historically broken with respect to
  their handling of window gravity. So moving a window to its current position
  as returned by the @sym{gtk-window-get-position} function tends to result in
  moving the window slightly. Window managers are slowly getting better over
  time.

  If a window has gravity @code{:static} the window manager frame is not
  relevant, and thus the @sym{gtk-window-get-position} function will always
  produce accurate results. However you can not use static gravity to do things
  like place a window in a corner of the screen, because static gravity ignores
  the window manager decorations.

  If you are saving and restoring your application's window positions, you
  should know that it is impossible for applications to do this without getting
  it somewhat wrong because applications do not have sufficient knowledge of
  window manager state. The Correct Mechanism is to support the session
  management protocol, see the \"GnomeClient\" object in the GNOME libraries for
  example, and allow the window manager to save your window sizes and positions.
  @see-class{gtk-window}
  @see-symbol{gdk-gravity}
  @see-function{gtk-window-move}"
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-window-get-position window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-window-get-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_size" %gtk-window-get-size) :void
  (window (g-object gtk-window))
  (width (:pointer :int))
  (height (:pointer :int)))

;; The Lisp implemenation returns the size as a value list.

(defun gtk-window-get-size (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{return}
    @code{width} -- an integer width, or @code{nil}@br{}
    @code{height} -- an integer height, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the current size of window.
  @end{short}
  If the window is not onscreen, it returns the size GTK+ will suggest to the
  window manager for the initial window size, but this is not reliably the same
  as the size the window manager will actually select. The size obtained by the
  @sym{gtk-window-get-size} function is the last size received in a
  @class{gdk-event-configure} event, that is, GTK+ uses its locally stored size,
  rather than querying the X server for the size. As a result, if you call the
  @fun{gtk-window-resize} function then immediately call the
  @sym{gtk-window-get-size} function, the size won't have taken effect yet.
  After the window manager processes the resize request, GTK+ receives
  notification that the size has changed via a configure event, and the size of
  the window gets updated.
  @begin[Note]{dictionary}
    @begin{enumerate}
      @begin{item}
        Nearly any use of this function creates a race condition, because the
        size of the window may change between the time that you get the size and
        the time that you perform some action assuming that size is the current
        size. To avoid race conditions, connect to the \"configure-event\"
        signal on the window and adjust your size dependent state to match the
        size delivered in the @class{gdk-event-configure} event.
      @end{item}
      @begin{item}
        The returned size does not include the size of the window manager
        decorations, aka the window frame or border. Those are not drawn by GTK+
        and GTK+ has no reliable method of determining their size.
      @end{item}
      @begin{item}
        If you are getting a window size in order to position the window
        onscreen, there may be a better way. The preferred way is to simply set
        the window's semantic type with the @fun{gtk-window-type-hint}
        function, which allows the window manager to e. g. center dialogs. Also,
        if you set the transient parent of dialogs with the
        @fun{gtk-window-transient-for} function window managers will often
        center the dialog over its parent window. It is much preferred to let
        the window manager handle these things rather than doing it yourself,
        because all apps will behave consistently and according to user prefs if
        the window manager handles it. Also, the window manager can take the
        size of the window decorations/border into account, while your
        application cannot.
      @end{item}
    @end{enumerate}
    In any case, if you insist on application-specified window positioning,
    there is still a better way than doing it yourself - the
    @fun{gtk-window-set-position} function will frequently handle the details
    for you.
  @end{dictionary}
  @see-class{gtk-window}
  @see-class{gdk-configure-event}
  @see-function{gtk-window-resize}
  @see-function{gtk-window-type-hint}
  @see-function{gtk-window-set-position}"
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-window-get-size window width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(export 'gtk-window-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_group" gtk-window-get-group)
    (g-object gtk-window-group)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget, or @code{nil}}
  @return{The @class{gtk-window-group} for a @arg{window} or the default group.}
  @begin{short}
    Returns the group for @arg{window} or the default group, if @arg{window} is
    @code{nil} or if @arg{window} does not have an explicit window group.
  @end{short}
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_has_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_has_group" gtk-window-has-group) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if @arg{window} has an explicit window group.}
  @short{Returns whether window has an explicit window group.}
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

(export 'gtk-window-has-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_window_type ()
;;; ----------------------------------------------------------------------------

(defun gtk-window-get-window-type (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The type of the window.}
  @short{Gets the type of the window.}
   See the @symbol{gtk-window-type} enumeration.
  @see-class{gtk-window}"
  (gtk-window-type window))

(export 'gtk-window-get-window-type)

;;; ----------------------------------------------------------------------------
;;; gtk_window_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_move" gtk-window-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[x]{X coordinate to move window to}
  @argument[y]{Y coordinate to move window to}
  @begin{short}
    Asks the window manager to move window to the given position.
  @end{short}
  Window managers are free to ignore this; most window managers ignore requests
  for initial window positions, instead using a user-defined placement
  algorithm, and honor requests after the window has already been shown.

  Note: The position is the position of the gravity-determined reference point
  for the window. The gravity determines two things: first, the location of
  the reference point in root window coordinates; and second, which point on
  the window is positioned at the reference point.

  By default the gravity is @code{:north-west}, so the reference point is
  simply the x, y supplied to the @sym{gtk-window-move} function. The top-left
  corner of the window decorations, aka window frame or border, will be placed
  at x, y. Therefore, to position a window at the top left of the screen, you
  want to use the default gravity, which is @code{:north-west}, and move the
  window to 0,0.

  To position a window at the bottom right corner of the screen, you would set
  @code{:south-east}, which means that the reference point is at x + the
  window width and y + the window height, and the bottom-right corner of the
  window border will be placed at that reference point. So, to place a window
  in the bottom right corner you would first set gravity to south east, then
  write: @code{(gtk-window-move window (- (gdk-screen-width) window-width)
  (- (gdk-screen-height) window-height))}. Note that this example does not take
  multi-head scenarios into account.

  The Extended Window Manager Hints specification at
  http://www.freedesktop.org/Standards/wm-spec has a nice table of gravities
  in the \"implementation notes\" section.

  The @fun{gtk-window-get-position} function documentation may also be relevant.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (x :int)
  (y :int))

(export 'gtk-window-move)

;;; ----------------------------------------------------------------------------
;;; gtk_window_parse_geometry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_parse_geometry" gtk-window-parse-geometry) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[geometry]{geometry string}
  @return{@em{True} if string was parsed successfully.}
  @begin{short}
    Parses a standard X Window System geometry string - see the manual page for
    X (type 'man X') for details on this.
  @end{short}
  The @sym{gtk-window-parse-geometry} function does work on all GTK+ ports
  including Win32 but is primarily intended for an X environment.

  If either a size or a position can be extracted from the geometry string, the
  @sym{gtk-window-parse-geometry} function returns @em{true} and calls the
  @fun{gtk-window-default-size} and/or @fun{gtk-window-move} functions
  to resize/move the window.

  If the @sym{gtk-window-parse-geometry} functions returns @em{true}, it will
  also set the @code{:user-pos} and/or @code{:user-size} hints indicating to the
  window manager that the size/position of the window was user specified. This
  causes most window managers to honor the geometry.

  Note that for the @sym{gtk-window-parse-geometry} function to work as
  expected, it has to be called when the window has its \"final\" size, i. e.
  after calling the @fun{gtk-widget-show-all} function on the contents and the
  @fun{gtk-window-set-geometry-hints} function on the window.
  @begin{pre}
 #include <gtk/gtk.h>

 static void
 fill_with_content (GtkWidget *vbox)
 {
   /* fill with content... */
 @}

 int
 main (int argc, char *argv[])
 {
   GtkWidget *window, *vbox;
   GdkGeometry size_hints = {
     100, 50, 0, 0, 100, 50, 10, 10, 0.0, 0.0, GDK_GRAVITY_NORTH_WEST
   @};

   gtk_init (&argc, &argv);

   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, FALSE, 0);

   gtk_container_add (GTK_CONTAINER (window), vbox);
   fill_with_content (vbox);
   gtk_widget_show_all (vbox);

   gtk_window_set_geometry_hints (GTK_WINDOW (window),
                     window,
                     &size_hints,
                     GDK_HINT_MIN_SIZE |
                     GDK_HINT_BASE_SIZE |
                     GDK_HINT_RESIZE_INC);

   if (argc > 1)
     {
       if (!gtk_window_parse_geometry (GTK_WINDOW (window), argv[1]))
         fprintf (stderr, \"Failed to parse '%s'\n\", argv[1]);
     @}

   gtk_widget_show_all (window);
   gtk_main ();

   return 0;
 @}
  @end{pre}
  @begin[Warning]{dictionary}
    The @sym{gtk-window-parse-geometry} function has been deprecated since
    version 3.20 and should not be used in newly-written code.
    Geometry handling in GTK is deprecated.
  @end{dictionary}
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (geometry :string))

(export 'gtk-window-parse-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_reshow_with_initial_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_reshow_with_initial_size"
          gtk-window-reshow-with-initial-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-18}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Hides the window, then reshows it, resetting the default size and position
    of the window.
  @end{short}
  Used by GUI builders only.
  @begin[Warning]{dictionary}
    The @sym{gtk-window-reshow-with-initial-size} function has been deprecated
    since version 3.10 and should not be used in newly-written code.

    GUI builders can call the @fun{gtk-widget-hide}, @fun{gtk-widget-unrealize}
    and then @fun{gtk-widget-show} functions on window themselves, if they still
    need this functionality.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-widget-hide}
  @see-function{gtk-widget-show}
  @see-function{gtk-widget-undrealize}"
  (window (g-object gtk-window)))

(export 'gtk-window-reshow-with-initial-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_resize" gtk-window-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[width]{width in pixels to resize the @arg{window} to}
  @argument[height]{height in pixels to resize the @arg{window} to}
  @begin{short}
    Resizes the window as if the user had done so, obeying geometry constraints.
  @end{short}
  The default geometry constraint is that windows may not be smaller than their
  size request; to override this constraint, call the
  @fun{gtk-widget-size-request} function to set the window's request to a
  smaller value.

  If the @sym{gtk-window-resize} function is called before showing a window for
  the first time, it overrides any default size set with the
  @fun{gtk-window-default-size} function.

  Windows may not be resized smaller than 1 by 1 pixels.
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_to_geometry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_resize_to_geometry" gtk-window-resize-to-geometry) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[window]{a @class{gtk-window} widget}
  @argument[width]{width in resize increments to resize the @arg{window} to}
  @argument[height]{height in resize increments to resize the @arg{window} to}
  @begin{short}
    Like the @fun{gtk-window-resize} function, but @arg{width} and @arg{height}
    are interpreted in terms of the base size and increment set with
    the @fun{gtk-window-set-geometry-hints} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-window-resize-to-geometry} function has been deprecated since
    version 3.20 and should not be used in newly-written code.
    This function does nothing. Use the @fun{gtk-window-resize} function and
    compute the geometry yourself.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-window-resize}
  @see-function{gtk-window-set-geometry-hints}"
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-resize-to-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_list" %gtk-window-set-default-icon-list)
    :void
  (icon-list (g-list (g-object gdk-pixbuf))))

(defun gtk-window-set-default-icon-list (icon-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-13}
  @argument[list]{a list of @class{gdk-pixbuf} objects}
  @begin{short}
    Sets an icon list to be used as fallback for windows that have not had the
    @fun{gtk-window-set-icon-list} function called on them to set up a window
    specific icon list.
  @end{short}
  This function allows you to set up the icon for all windows in your app at
  once.

  See the @fun{gtk-window-set-icon-list} function for more details.
  @see-class{gtk-window}
  @see-function{gtk-window-set-icon-list}"
  ;; We have to pass a list of pointers.
  (%gtk-window-set-default-icon-list (mapcar #'pointer icon-list)))

(export 'gtk-window-set-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon" gtk-window-set-default-icon) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-5}
  @argument[icon]{the icon of type @class{gdk-pixbuf}}
  @begin{short}
    Sets an icon to be used as fallback for windows that have not had the
    @fun{gtk-window-set-icon} function called on them from a pixbuf.
  @end{short}
  @see-class{gtk-window}
  @see-class{gdk-pixbuf}
  @see-function{gtk-window-set-icon}"
  (icon (g-object gdk-pixbuf)))

(export 'gtk-window-set-default-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_from_file"
          %gtk-window-set-default-icon-from-file) :boolean
  (filename :string)
  (err :pointer))

(defun gtk-window-set-default-icon-from-file (filename)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[filename]{location of icon file}
  @return{@em{True} if setting the icon succeeded.}
  @begin{short}
    Sets an icon to be used as fallback for windows that have not had the
    @fun{gtk-window-set-icon-list} function called on them from a file on disk.
  @end{short}
  @see-class{gtk-window}
  @see-function{gtk-window-set-icon-list}"
  (with-g-error (err)
    (%gtk-window-set-default-icon-from-file filename err)))

(export 'gtk-window-set-default-icon-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_name" gtk-window-set-default-icon-name)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[name]{the name of the themed icon}
  @begin{short}
    Sets an icon to be used as fallback for windows that haven't had the
    @fun{gtk-window-set-icon-list} function called on them from a named themed
    icon.
  @end{short}
  See the @fun{gtk-window-set-icon-name} function.
  @see-class{gtk-window}"
  (name :string))

(export 'gtk-window-set-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_list ()
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation. It is not correct.

(defcfun ("gtk_window_set_icon_list" gtk-window-set-icon-list) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[list]{list of @class{gdk-pixbuf} objects}
  @begin{short}
    Sets up the icon representing a @class{gtk-window} widget.
  @end{short}
  The icon is used when the window is minimized, also known as iconified.
  Some window managers or desktop environments may also place it in the window
  frame, or display it in other contexts.

  The @sym{gtk-window-set-icon-list} function allows you to pass in the same
  icon in several hand-drawn. The list should contain the natural sizes your
  icon is available in; that is, don't scale the image before passing it to
  GTK+. Scaling is postponed until the last minute, when the desired final size
  is known, to allow best quality.

  By passing several sizes, you may improve the final image quality of the
  icon, by reducing or eliminating automatic image scaling.

  Recommended sizes to provide: 16 x 16, 32 x 32, 48 x 48 at minimum, and larger
  images (64 x 64, 128 x 128) if you have them.

  See also the @fun{gtk-window-set-default-icon-list} function to set the icon
  for all windows in your application in one go.

  Note that transient windows, those who have been set transient for another
  window using the @fun{gtk-window-transient-for} function, will inherit their
  icon from their transient parent. So there's no need to explicitly set the
  icon on transient windows.
  @see-class{gtk-window}
  @see-function{gtk-window-set-default-icon-list}
  @see-function{gtk-window-transient-for}"
  (window (g-object gtk-window))
  (g-list (g-object gdk-pixbuf) :free-to-foreign t))

(export 'gtk-window-set-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_icon_from_file" %gtk-window-set-icon-from-file)
    :boolean
  (window (g-object gtk-window))
  (filename :string)
  (error :pointer))

(defun gtk-window-set-icon-from-file (window filename)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[window]{a @class{gtk-window} widget}
  @argument[filename]{location of icon file}
  @return{@em{True} if setting the icon succeeded.}
  @begin{short}
    Sets the icon for the window.
  @end{short}

  This function is equivalent to calling the @fun{gtk-window-set-icon} function
  with a pixbuf created by loading the image from filename.
  @see-class{gtk-window}
  @see-function{gtk-window-set-icon}"
  (with-g-error (err)
    (%gtk-window-set-icon-from-file window filename err)))

(export 'gtk-window-set-icon-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_auto_startup_notification ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_auto_startup_notification"
           gtk-window-set-auto-startup-notification) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-16}
  @argument[setting]{@em{true} to automatically do startup notification}
  @begin{short}
    By default, after showing the first @class{gtk-window}, GTK+ calls the
    @fun{gdk-notify-startup-complete} function. Call this function to disable
    the automatic startup notification.
  @end{short}
  You might do this if your first window is a splash screen, and you want to
  delay notification until after your real main window has been shown, for
  example.

  In that example, you would disable startup notification temporarily, show
  your splash screen, then re-enable it so that showing the main window would
  automatically result in notification.
  @see-class{gtk-window}
  @see-function{gdk-notify-startup-complete}"
  (setting :boolean))

(export 'gtk-window-set-auto-startup-notification)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_grip_is_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-resize-grip-is-visible))

(defun gtk-window-resize-grip-is-visible (window)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-9}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if a resize grip exists and is visible.}
  @begin{short}
    Determines whether a resize grip is visible for the specified window.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-window-resize-grip-is-visible} function has been deprecated
    since version 3.14 and should not be used in newly-written code.
    Resize grips have been removed.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-window-has-resize-grip}"
  (and (gtk-window-has-resize-grip window)
       (gtk-window-resize-grip-visible window)))

(export 'gtk-window-resize-grip-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_resize_grip_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_resize_grip_area" %gtk-window-get-resize-grip-area)
    :boolean
  (window (g-object gtk-window))
  (rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-window-get-resize-grip-area (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[window]{a @class{gtk-window} widget}
  @begin{return}
    @code{rect} -- a @class{gdk-rectangle} with the resize grip area.
  @end{return}
  @begin{short}
    If the window has a resize grip, this function will retrieve the grip
    position, width and height into the specified @arg{rect}.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-window-get-resize-grip-area} function has been deprecated since
    version 3.14 and should not be used in newly-written code.
    Resize grips have been removed.
  @end{dictionary}
  @see-class{gtk-window}
  @see-function{gtk-window-has-resize-grip}"
  (let ((rect (make-gdk-rectangle)))
    (when (%gtk-window-get-resize-grip-area window rect)
      rect)))

(export 'gtk-window-get-resize-grip-area)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_has_user_ref_count ()
;;;
;;; void gtk_window_set_has_user_ref_count (GtkWindow *window, gboolean setting)
;;;
;;; Tells GTK+ whether to drop its extra reference to the window when
;;; gtk_window_destroy() is called.
;;;
;;; This function is only exported for the benefit of language bindings which
;;; may need to keep the window alive until their wrapper object is garbage
;;; collected. There is no justification for ever calling this function in an
;;; application.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     the new value
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_titlebar ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_window_set_titlebar" gtk-window-set-titlebar) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-4-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[widget]{a @class{gtk-widget} object to use as titlebar}
  @begin{short}
    Sets a custom titlebar for the window.
  @end{short}

  If you set a custom titlebar, GTK+ will do its best to convince the window
  manager not to put its own titlebar on the window. Depending on the system,
  this function may not work for a window that is already visible, so you set
  the titlebar before calling the @fun{gtk-widget-show} function.

  Since 3.10
  @see-class{gtk-window}"
  (window (g-object gtk-window))
  (titlebar (g-object gtk-widget)))

#+gtk-3-10
(export 'gtk-window-set-titlebar)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_titlebar ()
;;; ----------------------------------------------------------------------------

#+gtk-3-16
(defcfun ("gtk_window_get_titlebar" gtk-window-get-titlebar)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The @class{gtk-widget} custom titlebar, or @code{nil}.}
  @begin{short}
    Returns the custom titlebar that has been set with the
    @fun{gtk-window-set-titlebar} function.
  @end{short}

  Since 3.16
  @see-class{gtk-window}"
  (window (g-object gtk-window)))

#+gtk-3-16
(export 'gtk-window-get-titlebar)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_interactive_debugging () -> gtk-window-interactice-debugging
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcfun ("gtk_window_set_interactive_debugging"
           gtk-window-interactive-debugging) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-26}
  @argument[enable]{@em{true} to enable interactice debugging}
  @begin{short}
    Opens or closes the interactive debugger, which offers access to the widget
    hierarchy of the application and to useful debugging tools.
  @end{short}

  Since 3.14
  @see-class{gtk-window}"
  (enable :boolean))

#+gtk-3-14
(export 'gtk-window-interactive-debugging)

;;; --- End of file gtk.window.lisp --------------------------------------------
