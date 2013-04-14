;;; ----------------------------------------------------------------------------
;;; gtk.window.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkWindow
;;;
;;; Toplevel which can contain other widgets
;;;
;;; Synopsis
;;;
;;;     GtkWindow
;;;
;;;     gtk_window_new
;;;     gtk_window_set_title
;;;     gtk_window_set_wmclass
;;;     gtk_window_set_resizable
;;;     gtk_window_get_resizable
;;;     gtk_window_add_accel_group
;;;     gtk_window_remove_accel_group
;;;     gtk_window_activate_focus
;;;     gtk_window_activate_default
;;;     gtk_window_set_modal
;;;     gtk_window_set_default_size
;;;     gtk_window_set_default_geometry
;;;     gtk_window_set_geometry_hints
;;;     gtk_window_set_gravity
;;;     gtk_window_get_gravity
;;;     gtk_window_set_position
;;;     gtk_window_set_transient_for
;;;     gtk_window_set_attached_to
;;;     gtk_window_set_destroy_with_parent
;;;     gtk_window_set_hide_titlebar_when_maximized
;;;     gtk_window_set_screen
;;;     gtk_window_get_screen
;;;     gtk_window_is_active
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
;;;     gtk_window_iconify
;;;     gtk_window_deiconify
;;;     gtk_window_stick
;;;     gtk_window_unstick
;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_fullscreen
;;;     gtk_window_unfullscreen
;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below
;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag
;;;     gtk_window_set_decorated
;;;     gtk_window_set_deletable
;;;     gtk_window_set_mnemonic_modifier
;;;     gtk_window_set_type_hint
;;;     gtk_window_set_skip_taskbar_hint
;;;     gtk_window_set_skip_pager_hint
;;;     gtk_window_set_urgency_hint
;;;     gtk_window_set_accept_focus
;;;     gtk_window_set_focus_on_map
;;;     gtk_window_set_startup_id
;;;     gtk_window_set_role
;;;     gtk_window_get_decorated
;;;     gtk_window_get_deletable
;;;     gtk_window_get_default_icon_list
;;;     gtk_window_get_default_icon_name
;;;     gtk_window_get_default_size
;;;     gtk_window_get_destroy_with_parent
;;;     gtk_window_get_hide_titlebar_when_maximized
;;;     gtk_window_get_icon
;;;     gtk_window_get_icon_list
;;;     gtk_window_get_icon_name
;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_get_modal
;;;     gtk_window_get_position
;;;     gtk_window_get_role
;;;     gtk_window_get_size
;;;     gtk_window_get_title
;;;     gtk_window_get_transient_for
;;;     gtk_window_get_attached_to
;;;     gtk_window_get_type_hint
;;;     gtk_window_get_skip_taskbar_hint
;;;     gtk_window_get_skip_pager_hint
;;;     gtk_window_get_urgency_hint
;;;     gtk_window_get_accept_focus
;;;     gtk_window_get_focus_on_map
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
;;;     gtk_window_set_icon
;;;     gtk_window_set_icon_list
;;;     gtk_window_set_icon_from_file
;;;     gtk_window_set_icon_name
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity
;;;     gtk_window_get_mnemonics_visible
;;;     gtk_window_set_mnemonics_visible
;;;     gtk_window_get_focus_visible
;;;     gtk_window_set_focus_visible
;;;     gtk_window_set_has_resize_grip
;;;     gtk_window_get_has_resize_grip
;;;     gtk_window_resize_grip_is_visible
;;;     gtk_window_get_resize_grip_area
;;;     gtk_window_get_application
;;;     gtk_window_set_application
;;;     gtk_window_set_has_user_ref_count
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
    "title" :string  t t)
   (transient-for
    gtk-window-transient-for
    "transient-for" "GtkWindow" t t)
   (type
    gtk-window-type
    "type" "GtkWindowType" t nil)
   (type-hint
    gtk-window-type-hint
    "type-hint" "GdkWindowTypeHint" t t)
   ;; "ubuntu-no-proxy" is not documented. Special for Ubuntu?
   (ubuntu-no-proxy
    gtk-window-ubuntu-no-proxy
    "ubuntu-no-proxy" "gboolean" t nil)
   (urgency-hint
    gtk-window-urgency-hint
    "urgency-hint" "gboolean" t t)
   (window-position
    gtk-window-window-position
    "window-position" "GtkWindowPosition" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-window 'type)
 "@version{2013-3-28}
  @begin{short}
    A @sym{gtk-window} is a toplevel window which can contain other widgets.
    Windows normally have decorations that are under the control of the
    windowing system and allow the user to manipulate the window (resize it,
    move it, close it, ...).

    GTK+ also allows windows to have a resize grip (a small area in the lower
    right or left corner) which can be clicked to reszie the window. To control
    whether a window has a resize grip, use
    @fun{gtk-window-set-has-resize-grip}.
  @end{short}

  @subheading{GtkWindow as GtkBuildable}
    The @sym{gtk-window} implementation of the @class{gtk-buildable} interface
    supports a custom @code{<accel-groups>} element, which supports any number
    of @code{<group>} elements representing the @class{gtk-accel-group} objects
    you want to add to your window. This is synonymous with
    @fun{gtk-window-add-accel-group}.

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
  @begin[Style Property Details]{dictionary}
    @subheading{The \"resize-grip-height\" style property}
      @code{\"resize-grip-height\"} of type @code{:int} (Read / Write)@br{}
      Height of resize grip.@br{}
      Allowed values: >= 0@br{}
      Default value: 16

    @subheading{The \"resize-grip-width\" style property}
      @code{\"resize-grip-width\"} of type @code{:int} (Read / Write)@br{}
      Width of resize grip.@br{}
      Allowed values: >= 0@br{}
      Default value: 16
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-default\" signal}
      @begin{pre}
 lambda (window)   : Action
      @end{pre}
      The \"activate-default\" signal is a keybinding signal which gets emitted
      when the user activates the default widget of window.
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
    @subheading{The \"activate-focus\" signal}
      @begin{pre}
 lambda (window)   : Action
      @end{pre}
      The \"activate-focus\" signal is a keybinding signal which gets emitted
      when the user activates the currently focused widget of window.
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
    @subheading{The \"keys-changed\" signal}
      @begin{pre}
 lambda (window)   : Run First
      @end{pre}
      The \"keys-changed\" signal gets emitted when the set of accelerators or
      mnemonics that are associated with window changes.
      @begin[code]{table}
        @entry[window]{The window which received the signal.}
      @end{table}
    @subheading{The \"set-focus\" signal}
      @begin{pre}
 lambda (window widget)   : Run Last
      @end{pre}
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
  @see-slot{gtk-window-window-position}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accept-focus" 'gtk-window) 't)
 "The @code{\"accept-focus\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the window should receive the input focus.@br{}
  Default value: @em{true}@br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "application" 'gtk-window) 't)
 "The @code{\"application\"} property of type @class{gtk-application}
  (Read / Write)@br{}
  The @class{gtk-application} associated with the window.
  The application will be kept alive for at least as long as it has any
  windows associated with it (see @fun{g-application-hold} for a way to keep
  it alive without windows).
  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it by setting
  the @code{\"application\"} property to @code{nil}.@br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attached-to" 'gtk-window) 't)
 "The @code{\"attached-to\"} property of type @class{gtk-widget}
  (Read / Write / Construct)@br{}
  The widget to which this window is attached.
  See @fun{gtk-window-set-attached-to}.
  Examples of places where specifying this relation is useful are for instance
  a @class{gtk-menu} created by a @class{gtk-combo-box}, a completion popup
  window created by @class{gtk-entry} or a typeahead search entry created by
  @class{gtk-tree-view}.@br{}
  Since 3.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "decorated" 'gtk-window) 't)
 "The @code{\"decorated\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the window should be decorated by the window manager.@br{}
  Default value: @em{true}@br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-height" 'gtk-window) 't)
 "The @code{\"default-height\"} property of type @code{:int} (Read / Write)@br{}
  The default height of the window, used when initially showing the window.@br{}
  Allowed values: >= @code{G_MAXULONG}@br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-width" 'gtk-window) 't)
 "The @code{\"default-width\"} property of type @code{:int} (Read / Write)@br{}
  The default width of the window, used when initially showing the window.@br{}
  Allowed values: >= @code{G_MAXULONG}@br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "deletable" 'gtk-window) 't)
 "The @code{\"deletable\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the window frame should have a close button.@br{}
  Default value: @em{true}@br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "destroy-with-parent"
                                               'gtk-window) 't)
 "The @code{\"destroy-with-parent\"} property of type @code{:boolean}
  (Read / Write)@br{}
  If this window should be destroyed when the parent is destroyed.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-map" 'gtk-window) 't)
 "The @code{\"focus-on-map\"} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the window should receive the input focus when mapped.@br{}
  Default value: @em{true}@br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-visible" 'gtk-window) 't)
 "The @code{\"focus-visible\"} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether @code{\"focus rectangles\"} are currently visible in this window.
  This property is maintained by GTK+ based on the @code{\"gtk-visible-focus\"}
  setting and user input and should not be set by applications.@br{}
  Default value: @em{true}@br{}
  Since 2.20")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gravity" 'gtk-window) 't)
 "The @code{\"gravity\"} property of type @symbol{gdk-gravity} (Read / Write)
  @br{}
  The window gravity of the window. See @fun{gtk-window-move} and
  @symbol{gdk-gravity} for more details about window gravity.@br{}
  Default value: @code{:north-west}@br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-resize-grip"
                                               'gtk-window) 't)
 "The @code{\"has-resize-grip\"} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the window has a corner resize grip.
  Note that the resize grip is only shown if the window is actually resizable
  and not maximized. Use @code{\"resize-grip-visible\"} to find out if the
  resize grip is currently shown.@br{}
  Default value: @em{true}@br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-toplevel-focus"
                                               'gtk-window) 't)
 "The @code{\"has-toplevel-focus\"} property of type @code{:boolean} (Read)@br{}
  Whether the input focus is within this @sym{gtk-window}.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hide-titlebar-when-maximized"
                                               'gtk-window) 't)
 "The @code{\"hide-titlebar-when-maximized\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the titlebar should be hidden during maximization.@br{}
  Default value: @code{nil}@br{}
  Since 3.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon" 'gtk-window) 't)
 "The @code{\"icon\"} property of type @class{gdk-pixbuf} (Read / Write)@br{}
  Icon for this window.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-window) 't)
 "The @code{\"icon-name\"} property of type @code{:string} (Read / Write)@br{}
  The @code{\"icon-name\"} property specifies the name of the themed icon to
  use as the window icon. See @class{gtk-icon-theme} for more details.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-active" 'gtk-window) 't)
 "The @code{\"is-active\"} property of type @code{:boolean} (Read)@br{}
  Whether the toplevel is the current active window.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mnemonics-visible"
                                               'gtk-window) 't)
 "The @code{\"mnemonics-visible\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether mnemonics are currently visible in this window.
  This property is maintained by GTK+ based on the @code{\"gtk-auto-mnemonics\"}
  setting and user input, and should not be set by applications.@br{}
  Default value: @em{true}@br{}
  Since 2.20")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "modal" 'gtk-window) 't)
 "The @em{\"modal\"} property of type @code{:boolean} (Read / Write)@br{}
  If @em{true}, the window is modal (other windows are not usable while this
  one is up).@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "opacity" 'gtk-window) 't)
 "The @code{\"opacity\"} property of type @code{:double} (Read / Write)@br{}
  The requested opacity of the window. See @fun{gtk-window-set-opacity} for more
  details about window opacity.@br{}
  Allowed values: [0,1]@br{}
  Default value: 1@br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resizable" 'gtk-window) 't)
 "The @code{\"resizable\"} property of type @code{:boolean} (Read / Write)@br{}
  If @em{true}, users can resize the window.@br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resize-grip-visible"
                                               'gtk-window) 't)
 "The @code{\"resize-grip-visible\"} property of type @code{:boolean}
  (Read)@br{}
  Whether a corner resize grip is currently shown.@br{}
  Default value: @code{nil}@br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "role" 'gtk-window) 't)
 "The @code{\"role\"} property of type @code{:string} (Read / Write)@br{}
  Unique identifier for the window to be used when restoring a session.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-window) 't)
 "The @code{\"screen\"} property of type @class{gdk-screen} (Read / Write)@br{}
  The screen where this window will be displayed.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "skip-pager-hint"
                                               'gtk-window) 't)
 "The @code{\"skip-pager-hint\"} property of type @code{:boolean}
  (Read / Write)@br{}
  @em{true} if the window should not be in the pager.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "skip-taskbar-hint"
                                               'gtk-window) 't)
 "The @code{\"skip-taskbar-hint\"} property of type @code{:boolean}
  (Read / Write)@br{}
  @em{true} if the window should not be in the task bar.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "startup-id" 'gtk-window) 't)
 "The @code{\"startup-id\"} property of type @code{:string} (Write)@br{}
  The @code{\"startup-id\"} is a write-only property for setting window's
  startup notification identifier. See @fun{gtk-window-set-startup-id} for more
  details.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-window) 't)
 "The @code{\"title\"} property of type @code{:string} (Read / Write)@br{}
  The title of the window.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transient-for" 'gtk-window) 't)
 "The @code{\"transient-for\"} property of type @sym{gtk-window}
  (Read / Write / Construct)@br{}
  The transient parent of the window. See @fun{gtk-window-set-transient-for} for
  more details about transient windows.@br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "type" 'gtk-window) 't)
 "The @code{\"type\"} property of type @symbol{gtk-window-type}
  (Read / Write / Construct)@br{}
  The type of the window.@br{}
  Default value: @code{:toplevel}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "type-hint" 'gtk-window) 't)
 "The @code{\"type-hint\"} property of type @symbol{gdk-window-type-hint}
  (Read / Write)@br{}
  Hint to help the desktop environment understand what kind of window this is
  and how to treat it.@br{}
  Default value: @code{:hint-normal}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "urgency-hint" 'gtk-window) 't)
 "The @code{\"urgency-hint\"} property of type @code{:boolean}
  (Read / Write)@br{}
  @em{True} if the window should be brought to the user's attention.@br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-position"
                                               'gtk-window) 't)
 "The @code{\"window-position\"} property of type @symbol{gtk-window-position}
  (Read / Write)@br{}
  The initial position of the window.@br{}
  Default value: @code{:none}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-accept-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-accept-focus 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"accept-focus\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-application atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-application 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"application\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-attached-to atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-attached-to 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"attached-to\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-decorated atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-decorated 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"decorated\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-default-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-default-height 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"default-height\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-default-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-default-width 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"default-width\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-deletable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-deletable 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"deletable\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-destroy-with-parent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-destroy-with-parent 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"destroy-with-parent\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-focus-on-map atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-focus-on-map 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"focus-on-map\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-focus-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-focus-visible 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"focus-visible\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-gravity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-gravity 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"gravity\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-has-resize-grip atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-has-resize-grip 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"has-resize-grip\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-has-toplevel-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-has-toplevel-focus 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"has-toplevel-focus\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-hide-titlebar-when-maximized atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-hide-titlebar-when-maximized 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"hide-titlebar-when-maximized\"} of the
  @class{gtk-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-icon 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"icon\"} of the @class{gtk-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-icon-name 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"icon-name\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-is-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-is-active 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"is-active\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-mnemonics-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-mnemonics-visible 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"mnemonics-visible\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-modal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-modal 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"modal\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-opacity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-opacity 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"opacity\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-resizable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-resizable 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"resizable\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-resize-grip-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-resize-grip-visible 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"resize-grip-visible\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-role atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-role 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"role\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-screen 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"screen\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-skip-pager-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-skip-pager-hint 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"skip-pager-hint\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-skip-taskbar-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-skip-taskbar-hint 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"skip-taskbar-hint\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-startup-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-startup-id 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"startup-id\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-title 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"title\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-transient-for atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-transient-for 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"transient-for\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-type 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"type\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-type-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-type-hint 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"type-hint\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-urgency-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-urgency-hint 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"urgency-hint\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-window-window-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-window-window-position 'function)
 "@version{2013-3-29}
  Accessor of the slot @code{\"window-position\"} of the @class{gtk-window}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_window_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-new))

(defun gtk-window-new (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[type]{type of window, one of the @symbol{gtk-window-type}
    enumeration}
  @return{A new @class{gtk-window} widget.}
  @begin{short}
    Creates a new @class{gtk-window} widget, which is a toplevel window that
    can contain other widgets.
  @end{short}
  Nearly always, the type of the window should be @code{:toplevel}. If you are
  implementing something like a popup menu from scratch (which is a bad idea,
  just use the @class{gtk-menu} class), you might use @code{:popup}.
  @code{:popup} is not for dialogs, though in some other toolkits dialogs are
  called \"popups\". In GTK+, @code{:popup} means a pop-up menu or pop-up
  tooltip. On X11, popup windows are not controlled by the window manager.

  If you simply want an undecorated window (no window borders), use
  @fun{gtk-window-set-decorated}, don't use @code{:popup}.
  @see-symbol{gtk-window-type}
  @see-function{gtk-window-set-decorated}"
  (make-instance 'gtk-window :type type))

(export 'gtk-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_title ()
;;;
;;; void gtk_window_set_title (GtkWindow *window, const gchar *title);
;;;
;;; Sets the title of the GtkWindow. The title of a window will be displayed in
;;; its title bar; on the X Window System, the title bar is rendered by the
;;; window manager, so exactly how the title appears to users may vary according
;;; to a user's exact configuration. The title should help a user distinguish
;;; this window from other windows they may have open. A good title might
;;; include the application name and current document filename, for example.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; title :
;;;     title of the window
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-title))

(defun gtk-window-set-title (window title)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[title]{title of the @arg{window}}
  @begin{short}
    Sets the title of @arg{window}.
  @end{short}
  The title of a window will be displayed in its title bar; on the X Window
  System, the title bar is rendered by the window manager, so exactly how the
  title appears to users may vary according to a user's exact configuration. The
  title should help a user distinguish this window from other windows they may
  have open. A good title might include the application name and current
  document filename, for example.
  @see-function{gtk-window-get-title}"
  (setf (gtk-window-title window) title))

(export 'gtk-window-set-title)

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
;;; gtk_window_set_resizable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-resizeable))

(defun gtk-window-set-resizable (window resizable)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[resizable]{@em{true} if the user can resize this @arg{window}}
  Sets whether the user can resize a window. Windows are user resizable by
  default."
  (setf (gtk-window-resizable window) resizable))

(export 'gtk-window-set-resizable)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_resizable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-resizable))

(defun gtk-window-get-resizable (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if the user can resize the @arg{window}.}
  Gets the value set by @fun{gtk-window-set-resizable}.
  @see-function{gtk-window-set-resizable}"
  (gtk-window-resizable window))

(export 'gtk-window-get-resizable)

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_accel_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_add_accel_group" gtk-window-add-accel-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{@arg{window} to attach accelerator group to}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  Associate @arg{accel-group} with @arg{window}, such that calling
  @fun{gtk-accel-group-activate} on @arg{window} will activate accelerators
  in @arg{accel-group}.
  @see-function{gtk-accel-group-activate}"
  (window (g-object gtk-window))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-window-add-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_accel_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_remove_accel_group" gtk-window-remove-accel-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  Reverses the effects of @fun{gtk-window-add-accel-group}.
  @see-function{gtk-window-add-accel-group}"
  (window (g-object gtk-window))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-window-remove-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_focus" gtk-window-activate-focus) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if a widget got activated.}
  Activates the current focused widget within the window."
  (window (g-object gtk-window)))

(export 'gtk-window-activate-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_activate_default" gtk-window-activate-default) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if a widget got activated.}
  Activates the default widget for the @arg{window}, unless the current
  focused widget has been configured to receive the default action (see
  @fun{gtk-widget-set-receives-default}), in which case the focused widget is
  activated.
  @see-function{gtk-widget-set-receives-default}"
  (window (g-object gtk-window)))

(export 'gtk-window-activate-default)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_modal ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-modal))

(defun gtk-window-set-modal (window modal)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[modal]{whether the @arg{window} is modal}
  Sets a @arg{window} modal or non-modal. Modal windows prevent interaction with
  other windows in the same application. To keep modal dialogs on top of main
  application windows, use @fun{gtk-window-set-transient-for} to make the dialog
  transient for the parent; most window managers will then disallow lowering
  the dialog below the parent.
  @see-function{gtk-window-set-transient-for}"
  (setf (gtk-window-modal window) modal))

(export 'gtk-window-set-modal)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-default-size))

(defun gtk-window-set-default-size (window width height)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[width]{width in pixels, or -1 to unset the default width}
  @argument[height]{height in pixels, or -1 to unset the default height}
  @begin{short}
    Sets the default size of a window.
  @end{short}
  If the window's \"natural\" size (its size request) is larger than the
  default, the default will be ignored. More generally, if the default size does
  not obey the geometry hints for the window
  (@fun{gtk-window-set-geometry-hints} can be used to set these explicitly), the
  default size will be clamped to the nearest permitted size.

  Unlike @fun{gtk-widget-set-size-request}, which sets a size request for a
  widget and thus would keep users from shrinking the window, this function only
  sets the initial size, just as if the user had resized the window themselves.
  Users can still shrink the window again as they normally would. Setting a
  default size of -1 means to use the \"natural\" default size (the size request
  of the window).

  For more control over a window's initial size and how resizing works,
  investigate @fun{gtk-window-set-geometry-hints}.

  For some uses, @fun{gtk-window-resize} is a more appropriate function.
  @fun{gtk-window-resize} changes the current size of the window, rather than
  the size to be used on initial display. @fun{gtk-window-resize} always affects
  the window itself, not the geometry widget.

  The default size of a window only affects the first time a window is shown;
  if a window is hidden and re-shown, it will remember the size it had prior
  to hiding, rather than using the default size.

  Windows can't actually be 0 x 0 in size, they must be at least 1 x 1, but
  passing 0 for width and height is OK, resulting in a 1 x 1 default size.
  @see-function{gtk-window-set-geometry-hints}
  @see-function{gtk-widget-set-size-request}
  @see-function{gtk-window-resize}
  @see-function{gtk-window-default-width}
  @see-function{gtk-window-default-height}"
  (setf (gtk-window-default-width window) width
        (gtk-window-default-height window) height))

(export 'gtk-window-set-default-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_geometry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_geometry" gtk-window-set-default-geometry)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[width]{width in resize increments, or -1 to unset the default width}
  @argument[height]{height in resize increments, or -1 to unset the default
    height}
  @begin{short}
    Like @fun{gtk-window-set-default-size}, but @arg{width} and @arg{height} are
    interpreted in terms of the base size and increment set
    with @fun{gtk-window-set-geometry-hints}.
  @end{short}

  Since 3.0
  @see-function{gtk-window-set-default-size}
  @see-function{gtk-window-set-geometry-hints}
  @see-function{gtk-window-set-default-size}"
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-set-default-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_geometry_hints ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_geometry_hints" gtk-window-set-geometry-hints) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[geometry-widget]{widget the geometry hints will be applied to or
    @code{nil}}
  @argument[geometry]{struct containing geometry information or @code{nil}}
  @argument[geometry-mask]{mask indicating which struct fields should be paid
    attention to}
  @begin{short}
    This function sets up hints about how a window can be resized by the user.
  @end{short}
  You can set a minimum and maximum size; allowed resize increments (e. g. for
  xterm, you can only resize by the size of a character); aspect ratios; and
  more. See the @class{gdk-geometry} struct.
  @see-class{gdk-geometry}"
  (window (g-object gtk-window))
  (geometry-widget (g-object gtk-widget))
  (geometry (g-boxed-foreign gdk-geometry))
  (geometry-mask gdk-window-hints))

(export 'gtk-window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_gravity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_gravity" gtk-window-set-gravity) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[gravity]{@arg{window} gravity}
  @begin{short}
    Window gravity defines the meaning of coordinates passed to
    @fun{gtk-window-move}. See @fun{gtk-window-move} and @symbol{gdk-gravity}
    for more details.
  @end{short}

  The default window gravity is @code{:north-west} which will typically
  \"do what you mean\".
  @see-symbol{gdk-gravity}
  @see-function{gtk-window-move}"
  (window (g-object gtk-window))
  (gravity gdk-gravity))

(export 'gtk-window-set-gravity)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_gravity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_gravity" gtk-window-get-gravity) gdk-gravity
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The @arg{window} gravity.}
  @begin{short}
    Gets the value set by @fun{gtk-window-set-gravity} for @arg{window}.
  @end{short}
  @see-function{gtk-window-set-gravity}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-gravity)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_position ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-position))

(defun gtk-window-set-position (window position)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[position]{a position constraint}
  @begin{short}
    Sets a position constraint for this @arg{window}.
  @end{short}
  If the old or new constraint is @code{:center-always}, this will also cause
  the @arg{window} to be repositioned to satisfy the new constraint."
  (setf (gtk-window-window-position window) position))

(export 'gtk-window-set-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_transient_for ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-transient-for))

(defun gtk-window-set-transient-for (window parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[parent]{parent window, or @code{nil}}
  @begin{short}
    Dialog windows should be set transient for the main application window they
    were spawned from. This allows window managers to e. g. keep the dialog on
    top of the main window, or center the dialog over the main window.
  @end{short}
  @fun{gtk-dialog-new-with-buttons} and other convenience functions in GTK+ will
  sometimes call @sym{gtk-window-set-transient-for} on your behalf.

  Passing @code{nil} for parent unsets the current transient window.

  On Windows, this function puts the child window on top of the parent, much
  as the window manager would have done on X."
  (setf (gtk-window-transient-for window) parent))

(export 'gtk-window-set-transient-for)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_attached_to ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-attached-to))

(defun gtk-window-set-attached-to (window attach-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[attach-widget]{a @class{gtk-widget}, or @code{nil}}
  @begin{short}
    Marks @arg{window} as attached to @arg{attach-widget}.
  @end{short}
  This creates a logical binding between the @arg{window} and the widget it
  belongs to, which is used by GTK+ to propagate information such as styling or
  accessibility to window as if it was a children of @arg{attach-widget}.

  Examples of places where specifying this relation is useful are for instance
  a @class{gtk-menu} created by a @class{gtk-combo-box}, a completion popup
  window created by @class{gtk-entry} or a typeahead search entry created by
  @class{gtk-tree-view}.

  Note that this function should not be confused with
  @fun{gtk-window-set-transient-for}, which specifies a window manager relation
  between two toplevels instead.

  Passing @code{nil} for @arg{attach-widget} detaches the window.

  Since 3.4
  @see-function{gtk-window-set-transient-for}"
  (setf (gtk-window-attached-to window) attach-widget))

(export 'gtk-window-set-attached-to)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_destroy_with_parent ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-destroy-with-parent))

(defun gtk-window-set-destroy-with-parent (window setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[setting]{whether to destroy @arg{window} with its transient parent}
  @begin{short}
    If @arg{setting} is @arg{true}, then destroying the transient parent of
    @arg{window} will also destroy @arg{window} itself.
  @end{short}
  This is useful for dialogs that shouldn't persist beyond the lifetime of the
  main window they're associated with, for example."
  (setf (gtk-window-destroy-with-parent window) setting))

(export 'gtk-window-set-destroy-with-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_hide_titlebar_when_maximized ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-hide-titlebar-when-maximized))

(defun gtk-window-set-hide-titlebar-when-maximized (window setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[setting]{whether to hide the titlebar when @arg{window} is
    maximized}
  @begin{short}
    If @arg{setting} is @em{true}, then @arg{window} will request that it's
    titlebar should be hidden when maximized.
  @end{short}
  This is useful for windows that do not convey any information other than the
  application name in the titlebar, to put the available screen space to better
  use. If the underlying window system does not support the request, the setting
  will not have any effect.

  Since 3.4"
  (setf (gkt-window-hide-titlebar-when-maximized window) setting))

(export 'gtk-window-set-hide-titlebar-when-maximized)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-screen))

(defun gtk-window-set-screen (window screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Sets the @class{gdk-screen} where the @arg{window} is displayed; if the
    @arg{window} is already mapped, it will be unmapped, and then remapped on
    the new @arg{screen}.
  @end{short}

  Since 2.2"
  (setf (gtk-window-screen window) screen))

(export 'gtk-window-set-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-screen))

(defun gtk-window-get-screen (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{A @class{gdk-screen} object.}
  @begin{short}
    Returns the @class{gdk-screen} associated with @arg{window}.
  @end{short}

  Since 2.2"
  (gtk-window-screen window))

(export 'gtk-window-get-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_is_active ()
;;;
;;; gboolean gtk_window_is_active (GtkWindow *window);
;;;
;;; Returns whether the window is part of the current active toplevel. (That is,
;;; the toplevel window receiving keystrokes.) The return value is TRUE if the
;;; window is active toplevel itself, but also if it is, say, a GtkPlug embedded
;;; in the active toplevel. You might use this function if you wanted to draw a
;;; widget differently in an active window from a widget in an inactive window.
;;; See gtk_window_has_toplevel_focus()
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if the window part of the current active window
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; Implemented as the accessor of the slot "is-active".

;;; ----------------------------------------------------------------------------
;;; gtk_window_has_toplevel_focus ()
;;;
;;; gboolean gtk_window_has_toplevel_focus (GtkWindow *window);
;;;
;;; Returns whether the input focus is within this GtkWindow. For real toplevel
;;; windows, this is identical to gtk_window_is_active(), but for embedded
;;; windows, like GtkPlug, the results will differ.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if the input focus is within this GtkWindow
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; Implemented as the accessor of the slot "has-toplevel-focus".

;;; ----------------------------------------------------------------------------
;;; gtk_window_list_toplevels ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_list_toplevels" gtk-window-list-toplevels)
    (g-list (g-object gtk-window) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @return{List of toplevel widgets.}
  @begin{short}
    Returns a list of all existing toplevel windows.
  @end{short}
  The widgets in the list are not individually referenced. If you want to
  iterate through the list and perform actions involving callbacks that might
  destroy the widgets, you must call
  @code{g_list_foreach (result, (GFunc)g_object_ref, NULL)}
  first, and then unref all the widgets afterwards.")

(export 'gtk-window-list-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_add_mnemonic" gtk-window-add-mnemonic) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[keyval]{the mnemonic}
  @argument[target]{the widget that gets activated by the mnemonic}
  @begin{short}
    Adds a mnemonic to this @arg{window}.
  @end{short}"
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object gtk-widget)))

(export 'gtk-window-add-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_remove_mnemonic" gtk-window-remove-mnemonic) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[keyval]{the mnemonic}
  @argument[target]{the widget that gets activated by the mnemonic}
  @begin{short}
    Removes a mnemonic from this @arg{window}.
  @end{short}"
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object gtk-widget)))

(export 'gtk-window-remove-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_mnemonic_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_mnemonic_activate" gtk-window-mnemonic-activate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[keyval]{the mnemonic}
  @argument[modifier]{the modifiers}
  @return{@em{True} if the activation is done.}
  @begin{short}
    Activates the targets associated with the mnemonic.
  @end{short}"
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

  Since 2.4"
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
    container chain until a widget handles event. This is normally called by the
    default @code{key_press_event} and @code{key_release_event} handlers for
    toplevel windows, however in some cases it may be useful to call this
    directly when overriding the standard key handling for a toplevel window.
  @end{short}

  Since 2.4"
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
  convenient to use @fun{gtk-widget-grab-focus} instead of this function.
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
    Returns the default widget for @arg{window}. See
    @fun{gtk-window-set-default} for more details.
  @end{short}

  Since 2.14
  @see-function{gtk-window-set-default}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-default-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default" gtk-window-set-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[default-widget]{widget to be the default, or @code{nil} to unset the
    default widget for the toplevel}
  @begin{short}
    The default widget is the widget that is activated when the user presses
    Enter in a dialog for example. This function sets or unsets the default
    widget for a @class{gtk-window} about.
  @end{short}
  When setting (rather than unsetting) the default widget it is generally easier
  to call @fun{gtk-widget-grab-focus} on the widget. Before making a widget the
  default widget, you must call @fun{gtk-widget-set-can-default} on the widget
  you would like to make the default.
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
    Presents a window to the user. This may mean raising the window in the
    stacking order, deiconifying it, moving it to the current desktop, and/or
    giving it the keyboard focus, possibly dependent on the user's platform,
    window manager, and preferences.
  @end{short}

  If window is hidden, this function calls @fun{gtk-widget-show} as well.

  This function should be used when the user tries to open a window that is
  already open. Say for example the preferences dialog is currently open, and
  the user chooses Preferences from the menu a second time; use
  @sym{gtk-window-present} to move the already open dialog where the user can
  see it.

  If you are calling this function in response to a user interaction, it is
  preferable to use @fun{gtk-window-present-with-time}.
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
  @argument[timestamp]{the timestamp of the user interaction (typically a button
    or key press event) which triggered this call}
  @begin{short}
    Presents a window to the user in response to a user interaction. If you need
    to present a window without a timestamp, use @fun{gtk-window-present}. See
    @fun{gtk-window-present} for details.
  @end{short}

  Since 2.8
  @see-function{gtk-window-present}"
  (window (g-object gtk-window))
  (timestamp :uint32))

(export 'gtk-window-present-with-time)

;;; ----------------------------------------------------------------------------
;;; gtk_window_iconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_iconify" gtk-window-iconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to iconify (i. e. minimize) the specified window. Note that you
    should not assume the window is definitely iconified afterward, because
    other entities (e. g. the user or window manager) could deiconify it again,
    or there may not be a window manager in which case iconification is not
    possible, etc. But normally the window will end up iconified. Just do not
    write code that crashes if not.
  @end{short}

  It is permitted to call this function before showing a window, in which case
  the window will be iconified before it ever appears onscreen.

  You can track iconification via the \"window-state-event\" signal on
  @class{gtk-widget}."
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
    Asks to deiconify (i. e. unminimize) the specified @arg{window}. Note that
    you should not assume the @arg{window} is definitely deiconified afterward,
    because other entities (e. g. the user or window manager) could iconify it
    again before your code which assumes deiconification gets to run.
  @end{short}

  You can track iconification via the \"window-state-event\" signal on
  @class{gtk-widget}."
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
    Asks to stick @arg{window}, which means that it will appear on all user
    desktops. Note that you should not assume the @arg{window} is definitely
    stuck afterward, because other entities (e. g. the user or window manager)
    could unstick it again, and some window managers do not support sticking
    windows. But normally the window will end up stuck. Just do not write code
    that crashes if not.
  @end{short}

  It's permitted to call this function before showing a window.

  You can track stickiness via the \"window-state-event\" signal on
  @class{gtk-widget}."
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
    Asks to unstick @arg{window}, which means that it will appear on only one of
    the user's desktops. Note that you should not assume the window is
    definitely unstuck afterward, because other entities (e. g. the user or
    window manager) could stick it again. But normally the window will end up
    stuck. Just do not write code that crashes if not.
  @end{short}

  You can track stickiness via the \"window-state-event\" signal on
  @class{gtk-widget}."
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
    Asks to maximize @arg{window}, so that it becomes full screen. Note that you
    should not assume the @arg{window} is definitely maximized afterward,
    because other entities (e. g. the user or window manager) could unmaximize
    it again, and not all window managers support maximization. But normally the
    window will end up maximized. Just don't write code that crashes if not.
  @end{short}

  It is permitted to call this function before showing a window, in which case
  the @arg{window} will be maximized when it appears onscreen initially.

  You can track maximization via the \"window-state-event\" signal on
  @class{gtk-widget}."
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
    Asks to unmaximize @arg{window}. Note that you should not assume the
    @arg{window} is definitely unmaximized afterward, because other entities
    (e. g. the user or window manager) could maximize it again, and not all
    window managers honor requests to unmaximize. But normally the @arg{window}
    will end up unmaximized. Just don't write code that crashes if not.
  @end{short}

  You can track maximization via the \"window-state-event\" signal on
  @class{gtk-widget}."
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
    Asks to place @arg{window} in the fullscreen state. Note that you should not
    assume the @arg{window} is definitely full screen afterward, because other
    entities (e. g. the user or window manager) could unfullscreen it again, and
    not all window managers honor requests to fullscreen windows. But normally
    the window will end up fullscreen. Just do not write code that crashes if
    not.
  @end{short}

  You can track the fullscreen state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Since 2.2"
  (window (g-object gtk-window)))

(export 'gtk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unfullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unfullscreen" gtk-window-unfullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{short}
    Asks to toggle off the fullscreen state for @arg{window}.
  @end{short}
  Note that you should not assume the @arg{window} is definitely not full screen
  afterward, because other entities (e. g. the user or window manager) could
  fullscreen it again, and not all window managers honor requests to
  unfullscreen windows. But normally the window will end up restored to its
  normal state. Just do not write code that crashes if not.

  You can track the fullscreen state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Since 2.2"
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

  Since 2.4"
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

  Since 2.4"
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
  @argument[button]{mouse button that initiated the drag}
  @argument[edge]{position of the resize control}
  @argument[root-x]{X position where the user clicked to initiate the drag,
    in root window coordinates}
  @argument[root-y]{Y position where the user clicked to initiate the drag}
  @argument[timestamp]{timestamp from the click event that initiated the drag}
  Starts resizing a window. This function is used if an application has window
  resizing controls. When GDK can support it, the resize will be done using
  the standard mechanism for the window manager or windowing system.
  Otherwise, GDK will try to emulate window resizing, potentially not all that
  well, depending on the windowing system."
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
  Starts moving a window. This function is used if an application has window
  movement grips. When GDK can support it, the window movement will be done
  using the standard mechanism for the window manager or windowing system.
  Otherwise, GDK will try to emulate window movement, potentially not all that
  well, depending on the windowing system."
  (window (g-object gtk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_decorated ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-decorated))

(defun gtk-window-set-decorated (window setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[setting]{@em{true} to decorate the window}
  @begin{short}
    By default, windows are decorated with a title bar, resize controls, etc.
    Some window managers allow GTK+ to disable these decorations, creating a
    borderless window. If you set the decorated property to @code{nil} using
    this function, GTK+ will do its best to convince the window manager not to
    decorate the window. Depending on the system, this function may not have any
    effect when called on a window that is already visible, so you should call
    it before calling @fun{gtk-widget-show}.
  @end{short}

  On Windows, this function always works, since there is no window manager
  policy involved."
  (setf (gtk-window-decorated window) setting))

(export 'gtk-window-set-decorated)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_deletable ()
;;;
;;; void gtk_window_set_deletable (GtkWindow *window, gboolean setting);
;;;
;;; By default, windows have a close button in the window frame. Some window
;;; managers allow GTK+ to disable this button. If you set the deletable
;;; property to FALSE using this function, GTK+ will do its best to convince the
;;; window manager not to show a close button. Depending on the system, this
;;; function may not have any effect when called on a window that is already
;;; visible, so you should call it before calling gtk_window_show().
;;;
;;; On Windows, this function always works, since there's no window manager
;;; policy involved.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     TRUE to decorate the window as deletable
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

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
  @short{Sets the mnemonic modifier for this @arg{window}.}"
  (window (g-object gtk-window))
  (modifier gdk-modifier-type))

(export 'gtk-window-set-mnemonic-modifier)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_type_hint ()
;;;
;;; void gtk_window_set_type_hint (GtkWindow *window, GdkWindowTypeHint hint);
;;;
;;; By setting the type hint for the window, you allow the window manager to
;;; decorate and handle the window in a way which is suitable to the function of
;;; the window in your application.
;;;
;;; This function should be called before the window becomes visible.
;;;
;;; gtk_dialog_new_with_buttons() and other convenience functions in GTK+ will
;;; sometimes call gtk_window_set_type_hint() on your behalf.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; hint :
;;;     the window type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_skip_taskbar_hint ()
;;;
;;; void gtk_window_set_skip_taskbar_hint (GtkWindow *window, gboolean setting)
;;;
;;; Windows may set a hint asking the desktop environment not to display the
;;; window in the task bar. This function sets this hint.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     TRUE to keep this window from appearing in the task bar
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_skip_pager_hint ()
;;;
;;; void gtk_window_set_skip_pager_hint (GtkWindow *window, gboolean setting);
;;;
;;; Windows may set a hint asking the desktop environment not to display the
;;; window in the pager. This function sets this hint. (A "pager" is any desktop
;;; navigation tool such as a workspace switcher that displays a thumbnail
;;; representation of the windows on the screen.)
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     TRUE to keep this window from appearing in the pager
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_urgency_hint ()
;;;
;;; void gtk_window_set_urgency_hint (GtkWindow *window, gboolean setting);
;;;
;;; Windows may set a hint asking the desktop environment to draw the users
;;; attention to the window. This function sets this hint.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     TRUE to mark this window as urgent
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_accept_focus ()
;;;
;;; void gtk_window_set_accept_focus (GtkWindow *window, gboolean setting);
;;;
;;; Windows may set a hint asking the desktop environment not to receive the
;;; input focus. This function sets this hint.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     TRUE to let this window receive input focus
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_focus_on_map ()
;;;
;;; void gtk_window_set_focus_on_map (GtkWindow *window, gboolean setting);
;;;
;;; Windows may set a hint asking the desktop environment not to receive the
;;; input focus when the window is mapped. This function sets this hint.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     TRUE to let this window receive input focus on map
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_startup_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-startup-id))

(defun gtk-window-set-startup-id (window startup-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[startup-id]{a string with startup notification identifier}
  @begin{short}
    Startup notification identifiers are used by desktop environment to track
    application startup, to provide user feedback and other features.
  @end{short}
  This function changes the corresponding property on the underlying
  @class{gdk-window}. Normally, startup identifier is managed automatically and
  you should only use this function in special cases like transferring focus
  from other processes. You should use this function before calling
  @fun{gtk-window-present} or any equivalent function generating a window map
  event.

  This function is only useful on X11, not with other GTK+ targets.

  Since 2.12"
  (setf (gtk-window-startup-id window) startup-id))

(export 'gtk-window-set-startup-id)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_role ()
;;;
;;; void gtk_window_set_role (GtkWindow *window, const gchar *role);
;;;
;;; This function is only useful on X11, not with other GTK+ targets.
;;;
;;; In combination with the window title, the window role allows a window
;;; manager to identify "the same" window when an application is restarted. So
;;; for example you might set the "toolbox" role on your app's toolbox window,
;;; so that when the user restarts their session, the window manager can put the
;;; toolbox back in the same place.
;;;
;;; If a window already has a unique title, you don't need to set the role,
;;; since the WM can use the title to identify the window when restoring the
;;; session.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; role :
;;;     unique identifier for the window to be used when restoring a session
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_decorated ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-decorated))

(defun gtk-window-get-decorated (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if the @arg{window} has been set to have decorations.}
  Returns whether the @arg{window} has been set to have decorations such as a
  title bar via @fun{gtk-window-set-decorated}.
  @see-function{gtk-window-set-decorated}"
  (gtk-window-decorated window))

(export 'gtk-window-get-decorated)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_deletable ()
;;;
;;; gboolean gtk_window_get_deletable (GtkWindow *window);
;;;
;;; Returns whether the window has been set to have a close button via
;;; gtk_window_set_deletable().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if the window has been set to have a close button
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_list" gtk-window-get-default-icon-list)
    (g-list (g-object gdk-pixbuf))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @return{Copy of default icon list.}
  Gets the value set by @fun{gtk-window-set-default-icon-list}. The list is a
  copy and should be freed with g_list_free(), but the pixbufs in the list have
  not had their reference count incremented.")

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
    Returns the fallback icon name for windows that has been set with
    @fun{gtk-window-set-default-icon-name}. The returned string is owned by
    GTK+ and should not be modified. It is only valid until the next call to
    @fun{gtk-window-set-default-icon-name}.
  @end{short}

  Since 2.16")

(export 'gtk-window-get-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-default-size))

(defun gtk-window-get-default-size (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{@code{width} -- the default width, or @code{nil}@br{}
          @code{height} -- the default height, or @code{nil}}
  Gets the default size of the @arg{window}. A value of -1 for the width or
  height indicates that a default size has not been explicitly set for that
  dimension, so the \"natural\" size of the window will be used."
  (values (gtk-window-default-width window)
          (gtk-window-default-height window)))

(export 'gtk-window-get-default-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_destroy_with_parent ()
;;;
;;; gboolean gtk_window_get_destroy_with_parent (GtkWindow *window);
;;;
;;; Returns whether the window will be destroyed with its transient parent.
;;; See gtk_window_set_destroy_with_parent().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if the window will be destroyed with its transient parent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_hide_titlebar_when_maximized ()
;;;
;;; gboolean gtk_window_get_hide_titlebar_when_maximized (GtkWindow *window);
;;;
;;; Returns whether the window has requested to have its titlebar hidden when
;;; maximized. See gtk_window_set_hide_titlebar_when_maximized().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if the window has requested to have its titlebar hidden when
;;;     maximized
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-icon))

(defun gtk-window-get-icon (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[window]{a @class{gtk-window} widget}
  @return{Icon for @arg{window}.}
  Gets the value set by the function @fun{gtk-window-set-icon} or if you have
  called the function @fun{gtk-window-set-icon-list}, gets the first icon in
  the icon list.
  @see-function{gtk-window-set-icon}
  @see-function{gtk-window-set-icon-list}"
  (gtk-window-icon window))

(export 'gtk-window-get-icon)

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
    Retrieves the list of icons set by @fun{gtk-window-set-icon-list}.
  @end{short}
  The list is copied, but the reference count on each member won't be
  incremented.
  @see-function{gtk-window-set-icon-list}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-icon-name))

(defun gtk-window-get-icon-name (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[window]{a @class{gtk-window} widget}
  @return{The icon name or @code{nil} if the window has no themed icon.}
  @begin{short}
    Returns the name of the themed icon for the @arg{window}, see
    @fun{gtk-window-set-icon-name}.
  @end{short}

  Since 2.6
  @see-function{gtk-window-set-icon-name}"
  (gtk-window-icon-name window))

(export 'gtk-window-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_mnemonic_modifier ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_mnemonic_modifier" gtk-window-get-mnemnic-modifier)
    gdk-modifier-type
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The modifier mask used to activate mnemonics on this @arg{window}.}
  @begin{short}
    Returns the mnemonic modifier for this @arg{window}.
  @end{short}
  See @fun{gtk-window-set-mnemonic-modifier}.
  @see-function{gtk-window-set-mnemonic-modifier}"
  (window (g-object gtk-window)))

(export 'gtk-window-get-mnemonic-modifier)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_modal ()
;;;
;;; gboolean gtk_window_get_modal (GtkWindow *window);
;;;
;;; Returns whether the window is modal. See gtk_window_set_modal().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if the window is set to be modal and establishes a grab when shown
;;; ----------------------------------------------------------------------------

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
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{return}
    @code{root-x} -- X coordinate of gravity-determined reference point
    or @code{nil}@br{}
    @code{root-y} -- Y coordinate of gravity-determined reference point,
    or @code{nil}
  @end{return}
  @begin{short}
    This function returns the position you need to pass to
    @fun{gtk-window-move} to keep window in its current position. This means
    that the meaning of the returned value varies with window gravity. See
    @fun{gtk-window-move} for more details.
  @end{short}

  If you haven't changed the window gravity, its gravity will be
  @code{:north-west}. This means that @sym{gtk-window-get-position} gets the
  position of the top-left corner of the window manager frame for the window.
  @fun{gtk-window-move} sets the position of this same top-left corner.

  @sym{gtk-window-get-position} is not 100% reliable because the X Window System
  does not specify a way to obtain the geometry of the decorations placed on a
  window by the window manager. Thus GTK+ is using a \"best guess\" that works
  with most window managers.

  Moreover, nearly all window managers are historically broken with respect to
  their handling of window gravity. So moving a window to its current position
  as returned by @sym{gtk-window-get-position} tends to result in moving the
  window slightly. Window managers are slowly getting better over time.

  If a window has gravity @code{:static} the window manager frame is not
  relevant, and thus @sym{gtk-window-get-position} will always produce accurate
  results. However you can not use static gravity to do things like place a
  window in a corner of the screen, because static gravity ignores the window
  manager decorations.

  If you are saving and restoring your application's window positions, you
  should know that it is impossible for applications to do this without getting
  it somewhat wrong because applications do not have sufficient knowledge of
  window manager state. The Correct Mechanism is to support the session
  management protocol (see the \"GnomeClient\" object in the GNOME libraries for
  example) and allow the window manager to save your window sizes and
  positions."
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-window-get-position window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-window-get-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_role ()
;;;
;;; const gchar * gtk_window_get_role (GtkWindow *window);
;;;
;;; Returns the role of the window. See gtk_window_set_role() for further
;;; explanation.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     the role of the window if set, or NULL. The returned is owned by the
;;;     widget and must not be modified or freed.
;;; ----------------------------------------------------------------------------

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
    @code{width} -- width, or @code{nil}@br{}
    @code{height} -- height, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the current size of window. If window is not onscreen, it returns
    the size GTK+ will suggest to the window manager for the initial window size
    (but this is not reliably the same as the size the window manager will
    actually select). The size obtained by @sym{gtk-window-get-size} is the last
    size received in a @class{gdk-event-configure}, that is, GTK+ uses its
    locally stored size, rather than querying the X server for the size. As a
    result, if you call @fun{gtk-window-resize} then immediately call
    @sym{gtk-window-get-size}, the size won't have taken effect yet. After the
    window manager processes the resize request, GTK+ receives notification that
    the size has changed via a configure event, and the size of the window gets
    updated.
  @end{short}

  Note 1: Nearly any use of this function creates a race condition, because
  the size of the window may change between the time that you get the size and
  the time that you perform some action assuming that size is the current
  size. To avoid race conditions, connect to \"configure-event\" on the window
  and adjust your size dependent state to match the size delivered in the
  @class{gdk-event-configure}.

  Note 2: The returned size does not include the size of the window manager
  decorations (aka the window frame or border). Those are not drawn by GTK+
  and GTK+ has no reliable method of determining their size.

  Note 3: If you are getting a window size in order to position the window
  onscreen, there may be a better way. The preferred way is to simply set the
  window's semantic type with @fun{gtk-window-set-type-hint}, which allows the
  window manager to e. g. center dialogs. Also, if you set the transient parent
  of dialogs with @fun{gtk-window-set-transient-for} window managers will often
  center the dialog over its parent window. It is much preferred to let the
  window manager handle these things rather than doing it yourself, because
  all apps will behave consistently and according to user prefs if the window
  manager handles it. Also, the window manager can take the size of the window
  decorations/border into account, while your application cannot.

  In any case, if you insist on application-specified window positioning,
  there is still a better way than doing it yourself -
  @sym{gtk-window-set-position} will frequently handle the details for you."
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-window-get-size window width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(export 'gtk-window-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-title))

(defun gtk-window-get-title (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @begin{return}
    The title of the @arg{window}, or @code{nil} if none has been set
    explicitely. The returned string is owned by the widget and must not be
    modified or freed.
  @end{return}
  Retrieves the title of the window. See @fun{gtk-window-set-title}.
  @see-function{gtk-window-set-title}"
  (gtk-window-title window))

(export 'gtk-window-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_transient_for ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-transient-for))

(defun gtk-window-get-transient-for (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The transient parent for this @arg{window}, or @code{nil} if no
    transient parent has been set}
  @begin{short}
    Fetches the transient parent for this window.
  @end{short}
  See @fun{gtk-window-set-transient-for}."
  (gtk-window-transient-for window))

(export 'gtk-window-get-transient-for)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_attached_to ()
;;;
;;; GtkWidget * gtk_window_get_attached_to (GtkWindow *window);
;;;
;;; Fetches the attach widget for this window. See gtk_window_set_attached_to().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     the widget where the window is attached, or NULL if the window is not
;;;     attached to any widget
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_type_hint ()
;;;
;;; GdkWindowTypeHint gtk_window_get_type_hint (GtkWindow *window);
;;;
;;; Gets the type hint for this window. See gtk_window_set_type_hint().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     the type hint for window
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_skip_taskbar_hint ()
;;;
;;; gboolean gtk_window_get_skip_taskbar_hint (GtkWindow *window);
;;;
;;; Gets the value set by gtk_window_set_skip_taskbar_hint()
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if window shouldn't be in taskbar
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_skip_pager_hint ()
;;;
;;; gboolean gtk_window_get_skip_pager_hint (GtkWindow *window);
;;;
;;; Gets the value set by gtk_window_set_skip_pager_hint().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if window shouldn't be in pager
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_urgency_hint ()
;;;
;;; gboolean gtk_window_get_urgency_hint (GtkWindow *window);
;;;
;;; Gets the value set by gtk_window_set_urgency_hint()
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if window is urgent
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_accept_focus ()
;;;
;;; gboolean gtk_window_get_accept_focus (GtkWindow *window);
;;;
;;; Gets the value set by gtk_window_set_accept_focus().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if window should receive the input focus
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus_on_map ()
;;;
;;; gboolean gtk_window_get_focus_on_map (GtkWindow *window);
;;;
;;; Gets the value set by gtk_window_set_focus_on_map().
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if window should receive the input focus when mapped
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

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

  Since 2.10"
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

  Since 2.22"
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
  @short{Gets the type of the window. See @symbol{gtk-window-type}.}

  Since 2.20"
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
    Asks the window manager to move window to the given position. Window
    managers are free to ignore this; most window managers ignore requests for
    initial window positions (instead using a user-defined placement algorithm)
    and honor requests after the window has already been shown.
  @end{short}

  Note: the position is the position of the gravity-determined reference point
  for the window. The gravity determines two things: first, the location of
  the reference point in root window coordinates; and second, which point on
  the window is positioned at the reference point.

  By default the gravity is @code{:north-west}, so the reference point is
  simply the x, y supplied to @sym{gtk-window-move}. The top-left corner of the
  window decorations (aka window frame or border) will be placed at x, y.
  Therefore, to position a window at the top left of the screen, you want to
  use the default gravity (which is @code{:north-west}) and move the
  window to 0,0.

  To position a window at the bottom right corner of the screen, you would set
  @code{:south-east}, which means that the reference point is at x + the
  window width and y + the window height, and the bottom-right corner of the
  window border will be placed at that reference point. So, to place a window
  in the bottom right corner you would first set gravity to south east, then
  write: @code{(gtk-window-move window (- (gdk-screen-width) window-width)
  (- (gdk-screen-height) window-height))} (note that this example does not take
  multi-head scenarios into account).

  The Extended Window Manager Hints specification at
  http://www.freedesktop.org/Standards/wm-spec has a nice table of gravities
  in the \"implementation notes\" section.

  The @fun{gtk-window-get-position} documentation may also be relevant."
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
    X (type 'man X') for details on this. @sym{gtk-window-parse-geometry} does
    work on all GTK+ ports including Win32 but is primarily intended for an X
    environment.
  @end{short}

  If either a size or a position can be extracted from the geometry string,
  @sym{gtk-window-parse-geometry} returns @em{true} and calls
  @fun{gtk-window-set-default-size} and/or @fun{gtk-window-move} to resize/move
  the window.

  If @sym{gtk-window-parse-geometry} returns @em{true}, it will also set the
  @code{:user-pos} and/or @code{:user-size} hints indicating to the window
  manager that the size/position of the window was user specified. This causes
  most window managers to honor the geometry.

  Note that for @sym{gtk-window-parse-geometry} to work as expected, it has to
  be called when the window has its \"final\" size, i. e. after calling
  @fun{gtk-widget-show-all} on the contents and
  @fun{gtk-window-set-geometry-hints} on the window.
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
  @end{pre}"
  (window (g-object gtk-window))
  (geometry :string))

(export 'gtk-window-parse-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_reshow_with_initial_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_reshow_with_initial_size"
          gtk-window-reshow-with-initial-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  Hides @arg{window}, then reshows it, resetting the default size and position
  of the @arg{window}. Used by GUI builders only."
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
    The default geometry constraint is that windows may not be smaller than
    their size request; to override this constraint, call
    @fun{gtk-widget-set-size-request} to set the window's request to a smaller
    value.
  @end{short}

  If @sym{gtk-window-resize} is called before showing a window for the first
  time, it overrides any default size set with
  @fun{gtk-window-set-default-size}.

  Windows may not be resized smaller than 1 by 1 pixels."
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_to_geometry ()
;;;
;;; void gtk_window_resize_to_geometry (GtkWindow *window,
;;;                                     gint width,
;;;                                     gint height);
;;;
;;; Like gtk_window_resize(), but width and height are interpreted in terms of
;;; the base size and increment set with gtk_window_set_geometry_hints.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; width :
;;;     width in resize increments to resize the window to
;;;
;;; height :
;;;     height in resize increments to resize the window to
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

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
    function @fun{gtk-window-set-icon-list} called on them to set up a window
    specific icon list. This function allows you to set up the icon for all
    windows in your app at once.
  @end{short}

  See @fun{gtk-window-set-icon-list} for more details.
  @see-function{gtk-window-set-icon-list}"
  ;; We have to pass a list of pointers.
  (%gtk-window-set-default-icon-list (mapcar #'pointer icon-list)))

(export 'gtk-window-set-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon" gtk-window-set-default-icon) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[icon]{the icon}
  @begin{short}
    Sets an icon to be used as fallback for windows that have not had
    @fun{gtk-window-set-icon} called on them from a pixbuf.
  @end{short}

  Since 2.4"
  (icon (g-object gdk-pixbuf)))

(export 'gtk-window-set-default-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_from_file ()
;;;
;;; gboolean gtk_window_set_default_icon_from_file (const gchar *filename,
;;;                                                 GError **err);
;;;
;;; Sets an icon to be used as fallback for windows that haven't had
;;; gtk_window_set_icon_list() called on them from a file on disk. Warns on
;;; failure if err is NULL.
;;;
;;; filename :
;;;     location of icon file
;;;
;;; err :
;;;     location to store error, or NULL
;;;
;;; Returns :
;;;     TRUE if setting the icon succeeded.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_name" gtk-window-set-default-icon-name)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[name]{the name of the themed icon}
  @begin{short}
    Sets an icon to be used as fallback for windows that haven't had
    @fun{gtk-window-set-icon-list} called on them from a named themed icon, see
    @fun{gtk-window-set-icon-name}.
  @end{short}

  Since 2.6"
  (name :string))

(export 'gtk-window-set-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-icon))

(defun gtk-window-set-icon (window icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[window]{a @class{gtk-window} widget}
  @argument[icon]{a @class{gdk-pixbuf} icon image, or @code{nil}}
  @begin{short}
    Sets up the icon representing a @class{gtk-window} widget. This icon is used
    when the window is minimized (also known as iconified). Some window managers
    or desktop environments may also place it in the window frame, or display it
    in other contexts.
  @end{short}

  The icon should be provided in whatever size it was naturally drawn; that
  is, do not scale the image before passing it to GTK+. Scaling is postponed
  until the last minute, when the desired final size is known, to allow best
  quality.

  If you have your icon hand drawn in multiple sizes, use the function
  @fun{gtk-window-set-icon-list}. Then the best size will be used.

  This function is equivalent to calling the function
  @fun{gtk-window-set-icon-list} with a 1-element list.

  See also the function @fun{gtk-window-set-default-icon-list} to set the icon
  for all windows in your application in one go.
  @see-function{gtk-window-set-icon-list}
  @see-function{gtk-window-set-default-icon-list}"
  (setf (gtk-window-icon window) icon))

(export 'gtk-window-set-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_icon_list" gtk-window-set-icon-list) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[list]{list of @class{gdk-pixbuf} objects}
  @begin{short}
    Sets up the icon representing a @class{gtk-window} widget.
  @end{short}
  The icon is used when the @arg{window} is minimized (also known as iconified).
  Some window managers or desktop environments may also place it in the window
  frame, or display it in other contexts.

  @sym{gtk-window-set-icon-list} allows you to pass in the same icon in several
  hand-drawn sizes. The list should contain the natural sizes your icon is
  available in; that is, don't scale the image before passing it to GTK+.
  Scaling is postponed until the last minute, when the desired final size is
  known, to allow best quality.

  By passing several sizes, you may improve the final image quality of the
  icon, by reducing or eliminating automatic image scaling.

  Recommended sizes to provide: 16 x 16, 32 x 32, 48 x 48 at minimum, and larger
  images (64 x 64, 128 x 128) if you have them.

  See also @fun{gtk-window-set-default-icon-list} to set the icon for all
  windows in your application in one go.

  Note that transient windows (those who have been set transient for another
  window using @fun{gtk-window-set-transient-for}) will inherit their icon from
  their transient parent. So there's no need to explicitly set the icon on
  transient windows.
  @see-function{gtk-window-set-default-icon-list}
  @see-function{gtk-window-set-transient-for}"
  (window (g-object gtk-window))
  (g-list (g-object gdk-pixbuf) :free-to-foreign t))

(export 'gtk-window-set-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_from_file ()
;;;
;;; gboolean gtk_window_set_icon_from_file (GtkWindow *window,
;;;                                         const gchar *filename,
;;;                                         GError **err);
;;;
;;; Sets the icon for window. Warns on failure if err is NULL.
;;;
;;; This function is equivalent to calling gtk_window_set_icon() with a pixbuf
;;; created by loading the image from filename.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; filename :
;;;     location of icon file
;;;
;;; err :
;;;     location to store error, or NULL
;;;
;;; Returns :
;;;     TRUE if setting the icon succeeded
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-icon-name))

(defun gtk-window-set-icon-name (window name)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[window]{a @class{gtk-window} widget}
  @argument[name]{the name of the themed icon}
  @begin{short}
    Sets the icon for the @arg{window} from a named themed icon. See the docs
    for @class{gtk-icon-theme} for more details.
  @end{short}

  Note that this has nothing to do with the @code{WM_ICON_NAME} property which
  is mentioned in the ICCCM.

  Since 2.6"
  (setf (gtk-window-icon-name window) name))

(export 'gtk-window-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_auto_startup_notification ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_auto_startup_notification"
          gtk-set-window-auto-startup-notification) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[setting]{@em{true} to automatically do startup notification}
  @begin{short}
    By default, after showing the first @class{gtk-window}, GTK+ calls
    @fun{gdk-notify-startup-complete}. Call this function to disable the
    automatic startup notification. You might do this if your first window is a
    splash screen, and you want to delay notification until after your real main
    window has been shown, for example.
  @end{short}

  In that example, you would disable startup notification temporarily, show
  your splash screen, then re-enable it so that showing the main window would
  automatically result in notification.

  Since 2.2"
  (setting :boolean))

(export 'gtk-set-window-auto-startup-notification)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_opacity ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-opacity))

(defun gtk-window-get-opacity (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @return{The requested opacity for this @arg{window}.}
  @begin{short}
    Fetches the requested opacity for this @arg{window}.
  @end{short}
  See @fun{gtk-window-set-opacity}.

  Since 2.12"
  (gtk-window-opacity window))

(export 'gtk-window-get-opacity)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_opacity ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-opacity))

(defun gtk-window-set-opacity (window opacity)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window]{a @class{gtk-window} widget}
  @argument[opacity]{desired opacity, between 0 and 1}
  @begin{short}
    Request the windowing system to make window partially transparent, with
    @arg{opacity} 0 being fully transparent and 1 fully opaque.
  @end{short}
  (Values of the @arg{opacity} parameter are clamped to the [0,1] range.) On X11
  this has any effect only on X screens with a compositing manager running. See
  @fun{gtk-widget-is-composited}. On Windows it should work always.

  Note that setting a window's opacity after the window has been shown causes
  it to flicker once on Windows.

  Since 2.12"
  (setf (gtk-window-opacity window) opacity))

(export 'gtk-window-set-opacity)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_mnemonics_visible ()
;;;
;;; gboolean gtk_window_get_mnemonics_visible (GtkWindow *window);
;;;
;;; Gets the value of the "mnemonics-visible" property.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if mnemonics are supposed to be visible in this window
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_mnemonics_visible ()
;;;
;;; void gtk_window_set_mnemonics_visible (GtkWindow *window,
;;;                                        gboolean setting);
;;;
;;; Sets the "mnemonics-visible" property.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     the new value
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus_visible ()
;;;
;;; gboolean gtk_window_get_focus_visible (GtkWindow *window);
;;;
;;; Gets the value of the "focus-visible" property.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     TRUE if 'focus rectangles' are supposed to be visible in this window.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_focus_visible ()
;;;
;;; void gtk_window_set_focus_visible (GtkWindow *window, gboolean setting);
;;;
;;; Sets the "focus-visible" property.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     the new value
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_has_resize_grip ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-set-has-resize-grip))

(defun gtk-window-set-has-resize-grip (window value)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[window]{a @class{gtk-window} widget}
  @argument[value]{@em{true} to allow a resize grip for @arg{window}}
  @short{Sets whether @arg{window} has a corner resize grip.}

  Note that the resize grip is only shown if the @arg{window} is actually
  resizable and not maximized. Use @fun{gtk-window-resize-grip-is-visible} to
  find out if the resize grip is currently shown.

  Since 3.0
  @see-function{gtk-window-resize-grip-is-visible}"
  (setf (gtk-window-has-resize-grip window) value))

(export 'gtk-window-set-has-resize-grip)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_has_resize_grip ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-has-resize-grip))

(defun gtk-window-get-has-resize-grip (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if the @arg{window} has a resize grip.}
  @short{Determines whether the @arg{window} may have a resize grip.}

  Since 3.0"
  (gtk-window-has-resize-grip window))

(export 'gtk-window-get-has-resize-grip)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_grip_is_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-resize-grip-is-visible))

(defun gtk-window-resize-grip-is-visible (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[window]{a @class{gtk-window} widget}
  @return{@em{True} if a resize grip exists and is visible.}
  @begin{short}
    Determines whether a resize grip is visible for the specified @arg{window}.
  @end{short}

  Since 3.0"
  (and (gtk-window-has-resize-grip window)
       (gtk-window-resize-grip-visible window)))

(export 'gtk-window-resize-grip-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_resize_grip_area ()
;;;
;;; gboolean gtk_window_get_resize_grip_area (GtkWindow *window,
;;;                                           GdkRectangle *rect);
;;;
;;; If a window has a resize grip, this will retrieve the grip position, width
;;; and height into the specified GdkRectangle.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; rect :
;;;     a pointer to a GdkRectangle which we should store the resize grip area.
;;;
;;; Returns :
;;;     TRUE if the resize grip's area was retrieved
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_application ()
;;;
;;; GtkApplication * gtk_window_get_application (GtkWindow *window);
;;;
;;; Gets the GtkApplication associated with the window (if any).
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; Returns :
;;;     a GtkApplication, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_application ()
;;;
;;; void gtk_window_set_application (GtkWindow *window,
;;;                                  GtkApplication *application);
;;;
;;; Sets or unsets the GtkApplication associated with the window.
;;;
;;; The application will be kept alive for at least as long as the window is
;;; open.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; application :
;;;     a GtkApplication, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

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

;;; --- End of file gtk.window.lisp --------------------------------------------
