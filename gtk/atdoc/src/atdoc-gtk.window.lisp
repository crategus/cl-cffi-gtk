;;; ----------------------------------------------------------------------------
;;; gtk.window.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "accept-focus" 'gtk-window) 't)
 "@version{2012-12-30}
  The @code{accept-focus} property of type @code{gboolean} (Read / Write).@br{}
  Whether the window should receive the input focus.@br{}
  Default value: @arg{true}@br{}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "application" 'gtk-window) 't)
 "@version{2012-12-30}
  The @code{application} property of type @class{gtk-application}
  (Read / Write).@br{}
  The @class{gtk-application} associated with the window.@br{}
  The application will be kept alive for at least as long as it has any
  windows associated with it (see @code{g_application_hold()} for a way to keep
  it alive without windows).@br{}
  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it by setting
  the @code{application} property to @code{nil}.@br{}
  Since 3.0")

(setf (documentation (atdoc:get-slot-from-name "attached-to" 'gtk-window) 't)
 "@version{2012-12-30}
  The @code{attached-to} property of type @class{gtk-widget}
  (Read / Write / Construct).@br{}
  The widget to which this window is attached.
  See @fun{gtk-window-set-attached-to}.@br{}
  Examples of places where specifying this relation is useful are for instance
  a @code{GtkMenu} created by a @code{GtkComboBox}, a completion popup window
  created by @code{GtkEntry} or a typeahead search entry created by
  @code{GtkTreeView}.@br{}
  Since 3.4")

(setf (documentation (atdoc:get-slot-from-name "decorated" 'gtk-window) 't)
 "@version{2012-12-30}
  The @code{decorated} property of type @code{gboolean} (Read / Write).@br{}
  Whether the window should be decorated by the window manager.@br{}
  Default value: @arg{true}@br{}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "default-height" 'gtk-window) 't)
 "@version{2012-12-30}
  The @code{default-height} property of type @code{gint} (Read / Write).@br{}
  The default height of the window, used when initially showing the window.@br{}
  Allowed values: @code{>= G_MAXULONG}@br{}
  Default value: @arg{-1}")

(setf (documentation (atdoc:get-slot-from-name "default-width" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"default-width\"} property of type @code{gint} (Read / Write).@br{}
  The default width of the window, used when initially showing the window.@br{}
  Allowed values: @code{>= G_MAXULONG}@br{}
  Default value: @arg{-1}")

(setf (documentation (atdoc:get-slot-from-name "deletable" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"deletable\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether the window frame should have a close button.@br{}
  Default value: @arg{true}@br{}
  Since 2.10")

(setf (documentation (atdoc:get-slot-from-name "destroy-with-parent" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"destroy-with-parent\"} property of type @code{gboolean}
  (Read / Write).@br{}
  If this window should be destroyed when the parent is destroyed.@br{}
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "focus-on-map" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"focus-on-map\"} property of type @code{gboolean} (Read / Write).
  @br{}
  Whether the window should receive the input focus when mapped.@br{}
  Default value: @arg{true}@br{}
  Since 2.6")

(setf (documentation (atdoc:get-slot-from-name "focus-visible" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"focus-visible\"} property of type @code{gboolean} (Read / Write).
  @br{}
  Whether \"focus rectangles\" are currently visible in this window.@br{}
  This property is maintained by GTK+ based on the @arg{\"gtk-visible-focus\"}
  setting and user input and should not be set by applications.@br{}
  Default value: @arg{true}@br{}
  Since 2.20")

(setf (documentation (atdoc:get-slot-from-name "gravity" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"gravity\"} property of type @code{GdkGravity} (Read / Write).@br{}
  The window gravity of the window. See @fun{gtk-window-move} and
  @code{GdkGravity} for more details about window gravity.@br{}
  Default value: @code{:north-west}@br{}
  Since 2.4")

(setf (documentation (atdoc:get-slot-from-name "has-resize-grip" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"has-resize-grip\"} property of type @code{gboolean} (Read / Write).
  @br{}
  Whether the window has a corner resize grip.@br{}
  Note that the resize grip is only shown if the window is actually resizable
  and not maximized. Use @arg{\"resize-grip-visible\"} to find out if the resize
  grip is currently shown.@br{}
  Default value: @arg{true}@br{}
  Since 3.0")

(setf (documentation (atdoc:get-slot-from-name "has-toplevel-focus" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"has-toplevel-focus\"} property of type @code{gboolean} (Read).@br{}
  Whether the input focus is within this @code{GtkWindow}.@br{}
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "hide-titlebar-when-maximized" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"hide-titlebar-when-maximized\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether the titlebar should be hidden during maximization.@br{}
  Default value: @arg{false}@br{}
  Since 3.4")

(setf (documentation (atdoc:get-slot-from-name "icon" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"icon\"} property of type @code{GdkPixbuf*} (Read / Write).@br{}
  Icon for this window.")

(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"icon-name\"} property of type @code{gchar*} (Read / Write).@br{}
  The @arg{icon-name} property specifies the name of the themed icon to use as
  the window icon. See @code{GtkIconTheme} for more details.@br{}
  Default value: @code{NULL}@br{}
  Since 2.6")

(setf (documentation (atdoc:get-slot-from-name "is-active" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"is-active\"} property of type @code{gboolean} (Read).@br{}
  Whether the toplevel is the current active window.@br{} 
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "mnemonics-visible" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"mnemonics-visible\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether mnemonics are currently visible in this window.@br{}
  This property is maintained by GTK+ based on the @arg{\"gtk-auto-mnemonics\"}
  setting and user input, and should not be set by applications.@br{}
  Default value: @arg{true}@br{}
  Since 2.20")

(setf (documentation (atdoc:get-slot-from-name "modal" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"modal\"} property of type @code{gboolean} (Read / Write).@br{}
  If @arg{true}, the window is modal (other windows are not usable while this
  one is up).@br{}
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "opacity" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"opacity\"} property of type @code{gdouble} (Read / Write).@br{}
  The requested opacity of the window. See @fun{gtk-window-set-opacity} for more
  details about window opacity.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{1}@br{}
  Since 2.12")

(setf (documentation (atdoc:get-slot-from-name "resizable" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"resizable\"} property of type @code{gboolean} (Read / Write).@br{}
  If @arg{true}, users can resize the window.@br{}
  Default value: @arg{true}")

(setf (documentation (atdoc:get-slot-from-name "resize-grip-visible" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"resize-grip-visible\"} property of type @code{gboolean}
  (Read).@br{}
  Whether a corner resize grip is currently shown.@br{}
  Default value: @arg{false}@br{}
  Since 3.0")

(setf (documentation (atdoc:get-slot-from-name "role" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"role\"} property of type @code{gchar*} (Read / Write).@br{}
  Unique identifier for the window to be used when restoring a session.@br{}
  Default value: @code{NULL}")

(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"screen\"} property of type @code{GdkScreen*} (Read / Write).@br{}
  The screen where this window will be displayed.")

(setf (documentation (atdoc:get-slot-from-name "skip-pager-hint" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"skip-pager-hint\"} property of type @code{gboolean}
  (Read / Write).@br{}
  @arg{true} if the window should not be in the pager.@br{}
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "skip-taskbar-hint" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"skip-taskbar-hint\"} property of type @code{gboolean}
  (Read / Write).@br{}
  @arg{true} if the window should not be in the task bar.@br{}
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "startup-id" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"startup-id\"} property of type @code{gchar*} (Write).@br{}
  The @arg{startup-id} is a write-only property for setting window's startup
  notification identifier. See @fun{gtk-window-set-startup-id} for more details.
  @br{}
  Default value: @code{NULL}@br{}
  Since 2.12")

(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"title\"} property of type @code{gchar*} (Read / Write).@br{}
  The title of the window.@br{} 
  Default value: @code{NULL}")

(setf (documentation (atdoc:get-slot-from-name "transient-for" 'gtk-window) 't)
 "@version{2012-12-30}
  The @code{transient-for} property of type @sym{gtk-window}
  (Read / Write / Construct).@br{}
  The transient parent of the window. See @fun{gtk-window-set-transient-for} for
  more details about transient windows.@br{}
  Since 2.10")

(setf (documentation (atdoc:get-slot-from-name "type" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"type\"} property of type @code{GtkWindowType}
  (Read / Write / Construct).@br{}
  The type of the window.@br{}
  Default value: @code{:toplevel}")

(setf (documentation (atdoc:get-slot-from-name "type-hint" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"type-hint\"} property of type @code{GdkWindowTypeHint}
  (Read / Write).@br{}
  Hint to help the desktop environment understand what kind of window this is
  and how to treat it.@br{}
  Default value: @code{:hint-normal}")

(setf (documentation (atdoc:get-slot-from-name "urgency-hint" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"urgency-hint\"} property of type @code{gboolean}
  (Read / Write).@br{}
  @arg{true} if the window should be brought to the user's attention.@br{}
  Default value: @arg{false}")

(setf (documentation (atdoc:get-slot-from-name "window-position" 'gtk-window) 't)
 "@version{2012-12-30}
  The @arg{\"window-position\"} property of type @code{GtkWindowPosition}
  (Read / Write).@br{}
  The initial position of the window.@br{}
  Default value: @code{:none}")

;;; --- gtk-window -------------------------------------------------------------

(setf (documentation 'gtk-window 'type)
 "@version{2012-12-30}
  @begin{short}
    A GtkWindow is a toplevel window which can contain other widgets.
  @end{short}
  Windows normally have decorations that are under the control of the windowing
  system and allow the user to manipulate the window (resize it, move it,
  close it, ...).

  GTK+ also allows windows to have a resize grip (a small area in the lower
  right or left corner) which can be clicked to reszie the window. To control
  whether a window has a resize grip, use gtk_window_set_has_resize_grip().

  @b{GtkWindow as GtkBuildable}

  The GtkWindow implementation of the GtkBuildable interface supports a custom
  <accel-groups> element, which supports any number of <group> elements
  representing the GtkAccelGroup objects you want to add to your window
  (synonymous with gtk_window_add_accel_group()).

  Example 48. A UI definition fragment with accel groups
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
  @begin[Signal Details]{dictionary}
    @b{The \"activate-default\" signal}
    @begin{pre}
 void user_function (GtkWindow *window,
                     gpointer   user_data)      : Action
    @end{pre}
    The ::activate-default signal is a keybinding signal which gets emitted when
    the user activates the default widget of window.
    @begin{table}
      @entry[window]{the window which received the signal}
      @entry[user-data]{user data set when the signal handler was connected}
    @end{table}
    @b{The \"activate-focus\" signal}
    @begin{pre}
 void user_function (GtkWindow *window,
                     gpointer   user_data)      : Action
    @end{pre}
    The ::activate-focus signal is a keybinding signal which gets emitted when
    the user activates the currently focused widget of window.
    @begin{table}
      @entry[window]{the window which received the signal}
      @entry[user-data]{user data set when the signal handler was connected}
    @end{table}
    @b{The \"keys-changed\" signal}
    @begin{pre}
 void user_function (GtkWindow *window,
                     gpointer   user_data)      : Run First
    @end{pre}
    The ::keys-changed signal gets emitted when the set of accelerators or
    mnemonics that are associated with window changes.
    @begin{table}
      @entry[window]{the window which received the signal}
      @entry[user-data]{user data set when the signal handler was connected}
    @end{table}
    @b{The \"set-focus\" signal}
    @begin{pre}
 void user_function (GtkWindow *window,
                     GtkWidget *widget,
                     gpointer   user_data)      : Run Last
    @end{pre}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @b{The \"resize-grip-height\" style property}

    @arg{\"resize-grip-height\"}       @code{gint}   : Read / Write@br{}
    Height of resize grip.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{16}

    @b{The \"resize-grip-width\" style property}

    @arg{\"resize-grip-width\"}        @code{gint}   : Read / Write@br{}
    Width of resize grip.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{16}
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
")

;;; --- gtk-window-new ---------------------------------------------------------

(setf (documentation 'gtk-window-new 'function)
 "@version{2012-12-30}
  @argument[type]{type of window, one of the @symbol{gtk-window-type}
    enumeration}
  @return{A new @class{gtk-window} instance.}
  @begin{short}
    Creates a new @class{gtk-window} instance, which is a toplevel window that
    can contain other widgets.
  @end{short}
  Nearly always, the type of the window should be @code{:toplevel}. If you're
  implementing something like a popup menu from scratch (which is a bad idea,
  just use the @class{gtk-menu} class), you might use @code{:popup}.
  @code{:popup} is not for dialogs, though in some other toolkits dialogs are
  called \"popups\". In GTK+, @code{:popup} means a pop-up menu or pop-up
  tooltip. On X11, popup windows are not controlled by the window manager.

  If you simply want an undecorated window (no window borders), use
  @fun{gtk-window-set-decorated}, don't use @code{:popup}.
  @see-symbol{gtk-window-type}
  @see-function{gtk-window-set-decorated}")

;;; --- gtk-window-set-title ---------------------------------------------------

(setf (documentation 'gtk-window-set-title 'function)
 "@version{2012-12-30}
  @argument[window]{a @class{gtk-window} instance}
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
  @see-function{gtk-window-get-title}")

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

;;; --- gtk-window-set-resizable -----------------------------------------------

(setf (documentation 'gtk-window-set-resizable 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[resizable]{@arg{true} if the user can resize this @arg{window}}
  @begin{short}
    Sets whether the user can resize a window. Windows are user resizable by
    default.
  @end{short}")

;;; --- gtk-window-get-resizable -----------------------------------------------

(setf (documentation 'gtk-window-get-resizable 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @return{@arg{true} if the user can resize the @arg{window}.}
  @begin{short}
    Gets the value set by @fun{gtk-window-set-resizable}.
  @end{short}
  @see-function{gtk-window-set-resizable}")

;;; --- gtk-window-add-accel-group ---------------------------------------------

(setf (documentation 'gtk-window-add-accel-group 'function)
 "@version{2013-1-7}
  @argument[window]{@arg{window} to attach accelerator group to}
  @argument[accel-group]{a @class{gtk-accel-group} instance}
  @begin{short}
    Associate @arg{accel-group} with @arg{window}, such that calling
    @fun{gtk-accel-group-activate} on @arg{window} will activate accelerators
    in @arg{accel-group}.
  @end{short}")

;;; --- gtk-window-remove-accel-group ------------------------------------------

(setf (documentation 'gtk-window-remove-accel-group 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[accel-group]{a @class{gtk-accel-group} instance}
  @begin{short}
    Reverses the effects of @fun{gtk-window-add-accel-group}.
  @end{short}
  @see-function{gtk-window-add-accel-group}")

;;; --- gtk-window-activate-focus ----------------------------------------------

(setf (documentation 'gtk-window-activate-focus 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @return{@em{true} if a widget got activated}
  @begin{short}
    Activates the current focused widget within the window.
  @end{short}")

;;; --- gtk-window-activate-default --------------------------------------------

(setf (documentation 'gtk-window-activate-default 'function)
 "@version{2013-1-8}
  @argument[window]{a @class{gtk-window} instance}
  @return{@em{true} if a widget got activated}
  @begin{short}
    Activates the default widget for the @arg{window}, unless the current
    focused widget has been configured to receive the default action (see
    @fun{gtk-widget-set-receives-default}), in which case the focused widget is
    activated.
  @end{short}
  @see-function{gtk-widget-set-receives-default}")

;;; --- gtk-window-set-modal ---------------------------------------------------

(setf (documentation 'gtk-window-set-modal 'function)
 "@version{2013-1-8}
  @argument[window]{a @class{gtk-window} instance}
  @argument[modal]{whether the @arg{window} is modal}

  Sets a window modal or non-modal. Modal windows prevent interaction with
  other windows in the same application. To keep modal dialogs on top of main
  application windows, use @fun{gtk-window-set-transient-for} to make the dialog
  transient for the parent; most window managers will then disallow lowering 
  the dialog below the parent.")

;;; --- gtk-window-set-default-size --------------------------------------------

(setf (documentation 'gtk-window-set-default-size 'function)
 "@version{2012-12-30}
  @argument[window]{a @class{gtk-window} instance}
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

  Windows can't actually be 0x0 in size, they must be at least 1x1, but
  passing 0 for width and height is OK, resulting in a 1x1 default size.
  @see-function{gtk-window-default-width}
  @see-function{gtk-window-default-height}")


;;; --- gtk-window-set-default-geometry ----------------------------------------

(setf (documentation 'gtk-window-set-default-geometry 'function)
 "@version{2013-1-09}
  @argument[window]{a @class{gtk-window} instance}
  @argument[width]{width in resize increments, or -1 to unset the default width}
  @argument[height]{height in resize increments, or -1 to unset the default
    height}
  @begin{short}
    Like @fun{gtk-window-set-default-size}, but @arg{width} and @arg{height} are
    interpreted in terms of the base size and increment set
    with @fun{gtk-window-set-geometry-hints}.
  @end{short}

  Since 3.0
  @see-function{gtk-window-set-geometry-hints}
  @see-function{gtk-window-set-default-size}")

;;; --- gtk-window-set-geometry-hints ------------------------------------------

(setf (documentation 'gtk-window-set-geometry-hints 'function)
 "@version{2013-1-9}
  @argument[window]{a @class{gtk-window} instance}
  @argument[geometry-widget]{widget the geometry hints will be applied to or
    @code{nil}}
  @argument[geometry]{struct containing geometry information or @code{nil}}
  @argument[geom-mask]{mask indicating which struct fields should be paid
    attention to}
  @begin{short}
    This function sets up hints about how a window can be resized by the user.
  @end{short}
  You can set a minimum and maximum size; allowed resize increments (e. g. for
  xterm, you can only resize by the size of a character); aspect ratios; and
  more. See the @class{gdk-geometry} struct.
  @see-class{gdk-geometry}")

;;; --- gtk-window-set-gravity -------------------------------------------------

(setf (documentation 'gtk-window-set-gravity 'function)
 "@version{2013-1-9}
  @argument[window]{a @class{gtk-window} instance}
  @argument[gravity]{@arg{window} gravity}
  @begin{short}
    Window gravity defines the meaning of coordinates passed to
    @fun{gtk-window-move}. See @fun{gtk-window-move} and @symbol{gdk-gravity}
    for more details.
  @end{short}

  The default window gravity is @code{:north-west} which will typically
  \"do what you mean\".
  @see-symbol{gdk-gravity}
  @see-function{gtk-window-move})")

;;; --- gtk-window-get-gravity -------------------------------------------------

(setf (documentation 'gtk-window-get-gravity 'function)
 "@version{2013-1-9}
  @argument[window]{a @class{gtk-window} instance}
  @return{window gravity}
  @begin{short}
    Gets the value set by @fun{gtk-window-set-gravity}.
  @end{short}
  @see-function{gtk-window-set-gravity}")

;;; --- gtk-window-set-position ------------------------------------------------

(setf (documentation 'gtk-window-set-position 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[position]{a position constraint}
  @begin{short}
    Sets a position constraint for this window.
  @end{short}
  If the old or new constraint is @code{:center-always}, this will also cause
  the window to be repositioned to satisfy the new constraint.")

;;; --- gtk-window-set-transient-for -------------------------------------------

(setf (documentation 'gtk-window-set-transient-for 'function)
 "@version{2013-1-23}
  @argument[window]{a @class{gtk-window} instance}
  @argument[parent]{parent window, or @code{nil}}
  @begin{short}
    Dialog windows should be set transient for the main application window they
    were spawned from. This allows window managers to e.g. keep the dialog on
    top of the main window, or center the dialog over the main window.
  @end{short}
  @fun{gtk-dialog-new-with-buttons} and other convenience functions in GTK+ will
  sometimes call @sym{gtk-window-set-transient-for} on your behalf.

  Passing @code{nil} for parent unsets the current transient window.

  On Windows, this function puts the child window on top of the parent, much
  as the window manager would have done on X.")

;;; --- gtk-window-set-attached-to ---------------------------------------------

(setf (documentation 'gtk-window-set-attached-to 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[attach-widget]{a @class{gtk-widget}, or @code{nil}}
  @begin{short}
    Marks window as attached to @arg{attach-widget}.
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
  @see-function{gtk-window-set-transient-for}")

;;; --- gtk-window-set-destroy-with-parent -------------------------------------

(setf (documentation 'gtk-window-set-destroy-with-parent 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[setting]{whether to destroy @arg{window} with its transient parent}
  @begin{short}
    If @arg{setting} is @arg{true}, then destroying the transient parent of
    @arg{window} will also destroy @arg{window} itself.
  @end{short}
  This is useful for dialogs that shouldn't persist beyond the lifetime of the
  main window they're associated with, for example.")

;;; --- gtk-window-set-hide-title-when-maximized ------------------------------- 

(setf (documentation 'gtk-window-set-hide-title-when-maximized 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[setting]{whether to hide the titlebar when @arg{window} is
    maximized}
  @begin{short}
    If @arg{setting} is @arg{true}, then @arg{window} will request that it's
    titlebar should be hidden when maximized.
  @end{short}
  This is useful for windows that don't convey any information other than the
  application name in the titlebar, to put the available screen space to better
  use. If the underlying window system does not support the request, the setting
  will not have any effect.

  Since 3.4")

;;; --- gtk-window-set-screen --------------------------------------------------

(setf (documentation 'gtk-window-set-screen 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[screen]{a @class{gdk-screen} instance}
  @begin{short}
    Sets the @class{gdk-screen} where the @arg{window} is displayed; if the
    @arg{window} is already mapped, it will be unmapped, and then remapped on
    the new screen.
  @end{short}

  Since 2.2")

;;; --- gtk-window-get-screen --------------------------------------------------

(setf (documentation 'gtk-window-get-screen 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @return{a @class{gdk-screen} instance}
  @begin{short}
    Returns the @class{gdk-screen} associated with @arg{window}.
  @end{short}

  Since 2.2")

;;; gtk-window-is-active is implemented as the accessor of the slot "is-active".

;;; gtk-window-has-top-level-focus is implemented as the accessor of the slot
;;; "has-toplevel-focus".

;;; --- gtk-window-lisp-top-levels ---------------------------------------------

(setf (documentation 'gtk-window-list-top-levels 'function)
 "@version{2013-1-24}
  @return{list of toplevel widgets}
  @begin{short}
    Returns a list of all existing toplevel windows.
  @end{short}
  The widgets in the list are not individually referenced. If you want to
  iterate through the list and perform actions involving callbacks that might
  destroy the widgets, you must call
  @code{g_list_foreach (result, (GFunc)g_object_ref, NULL)}
  first, and then unref all the widgets afterwards.")

;;; --- gtk-window-add-mnemonic ------------------------------------------------

(setf (documentation 'gtk-window-add-mnemonic 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[keyval]{the mnemonic}
  @argument[target]{the widget that gets activated by the mnemonic}
  @begin{short}
    Adds a mnemonic to this @arg{window}.
  @end{short}")

;;; --- gtk-window-remove-mnemonic ---------------------------------------------

(setf (documentation 'gtk-window-remove-mnemonic 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[keyval]{the mnemonic}
  @argument[target]{the widget that gets activated by the mnemonic}
  @begin{short}
    Removes a mnemonic from this @arg{[window}.
  @end{short}")

;;; --- gtk-window-mnemonic-activate -------------------------------------------

(setf (documentation 'gtk-window-mnemonic-activate 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[keyval]{the mnemonic}
  @argument[modifier]{the modifiers}
  @return{@arg{true} if the activation is done}
  @begin{short}
    Activates the targets associated with the mnemonic.
  @end{short}")

;;; --- gtk-window-activate-key ------------------------------------------------

(setf (documentation 'gtk-window-activate-key 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @argument[event]{a @class{gdk-event-key}}
  @return{@arg{true} if a mnemonic or accelerator was found and activated.}
  @begin{short}
    Activates mnemonics and accelerators for this @arg{window}.
  @end{short}
  This is normally called by the default @code{::key_press_event} handler for
  toplevel windows, however in some cases it may be useful to call this directly
  when overriding the standard key handling for a toplevel window.

  Since 2.4")



#|
;;; ----------------------------------------------------------------------------
;;; gtk_window_propagate_key_event ()
;;; 
;;; gboolean gtk_window_propagate_key_event (GtkWindow *window,
;;;                                          GdkEventKey *event);
;;; 
;;; Propagate a key press or release event to the focus widget and up the focus
;;; container chain until a widget handles event. This is normally called by the
;;; default ::key_press_event and ::key_release_event handlers for toplevel
;;; windows, however in some cases it may be useful to call this directly when
;;; overriding the standard key handling for a toplevel window.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; event :
;;;     a GdkEventKey
;;; 
;;; Returns :
;;;     TRUE if a widget in the focus chain handled the event.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_propagate_key_event" gtk-window-propagate-key-event)
    :boolean
  (window (g-object gtk-window))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-window-propagate-key-event)
|#

;;; --- gtk-window-get-focus ---------------------------------------------------

(setf (documentation 'gtk-window-get-focus 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @return{The currently focused widget, or @code{nil} if there is none.}
  @begin{short}
    Retrieves the current focused widget within the window.
  @end{short}
  Note that this is the widget that would have the focus if the toplevel window
  focused; if the toplevel window is not focused then
  @code{(gtk-widget-has-focus widget)} will not be @em{true} for the widget.
  @see-function{gtk-widget-has-focus}")

;;; --- gtk-window-set-focus ---------------------------------------------------

(setf (documentation 'gtk-window-set-focus 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[focus]{widget to be the new focus widget, or @code{nil} to unset any
    focus widget for the toplevel window}
  @begin{short}
    If @arg{focus} is not the current focus widget, and is focusable, sets it as
    the focus widget for the @arg{window}.
  @end{short}
  If @arg{focus} is @code{nil}, unsets the focus widget for this @arg{window}.
  To set the focus to a particular widget in the toplevel, it is usually more
  convenient to use @fun{gtk-widget-grab-focus} instead of this function.
  @see-function{gtk-widget-grab-focus}")

;;; --- gtk-window-get-default-widget ------------------------------------------

(setf (documentation 'gtk-window-get-default-widget 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @return{The default widget, or @code{nil} if there is none.}
  @begin{short}
    Returns the default widget for @arg{window}. See
    @fun{gtk-window-set-default} for more details.
  @end{short}

  Since 2.14
  @see-function{gtk-window-set-default}")

;;; --- gtk-window-set-default -------------------------------------------------

(setf (documentation 'gtk-window-set-default 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[default-widget]{widget to be the default, or @code{nil} to unset the
    default widget for the toplevel}
  @begin{short}
    The default widget is the widget that's activated when the user presses
    Enter in a dialog (for example). This function sets or unsets the default
    widget for a GtkWindow about.
  @end{short}
  When setting (rather than unsetting) the default widget it's generally easier
  to call @fun{gtk-widget-grab-focus} on the widget. Before making a widget the
  default widget, you must set the @code{GTK_CAN_DEFAULT} flag on the widget
  you'd like to make the default using @code{GTK_WIDGET_SET_FLAGS()}.
  @see-function{gtk-widget-grab-focus}")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_window_present ()
;;; 
;;; void gtk_window_present (GtkWindow *window);
;;; 
;;; Presents a window to the user. This may mean raising the window in the
;;; stacking order, deiconifying it, moving it to the current desktop, and/or
;;; giving it the keyboard focus, possibly dependent on the user's platform,
;;; window manager, and preferences.
;;; 
;;; If window is hidden, this function calls gtk_widget_show() as well.
;;; 
;;; This function should be used when the user tries to open a window that's
;;; already open. Say for example the preferences dialog is currently open, and
;;; the user chooses Preferences from the menu a second time; use
;;; gtk_window_present() to move the already-open dialog where the user can see
;;; it.
;;; 
;;; If you are calling this function in response to a user interaction, it is
;;; preferable to use gtk_window_present_with_time().
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_present" gtk-window-present) :void
  (window (g-object gtk-window)))

(export 'gtk-window-present)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present_with_time ()
;;; 
;;; void gtk_window_present_with_time (GtkWindow *window, guint32 timestamp);
;;; 
;;; Presents a window to the user in response to a user interaction. If you need
;;; to present a window without a timestamp, use gtk_window_present(). See
;;; gtk_window_present() for details.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; timestamp :
;;;     the timestamp of the user interaction (typically a button or key press
;;;     event) which triggered this call
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_present_with_time" gtk-window-present-with-time) :void
  (window (g-object gtk-window))
  (timestamp :uint32))

(export 'gtk-window-present-with-time)

;;; ----------------------------------------------------------------------------
;;; gtk_window_iconify ()
;;; 
;;; void gtk_window_iconify (GtkWindow *window);
;;; 
;;; Asks to iconify (i.e. minimize) the specified window. Note that you
;;; shouldn't assume the window is definitely iconified afterward, because other
;;; entities (e.g. the user or window manager) could deiconify it again, or
;;; there may not be a window manager in which case iconification isn't
;;; possible, etc. But normally the window will end up iconified. Just don't
;;; write code that crashes if not.
;;; 
;;; It's permitted to call this function before showing a window, in which case
;;; the window will be iconified before it ever appears onscreen.
;;; 
;;; You can track iconification via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_iconify" gtk-window-iconify) :void
  (window (g-object gtk-window)))

(export 'gtk-window-iconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_deiconify ()
;;; 
;;; void gtk_window_deiconify (GtkWindow *window);
;;; 
;;; Asks to deiconify (i.e. unminimize) the specified window. Note that you
;;; shouldn't assume the window is definitely deiconified afterward, because
;;; other entities (e.g. the user or window manager) could iconify it again
;;; before your code which assumes deiconification gets to run.
;;; 
;;; You can track iconification via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_deiconify" gtk-window-deiconify) :void
  (window (g-object gtk-window)))

(export 'gtk-window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_stick ()
;;; 
;;; void gtk_window_stick (GtkWindow *window);
;;; 
;;; Asks to stick window, which means that it will appear on all user desktops.
;;; Note that you shouldn't assume the window is definitely stuck afterward,
;;; because other entities (e.g. the user or window manager) could unstick it
;;; again, and some window managers do not support sticking windows. But
;;; normally the window will end up stuck. Just don't write code that crashes
;;; if not.
;;; 
;;; It's permitted to call this function before showing a window.
;;; 
;;; You can track stickiness via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_stick" gtk-window-stick) :void
  (window (g-object gtk-window)))

(export 'gtk-window-stick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unstick ()
;;; 
;;; void gtk_window_unstick (GtkWindow *window);
;;; 
;;; Asks to unstick window, which means that it will appear on only one of the
;;; user's desktops. Note that you shouldn't assume the window is definitely
;;; unstuck afterward, because other entities (e.g. the user or window manager)
;;; could stick it again. But normally the window will end up stuck. Just don't
;;; write code that crashes if not.
;;; 
;;; You can track stickiness via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unstick" gtk-window-unstick) :void
  (window (g-object gtk-window)))

(export 'gtk-window-unstick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_maximize ()
;;; 
;;; void gtk_window_maximize (GtkWindow *window);
;;; 
;;; Asks to maximize window, so that it becomes full-screen. Note that you
;;; shouldn't assume the window is definitely maximized afterward, because other
;;; entities (e.g. the user or window manager) could unmaximize it again, and
;;; not all window managers support maximization. But normally the window will
;;; end up maximized. Just don't write code that crashes if not.
;;; 
;;; It's permitted to call this function before showing a window, in which case
;;; the window will be maximized when it appears onscreen initially.
;;; 
;;; You can track maximization via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_maximize" gtk-window-maximize) :void
  (window (g-object gtk-window)))

(export 'gtk-window-maximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unmaximize ()
;;; 
;;; void gtk_window_unmaximize (GtkWindow *window);
;;; 
;;; Asks to unmaximize window. Note that you shouldn't assume the window is
;;; definitely unmaximized afterward, because other entities (e.g. the user or
;;; window manager) could maximize it again, and not all window managers honor
;;; requests to unmaximize. But normally the window will end up unmaximized.
;;; Just don't write code that crashes if not.
;;; 
;;; You can track maximization via the "window-state-event" signal on GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_unmaximize" gtk-window-unmaximize) :void
  (window (g-object gtk-window)))

(export 'gtk-window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen ()
;;; 
;;; void gtk_window_fullscreen (GtkWindow *window);
;;; 
;;; Asks to place window in the fullscreen state. Note that you shouldn't assume
;;; the window is definitely full screen afterward, because other entities (e.g.
;;; the user or window manager) could unfullscreen it again, and not all window
;;; managers honor requests to fullscreen windows. But normally the window will
;;; end up fullscreen. Just don't write code that crashes if not.
;;; 
;;; You can track the fullscreen state via the "window-state-event" signal on
;;; GtkWidget.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_fullscreen" gtk-window-fullscreen) :void
  (window (g-object gtk-window)))

(export 'gtk-window-fullscreen)
|#

;;; --- gtk-window-unfullscreen ------------------------------------------------

(setf (documentation 'gtk-window-unfullscreen 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @begin{short}
    Asks to toggle off the fullscreen state for @arg{window}.
  @end{short}
  Note that you shouldn't assume the @arg{window} is definitely not full screen
  afterward, because other entities (e. g. the user or window manager) could
  fullscreen it again, and not all window managers honor requests to
  unfullscreen windows. But normally the window will end up restored to its
  normal state. Just don't write code that crashes if not.

  You can track the fullscreen state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Since 2.2")

;;; --- gtk-window-set-keep-above ----------------------------------------------

(setf (documentation 'gtk-window-set-keep-above 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[setting]{whether to keep @arg{window} above other windows}
  @begin{short}
    Asks to keep @arg{window} above, so that it stays on top.
  @end{short}
  Note that you shouldn't assume the @arg{window} is definitely above afterward,
  because other entities (e. g. the user or window manager) could not keep it
  above, and not all window managers support keeping windows above. But normally
  the window will end kept above. Just don't write code that crashes if not.

  It's permitted to call this function before showing a window, in which case
  the window will be kept above when it appears onscreen initially.

  You can track the above state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Note that, according to the Extended Window Manager Hints specification, the
  above state is mainly meant for user preferences and should not be used by
  applications e. g. for drawing attention to their dialogs.

  Since 2.4")

;;; --- gtk-window-set-keep-below ----------------------------------------------

(setf (documentation 'gtk-window-set-keep-below 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[setting]{whether to keep @arg{window} below other windows}
  @begin{short}
    Asks to keep window below, so that it stays in bottom.
  @end{short}
  Note that you shouldn't assume the @arg{window} is definitely below afterward,
  because other entities (e. g. the user or window manager) could not keep it
  below, and not all window managers support putting windows below. But normally
  the window will be kept below. Just don't write code that crashes if not.

  It's permitted to call this function before showing a window, in which case
  the window will be kept below when it appears onscreen initially.

  You can track the below state via the \"window-state-event\" signal on
  @class{gtk-widget}.

  Note that, according to the Extended Window Manager Hints specification,
  the above state is mainly meant for user preferences and should not be used
  by applications e. g. for drawing attention to their dialogs.

  Since 2.4")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_resize_drag ()
;;; 
;;; void gtk_window_begin_resize_drag (GtkWindow *window,
;;;                                    GdkWindowEdge edge,
;;;                                    gint button,
;;;                                    gint root_x,
;;;                                    gint root_y,
;;;                                    guint32 timestamp);
;;; 
;;; Starts resizing a window. This function is used if an application has window
;;; resizing controls. When GDK can support it, the resize will be done using
;;; the standard mechanism for the window manager or windowing system.
;;; Otherwise, GDK will try to emulate window resizing, potentially not all that
;;; well, depending on the windowing system.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; button :
;;;     mouse button that initiated the drag
;;; 
;;; edge :
;;;     position of the resize control
;;; 
;;; root_x :
;;;     X position where the user clicked to initiate the drag, in root window
;;;     coordinates
;;; 
;;; root_y :
;;;     Y position where the user clicked to initiate the drag
;;; 
;;; timestamp :
;;;     timestamp from the click event that initiated the drag
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_begin_resize_drag" gtk-window-begin-resize-drag) :void
  (window (g-object gtk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_move_drag ()
;;; 
;;; void gtk_window_begin_move_drag (GtkWindow *window,
;;;                                  gint button,
;;;                                  gint root_x,
;;;                                  gint root_y,
;;;                                  guint32 timestamp);
;;; 
;;; Starts moving a window. This function is used if an application has window
;;; movement grips. When GDK can support it, the window movement will be done
;;; using the standard mechanism for the window manager or windowing system.
;;; Otherwise, GDK will try to emulate window movement, potentially not all that
;;; well, depending on the windowing system.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; button :
;;;     mouse button that initiated the drag
;;; 
;;; root_x :
;;;     X position where the user clicked to initiate the drag, in root window
;;;     coordinates
;;; 
;;; root_y :
;;;     Y position where the user clicked to initiate the drag
;;; 
;;; timestamp :
;;;     timestamp from the click event that initiated the drag
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_begin_move_drag" gtk-window-begin-move-drag) :void
  (window (g-object gtk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-move-drag)

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
|#

;;; --- gtk-window-set-mnemonic-modifier ---------------------------------------

(setf (documentation 'gtk-window-set-mnemonic-modifier 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[modifier]{the modifier mask used to activate mnemonics on this
    @arg{window}}
  @short{Sets the mnemonic modifier for this @arg{window}.}")

#|
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
|#

;;; --- gtk-window-set-startup-id ----------------------------------------------

(setf (documentation 'gtk-window-set-startup-id 'function)
 "@version{2013-1-23}
  @argument[window]{a @class{gtk-window} instance}
  @argument[startup-id]{a string with startup-notification identifier}
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

  Since 2.12")

#|
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
;;; 
;;; GList * gtk_window_get_default_icon_list (void);
;;; 
;;; Gets the value set by gtk_window_set_default_icon_list(). The list is a copy
;;; and should be freed with g_list_free(), but the pixbufs in the list have not
;;; had their reference count incremented.
;;; 
;;; Returns :
;;;     copy of default icon list
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_list" gtk-window-get-default-icon-list)
  (g-list (g-object gdk-pixbuf)))

(export 'gtk-window-get-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_name ()
;;; 
;;; const gchar * gtk_window_get_default_icon_name (void);
;;; 
;;; Returns the fallback icon name for windows that has been set with
;;; gtk_window_set_default_icon_name(). The returned string is owned by GTK+ and
;;; should not be modified. It is only valid until the next call to
;;; gtk_window_set_default_icon_name().
;;; 
;;; Returns :
;;;     the fallback icon name for windows
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_default_icon_name" gtk-window-get-default-icon-name)
  (:string :free-from-foreign nil))

(export 'gtk-window-get-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_size ()
;;; 
;;; void gtk_window_get_default_size (GtkWindow *window,
;;;                                   gint *width,
;;;                                   gint *height);
;;; 
;;; Gets the default size of the window. A value of -1 for the width or height
;;; indicates that a default size has not been explicitly set for that
;;; dimension, so the "natural" size of the window will be used.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; width :
;;;     location to store the default width, or NULL
;;; 
;;; height :
;;;     location to store the default height, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-get-default-size))

(defun gtk-window-get-default-size (window)
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
;;; 
;;; GdkPixbuf * gtk_window_get_icon (GtkWindow *window);
;;; 
;;; Gets the value set by gtk_window_set_icon() (or if you've called
;;; gtk_window_set_icon_list(), gets the first icon in the icon list).
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Returns :
;;;     icon for window
;;; ----------------------------------------------------------------------------
|#

;;; --- gtk-window-get-icon-list -----------------------------------------------

(setf (documentation 'gtk-window-get-icon-list 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance.}
  @return{copy of @arg{window}'s icon list}
  @begin{short}
    Retrieves the list of icons set by @fun{gtk-window-set-icon-list}.
  @end{short}
  The list is copied, but the reference count on each member won't be
  incremented.
  @see-function{gtk-window-set-icon-list}")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon_name ()
;;; 
;;; const gchar * gtk_window_get_icon_name (GtkWindow *window);
;;; 
;;; Returns the name of the themed icon for the window, see
;;; gtk_window_set_icon_name().
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Returns :
;;;     the icon name or NULL if the window has no themed icon
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------
|#

;;; --- gtk-window-get-mnemonic-modifier ---------------------------------------

(setf (documentation 'gtk-window-get-mnemonic-modifier 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @return{The modifier mask used to activate mnemonics on this @arg{window}.}
  @begin{short}
    Returns the mnemonic modifier for this @arg{window}.
  @end{short}
  See @fun{gtk-window-set-mnemonic-modifier}.
  @see-function{gtk-window-set-mnemonic-modifier}")

#|
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
;;; 
;;; void gtk_window_get_position (GtkWindow *window, gint *root_x, gint *root_y)
;;; 
;;; This function returns the position you need to pass to gtk_window_move() to
;;; keep window in its current position. This means that the meaning of the
;;; returned value varies with window gravity. See gtk_window_move() for more
;;; details.
;;; 
;;; If you haven't changed the window gravity, its gravity will be
;;; GDK_GRAVITY_NORTH_WEST. This means that gtk_window_get_position() gets the
;;; position of the top-left corner of the window manager frame for the window.
;;; gtk_window_move() sets the position of this same top-left corner.
;;; 
;;; gtk_window_get_position() is not 100% reliable because the X Window System
;;; does not specify a way to obtain the geometry of the decorations placed on a
;;; window by the window manager. Thus GTK+ is using a "best guess" that works
;;; with most window managers.
;;; 
;;; Moreover, nearly all window managers are historically broken with respect to
;;; their handling of window gravity. So moving a window to its current position
;;; as returned by gtk_window_get_position() tends to result in moving the
;;; window slightly. Window managers are slowly getting better over time.
;;; 
;;; If a window has gravity GDK_GRAVITY_STATIC the window manager frame is not
;;; relevant, and thus gtk_window_get_position() will always produce accurate
;;; results. However you can't use static gravity to do things like place a
;;; window in a corner of the screen, because static gravity ignores the window
;;; manager decorations.
;;; 
;;; If you are saving and restoring your application's window positions, you
;;; should know that it's impossible for applications to do this without getting
;;; it somewhat wrong because applications do not have sufficient knowledge of
;;; window manager state. The Correct Mechanism is to support the session
;;; management protocol (see the "GnomeClient" object in the GNOME libraries for
;;; example) and allow the window manager to save your window sizes and
;;; positions.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; root_x :
;;;     return location for X coordinate of gravity-determined reference point,
;;;     or NULL
;;; 
;;; root_y :
;;;     return location for Y coordinate of gravity-determined reference point,
;;;     or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_position" %gtk-window-get-position) :void
  (window (g-object gtk-window))
  (root-x (:pointer :int))
  (root-y (:pointer :int)))

;; The Lisp implementation returns the position as a value list.

(defun gtk-window-get-position (window)
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
;;; 
;;; void gtk_window_get_size (GtkWindow *window, gint *width, gint *height);
;;; 
;;; Obtains the current size of window. If window is not onscreen, it returns
;;; the size GTK+ will suggest to the window manager for the initial window size
;;; (but this is not reliably the same as the size the window manager will
;;; actually select). The size obtained by gtk_window_get_size() is the last
;;; size received in a GdkEventConfigure, that is, GTK+ uses its locally-stored
;;; size, rather than querying the X server for the size. As a result, if you
;;; call gtk_window_resize() then immediately call gtk_window_get_size(), the
;;; size won't have taken effect yet. After the window manager processes the
;;; resize request, GTK+ receives notification that the size has changed via a
;;; configure event, and the size of the window gets updated.
;;; 
;;; Note 1: Nearly any use of this function creates a race condition, because
;;; the size of the window may change between the time that you get the size and
;;; the time that you perform some action assuming that size is the current
;;; size. To avoid race conditions, connect to "configure-event" on the window
;;; and adjust your size-dependent state to match the size delivered in the
;;; GdkEventConfigure.
;;; 
;;; Note 2: The returned size does not include the size of the window manager
;;; decorations (aka the window frame or border). Those are not drawn by GTK+
;;; and GTK+ has no reliable method of determining their size.
;;; 
;;; Note 3: If you are getting a window size in order to position the window
;;; onscreen, there may be a better way. The preferred way is to simply set the
;;; window's semantic type with gtk_window_set_type_hint(), which allows the
;;; window manager to e.g. center dialogs. Also, if you set the transient parent
;;; of dialogs with gtk_window_set_transient_for() window managers will often
;;; center the dialog over its parent window. It's much preferred to let the
;;; window manager handle these things rather than doing it yourself, because
;;; all apps will behave consistently and according to user prefs if the window
;;; manager handles it. Also, the window manager can take the size of the window
;;; decorations/border into account, while your application cannot.
;;; 
;;; In any case, if you insist on application-specified window positioning,
;;; there's still a better way than doing it yourself -
;;; gtk_window_set_position() will frequently handle the details for you.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; width :
;;;     return location for width, or NULL
;;; 
;;; height :
;;;     return location for height, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_get_size" %gtk-window-get-size) :void
  (window (g-object gtk-window))
  (width (:pointer :int))
  (height (:pointer :int)))

;; The Lisp implemenation returns the size as a value list.

(defun gtk-window-get-size (window)
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-window-get-size window width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(export 'gtk-window-get-size)
|#

;;; --- gtk-window-get-transient-for -------------------------------------------

(setf (documentation 'gtk-window-get-transient-for 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @return{The transient parent for this @arg{window}, or @code{nil} if no
    transient parent has been set}
  @begin{short}
    Fetches the transient parent for this window.
  @end{short}
  See @fun{gtk-window-set-transient-for}.")

#|
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
|#

;;; --- gtk-window-get-group ---------------------------------------------------

(setf (documentation 'gtk-window-get-group 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance, or @code{nil}}
  @return{The @class{gtk-window-group} for a @arg{window} or the default group.}
  @begin{short}
    Returns the group for @arg{window} or the default group, if @arg{window} is
    @code{nil} or if @arg{window} does not have an explicit window group.
  @end{short}

  Since 2.10")

;;; --- gtk-window-has-group ---------------------------------------------------

(setf (documentation 'gtk-window-has-group 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @return{@em{True} if @arg{window} has an explicit window group.}
  @short{Returns whether window has an explicit window group.}

  Since 2.22")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_window_move ()
;;; 
;;; void gtk_window_move (GtkWindow *window, gint x, gint y);
;;; 
;;; Asks the window manager to move window to the given position. Window
;;; managers are free to ignore this; most window managers ignore requests for
;;; initial window positions (instead using a user-defined placement algorithm)
;;; and honor requests after the window has already been shown.
;;; 
;;; Note: the position is the position of the gravity-determined reference point
;;; for the window. The gravity determines two things: first, the location of
;;; the reference point in root window coordinates; and second, which point on
;;; the window is positioned at the reference point.
;;; 
;;; By default the gravity is GDK_GRAVITY_NORTH_WEST, so the reference point is
;;; simply the x, y supplied to gtk_window_move(). The top-left corner of the
;;; window decorations (aka window frame or border) will be placed at x, y.
;;; Therefore, to position a window at the top left of the screen, you want to
;;; use the default gravity (which is GDK_GRAVITY_NORTH_WEST) and move the
;;; window to 0,0.
;;; 
;;; To position a window at the bottom right corner of the screen, you would set
;;; GDK_GRAVITY_SOUTH_EAST, which means that the reference point is at x + the
;;; window width and y + the window height, and the bottom-right corner of the
;;; window border will be placed at that reference point. So, to place a window
;;; in the bottom right corner you would first set gravity to south east, then
;;; write: gtk_window_move (window, gdk_screen_width() - window_width,
;;; gdk_screen_height() - window_height) (note that this example does not take
;;; multi-head scenarios into account).
;;; 
;;; The Extended Window Manager Hints specification at
;;; http://www.freedesktop.org/Standards/wm-spec has a nice table of gravities
;;; in the "implementation notes" section.
;;; 
;;; The gtk_window_get_position() documentation may also be relevant.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; x :
;;;     X coordinate to move window to
;;; 
;;; y :
;;;     Y coordinate to move window to
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_move" gtk-window-move) :void
  (window (g-object gtk-window))
  (x :int)
  (y :int))

(export 'gtk-window-move)

;;; ----------------------------------------------------------------------------
;;; gtk_window_parse_geometry ()
;;; 
;;; gboolean gtk_window_parse_geometry (GtkWindow *window,
;;;                                     const gchar *geometry);
;;; 
;;; Parses a standard X Window System geometry string - see the manual page for
;;; X (type 'man X') for details on this. gtk_window_parse_geometry() does work
;;; on all GTK+ ports including Win32 but is primarily intended for an X
;;; environment.
;;; 
;;; If either a size or a position can be extracted from the geometry string,
;;; gtk_window_parse_geometry() returns TRUE and calls
;;; gtk_window_set_default_size() and/or gtk_window_move() to resize/move the
;;; window.
;;; 
;;; If gtk_window_parse_geometry() returns TRUE, it will also set the
;;; GDK_HINT_USER_POS and/or GDK_HINT_USER_SIZE hints indicating to the window
;;; manager that the size/position of the window was user-specified. This causes
;;; most window managers to honor the geometry.
;;; 
;;; Note that for gtk_window_parse_geometry() to work as expected, it has to be
;;; called when the window has its "final" size, i.e. after calling
;;; gtk_widget_show_all() on the contents and gtk_window_set_geometry_hints() on
;;; the window.
;;; 
;;; #include <gtk/gtk.h>
;;;    
;;; static void
;;; fill_with_content (GtkWidget *vbox)
;;; {
;;;   /* fill with content... */
;;; }
;;;    
;;; int
;;; main (int argc, char *argv[])
;;; {
;;;   GtkWidget *window, *vbox;
;;;   GdkGeometry size_hints = {
;;;     100, 50, 0, 0, 100, 50, 10, 10, 0.0, 0.0, GDK_GRAVITY_NORTH_WEST  
;;;   };
;;;    
;;;   gtk_init (&argc, &argv);
;;;   
;;;   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;   vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, FALSE, 0);
;;;   
;;;   gtk_container_add (GTK_CONTAINER (window), vbox);
;;;   fill_with_content (vbox);
;;;   gtk_widget_show_all (vbox);
;;;   
;;;   gtk_window_set_geometry_hints (GTK_WINDOW (window),
;;;                     window,
;;;                     &size_hints,
;;;                     GDK_HINT_MIN_SIZE | 
;;;                     GDK_HINT_BASE_SIZE | 
;;;                     GDK_HINT_RESIZE_INC);
;;;   
;;;   if (argc > 1)
;;;     {
;;;       if (!gtk_window_parse_geometry (GTK_WINDOW (window), argv[1]))
;;;         fprintf (stderr, "Failed to parse '%s'\n", argv[1]);
;;;     }
;;;    
;;;   gtk_widget_show_all (window);
;;;   gtk_main ();
;;;    
;;;   return 0;
;;; }
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; geometry :
;;;     geometry string
;;; 
;;; Returns :
;;;     TRUE if string was parsed successfully
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_parse_geometry" gtk-window-parse-geometry) :boolean
  (window (g-object gtk-window))
  (geometry :string))

(export 'gtk-window-parse-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_reshow_with_initial_size ()
;;; 
;;; void gtk_window_reshow_with_initial_size (GtkWindow *window);
;;; 
;;; Hides window, then reshows it, resetting the default size and position of
;;; the window. Used by GUI builders only.
;;; 
;;; window :
;;;     a GtkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_reshow_with_initial_size"
          gtk-window-reshow-with-initial-size) :void
  (window (g-object gtk-window)))

(export 'gtk-window-reshow-with-initial-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize ()
;;; 
;;; void gtk_window_resize (GtkWindow *window, gint width, gint height);
;;; 
;;; Resizes the window as if the user had done so, obeying geometry constraints.
;;; The default geometry constraint is that windows may not be smaller than
;;; their size request; to override this constraint, call
;;; gtk_widget_set_size_request() to set the window's request to a smaller
;;; value.
;;; 
;;; If gtk_window_resize() is called before showing a window for the first time,
;;; it overrides any default size set with gtk_window_set_default_size().
;;; 
;;; Windows may not be resized smaller than 1 by 1 pixels.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; width :
;;;     width in pixels to resize the window to
;;; 
;;; height :
;;;     height in pixels to resize the window to
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_resize" gtk-window-resize) :void
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
;;; 
;;; void gtk_window_set_default_icon_list (GList *list);
;;; 
;;; Sets an icon list to be used as fallback for windows that haven't had
;;; gtk_window_set_icon_list() called on them to set up a window-specific icon
;;; list. This function allows you to set up the icon for all windows in your
;;; app at once.
;;; 
;;; See gtk_window_set_icon_list() for more details.
;;; 
;;; list :
;;;     a list of GdkPixbuf
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_list" gtk-window-set-default-icon-list)
    :boolean
  (icon-list (g-list (g-object gdk-pixbuf))))

(export 'gtk-window-set-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon ()
;;; 
;;; void gtk_window_set_default_icon (GdkPixbuf *icon);
;;; 
;;; Sets an icon to be used as fallback for windows that haven't had
;;; gtk_window_set_icon() called on them from a pixbuf.
;;; 
;;; icon :
;;;     the icon
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon" gtk-window-set-default-icon) :void
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
;;; 
;;; void gtk_window_set_default_icon_name (const gchar *name);
;;; 
;;; Sets an icon to be used as fallback for windows that haven't had
;;; gtk_window_set_icon_list() called on them from a named themed icon, see
;;; gtk_window_set_icon_name().
;;; 
;;; name :
;;;     the name of the themed icon
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_default_icon_name" gtk-window-set-default-icon-name)
    :void
  (name :string))

(export 'gtk-window-set-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon ()
;;; 
;;; void gtk_window_set_icon (GtkWindow *window, GdkPixbuf *icon);
;;; 
;;; Sets up the icon representing a GtkWindow. This icon is used when the window
;;; is minimized (also known as iconified). Some window managers or desktop
;;; environments may also place it in the window frame, or display it in other
;;; contexts.
;;; 
;;; The icon should be provided in whatever size it was naturally drawn; that
;;; is, don't scale the image before passing it to GTK+. Scaling is postponed
;;; until the last minute, when the desired final size is known, to allow best
;;; quality.
;;; 
;;; If you have your icon hand-drawn in multiple sizes, use
;;; gtk_window_set_icon_list(). Then the best size will be used.
;;; 
;;; This function is equivalent to calling gtk_window_set_icon_list() with a
;;; 1-element list.
;;; 
;;; See also gtk_window_set_default_icon_list() to set the icon for all windows
;;; in your application in one go.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; icon :
;;;     icon image, or NULL
;;; ----------------------------------------------------------------------------
|#

;;; --- gtk-window-set-icon-list -----------------------------------------------

(setf (documentation 'gtk-window-set-icon-list 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[list]{list of @class{gdk-pixbuf} instances}
  @begin{short}
    Sets up the icon representing a @class{gtk-window} instance.
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

  Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and larger
  images (64x64, 128x128) if you have them.

  See also @fun{gtk-window-set-default-icon-list} to set the icon for all
  windows in your application in one go.

  Note that transient windows (those who have been set transient for another
  window using @fun{gtk-window-set-transient-for}) will inherit their icon from
  their transient parent. So there's no need to explicitly set the icon on
  transient windows.
  @see-function{gtk-window-set-default-icon-list}
  @see-function{gtk-window-set-transient-for}")

#|
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
;;; 
;;; void gtk_window_set_icon_name (GtkWindow *window, const gchar *name);
;;; 
;;; Sets the icon for the window from a named themed icon. See the docs for
;;; GtkIconTheme for more details.
;;; 
;;; Note that this has nothing to do with the WM_ICON_NAME property which is
;;; mentioned in the ICCCM.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; name :
;;;     the name of the themed icon
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_auto_startup_notification ()
;;; 
;;; void gtk_window_set_auto_startup_notification (gboolean setting);
;;; 
;;; By default, after showing the first GtkWindow, GTK+ calls
;;; gdk_notify_startup_complete(). Call this function to disable the automatic
;;; startup notification. You might do this if your first window is a splash
;;; screen, and you want to delay notification until after your real main window
;;; has been shown, for example.
;;; 
;;; In that example, you would disable startup notification temporarily, show
;;; your splash screen, then re-enable it so that showing the main window would
;;; automatically result in notification.
;;; 
;;; setting :
;;;     TRUE to automatically do startup notification
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_set_auto_startup_notification"
          gtk-set-window-auto-startup-notification) :void
  (setting :boolean))

(export 'gtk-set-window-auto-startup-notification)
|#

;;; --- gtk-window-get-opacity -------------------------------------------------

(setf (documentation 'gtk-window-get-opacity 'function)
 "@version{2013-1-23}
  @argument[window]{a @class{gtk-window} instance}
  @return{The requested opacity for this window.}
  @begin{short}
    Fetches the requested opacity for this window.
  @end{short}
  See @fun{gtk-window-set-opacity}.

  Since 2.12")

;;; --- gtk-window-set-opacity -------------------------------------------------

(setf (documentation 'gtk-window-set-opacity 'function)
 "@version{2013-1-23}
  @argument[window]{a @class{gtk-window} instance}
  @argument[opacity]{desired opacity, between @code{0} and @code{1}}
  @begin{short}
    Request the windowing system to make window partially transparent, with
    @arg{opacity} 0 being fully transparent and 1 fully opaque.
  @end{short}
  (Values of the @arg{opacity} parameter are clamped to the [0,1] range.) On X11
  this has any effect only on X screens with a compositing manager running. See
  @fun{gtk-widget-is-composited}. On Windows it should work always.

  Note that setting a window's opacity after the window has been shown causes
  it to flicker once on Windows.

  Since 2.12")

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
;;; 
;;; void gtk_window_set_has_resize_grip (GtkWindow *window, gboolean value);
;;; 
;;; Sets whether window has a corner resize grip.
;;; 
;;; Note that the resize grip is only shown if the window is actually resizable
;;; and not maximized. Use gtk_window_resize_grip_is_visible() to find out if
;;; the resize grip is currently shown.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; value :
;;;     TRUE to allow a resize grip
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_has_resize_grip ()
;;; 
;;; gboolean gtk_window_get_has_resize_grip (GtkWindow *window);
;;; 
;;; Determines whether the window may have a resize grip.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Returns :
;;;     TRUE if the window has a resize grip
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_grip_is_visible ()
;;; 
;;; gboolean gtk_window_resize_grip_is_visible (GtkWindow *window);
;;; 
;;; Determines whether a resize grip is visible for the specified window.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Returns :
;;;     TRUE if a resize grip exists and is visible
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;;   "accept-focus"             gboolean             : Read / Write
;;;   "application"              GtkApplication*      : Read / Write
;;;   "attached-to"              GtkWidget*           : Read / Write / Construct



;;; --- gtk-widget-decorated ---------------------------------------------------

(setf (gethash 'gtk-window-decorated atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-window-decorated 'function)
 "@version{2013-1-7}
  @begin{short}
    Accessor of the slot \"decorated\" of the @class{gtk-window} class.
  @end{short}
  
  See the function @fun{gtk-window-set-decorated} for details.
  @see-function{gtk-window-set-decorated}")

;;; --- gtk-window-get-decorated -----------------------------------------------

(setf (documentation 'gtk-window-get-decorated 'function)
 "@version{2013-7-1}
  @argument[window]{a @class{gtk-window} instance}
  @return{@arg{true} if the @arg{window} has been set to have decorations}
  @begin{short}
    Returns whether the window has been set to have decorations such as a title
    bar via @fun{gtk-window-set-decorated}.
  @end{short}
  @see-function{gtk-window-set-decorated}")

;;; --- gtk-window-set-decorated -----------------------------------------------

(setf (documentation 'gtk-window-set-decorated 'function)
 "@version{2013-1-7}
  @argument[window]{a @class{gtk-window} instance}
  @argument[setting]{@arg{true} to decorate the @arg{window}}
  @begin{short}
    By default, windows are decorated with a title bar, resize controls, etc.
    Some window managers allow GTK+ to disable these decorations, creating a
    borderless window.
  @end{short}
  If you set the decorated property to FALSE using this function, GTK+ will do
  its best to convince the window manager not to decorate the window. Depending
  on the system, this function may not have any effect when called on a window
  that is already visible, so you should call it before calling
  @fun{gtk-widget-show}.

  On Windows, this function always works, since there's no window manager
  policy involved.
  @see-function{gtk-widget-show}")

;;; ----------------------------------------------------------------------------

;;;   "default-height"           gint                 : Read / Write

;;; --- gtk-window-default-height ----------------------------------------------

(setf (gethash 'gtk-window-default-height atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-window-default-height 'function)
 "@version{2012-12-30}
  @begin{short}
    Accessor of the property @arg{\"default-height\"} of the @class{gtk-window}
    class.
  @end{short}
  See @fun{gtk-window-set-default-size} for more information.
  @see-function{gtk-window-set-default-size}")

;;;   "default-width"            gint                 : Read / Write

;;; --- gtk-window-default-width -----------------------------------------------

(setf (gethash 'gtk-window-default-width atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-window-default-width 'function)
 "@version{2012-12-30}
  @begin{short}
    Accessor of the property @arg{\"default-width\"} of the @class{gtk-window}
    class.
  @end{short}
  See @fun{gtk-window-set-default-size} for more information.
  @see-function{gtk-window-set-default-size}")


;;;   "deletable"                gboolean             : Read / Write
;;;   "destroy-with-parent"      gboolean             : Read / Write



;;;   "focus-on-map"             gboolean             : Read / Write
;;;   "focus-visible"            gboolean             : Read / Write
;;;   "gravity"                  GdkGravity           : Read / Write
;;;   "has-resize-grip"          gboolean             : Read / Write
;;;   "has-toplevel-focus"       gboolean             : Read

;;; --- gtk-window-has-toplevel-focus ------------------------------------------

(setf (documentation 'gtk-window-has-toplevel-focus 'function)
 "@version{2013-1-24}
  @argument[window]{a @class{gtk-window} instance}
  @return{@arg{true} if the input focus is within this @arg{Window}}
  @begin{short}
    Returns whether the input focus is within this @arg{window}.
  @end{short}
  For real toplevel windows, this is identical to @fun{gtk-window-is-active},
  but for embedded windows, like @class{gtk-plug}, the results will differ.

  Since 2.4")

;;;   "hide-titlebar-when-maximized" gboolean         : Read / Write



;;;   "icon"                     GdkPixbuf*           : Read / Write
;;;   "icon-name"                gchar*               : Read / Write
;;;   "is-active"                gboolean             : Read

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

;;;   "mnemonics-visible"        gboolean             : Read / Write
;;;   "modal"                    gboolean             : Read / Write


;;; --- gtk-window-opacity -----------------------------------------------------

(setf (gethash 'gtk-window-opacity atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-window-opacity 'function)
 "@version{2013-1-23}
  @begin{short}
    Accessor of the slot @code{opacity} of the @class{gtk-window} class.
  @end{short}
  @see-function{gtk-window-get-opacity}
  @see-function{gtk-window-set-opacity}")

;;;   "resize-grip-visible"      gboolean             : Read
;;;   "role"                     gchar*               : Read / Write
;;;   "screen"                   GdkScreen*           : Read / Write


;;;   "skip-pager-hint"          gboolean             : Read / Write
;;;   "skip-taskbar-hint"        gboolean             : Read / Write

;;; --- gtk-window-startup-id --------------------------------------------------

(setf (gethash 'gtk-window-startup-id atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-window-startup-id 'function)
 "@version{2013-1-23}
  @begin{short}
    Accessor of the slot @code{startup-id} of the @class{gtk-window} class.
  @end{short}
  
  @see-function{gtk-window-set-startup-id}")

;;; --- gtk-window-get-title ---------------------------------------------------

(setf (documentation 'gtk-window-get-title 'function)
 "@version{2013-1-23}
  @argument[window]{a @class{gtk-window} instance}
  @return{The title of the @arg{window}, or @code{nil} if none has been set
    explicitely.}
  @begin{short}
    Retrieves the title of the @arg{window}.
  @end{short}
  See @fun{gtk-window-set-title}.")

;;; --- gtk-window-title -------------------------------------------------------

(setf (gethash 'gtk-window-title atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-window-title 'function)
 "@version{2013-1-23}
  @begin{short}
    Accessor of the slot @code{title} of the @class{gtk-window} class.
  @end{short}
  @see-function{gtk-window-get-title}
  @see-function{gtk-window-set-title}")



;;;   "transient-for"            GtkWindow*           : Read / Write / Construct



;;;   "type"                     GtkWindowType        : Read / Write / Construct

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_window_type ()
;;; 
;;; GtkWindowType gtk_window_get_window_type (GtkWindow *window);
;;; 
;;; Gets the type of the window. See GtkWindowType.
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Returns :
;;;     the type of the window
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;;   "type-hint"                GdkWindowTypeHint    : Read / Write
;;;   "urgency-hint"             gboolean             : Read / Write
;;;   "window-position"          GtkWindowPosition    : Read / Write




;;; --- End of file atdoc-gtk.window.lisp --------------------------------------
