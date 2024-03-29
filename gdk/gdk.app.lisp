;;; ----------------------------------------------------------------------------
;;; gdk.app.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; Application launching
;;;
;;;     Startup notification for applications
;;;
;;; Types and Values
;;;
;;;     GdkAppLaunchContext
;;;
;;; Functions
;;;
;;;     gdk_app_launch_context_new                         deprecated
;;;     gdk_app_launch_context_set_display                 Accessor / deprecated
;;;     gdk_app_launch_context_set_screen
;;;     gdk_app_launch_context_set_desktop
;;;     gdk_app_launch_context_set_timestamp
;;;     gdk_app_launch_context_set_icon
;;;     gdk_app_launch_context_set_icon_name
;;;
;;; Properties
;;;
;;;     GdkDisplay*    display    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GAppLaunchContext
;;;         ╰── GdkAppLaunchContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAppLaunchContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkAppLaunchContext" gdk-app-launch-context
  (:superclass g-app-launch-context
   :export t
   :interfaces nil
   :type-initializer "gdk_app_launch_context_get_type")
  ((display
    gdk-app-launch-context-display
    "display" "GdkDisplay" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-app-launch-context 'type)
 "@version{2021-12-11}
  @begin{short}
    The @sym{gdk-app-launch-context} object is an implementation of the
    @class{g-app-launch-context} object that handles launching an application
    in a graphical context.
  @end{short}
  It provides startup notification and allows to launch applications on a
  specific screen or workspace.
  @begin[Example]{dictionary}
    Launching an application.
    @begin{pre}
GdkAppLaunchContext *context;

context = gdk_display_get_app_launch_context (display);

gdk_app_launch_context_set_screen (screen);
gdk_app_launch_context_set_timestamp (event->time);

if (!g_app_info_launch_default_for_uri (\"http://www.gtk.org\",
                                        context, &error))
  g_warning (\"Launching failed: %s\n\", error->message);

g_object_unref (context);
    @end{pre}
  @end{dictionary}
  @see-class{g-app-launch-context}
  @see-slot{gdk-app-launch-context-display}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-app-launch-context-display -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display"
                                               'gdk-app-launch-context) 't)
 "The @code{display} property of type @class{gdk-display}
  (Read / Write / Construct Only) @br{}
  Display.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-app-launch-context-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-app-launch-context-display 'function)
 "@version{2021-12-11}
  @syntax[]{(gdk-app-launch-context-display object) => display}
  @syntax[]{(setf (gdk-app-launch-context-display object) display)}
  @argument[object]{a @class{gdk-app-launch-context} object}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Accessor of the @code{display} slot of the @class{gdk-app-launch-context}
    class.
  @end{short}

  The @sym{(setf gdk-app-launch-context-display)} slot access function sets the
  display on which applications will be launched when using this context. See
  also the @fun{gdk-app-launch-context-set-screen} function.
  @begin[Warning]{dictionary}
    The @sym{gdk-app-launch-context-display} function has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @fun{gdk-display-app-launch-context} function instead.
  @end{dictionary}
  @see-class{gdk-app-launch-context}
  @see-class{gdk-display}
  @see-function{gdk-display-app-launch-context}
  @see-function{gdk-app-launch-context-set-screen}")

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_new" gdk-app-launch-context-new)
         (g-object gdk-app-launch-context)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @return{A new @class{gdk-app-launch-context} object.}
  @begin{short}
    Creates a new @class{gdk-app-launch-context} object.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-app-launch-context-new} function has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @fun{gdk-display-app-launch-context} function instead.
  @end{dictionary}
  @see-class{gdk-app-launch-context}
  @see-function{gdk-display-app-launch-context}")

(export 'gdk-app-launch-context-new)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_screen" gdk-app-launch-context-set-screen)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Sets the screen on which applications will be launched when using this
    context.
  @end{short}
  See also the @fun{gdk-app-launch-context-display} function.

  If both screen and display are set, the screen takes priority. If neither
  screen or display are set, the default screen and display are used.
  @see-class{gdk-app-launch-context}
  @see-class{gdk-screen}
  @see-function{gdk-app-launch-context-display}"
  (context (g-object gdk-app-launch-context))
  (screen (g-object gdk-screen)))

(export 'gdk-app-launch-context-set-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_desktop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_desktop"
           gdk-app-launch-context-set-desktop) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[desktop]{an integer with the number of a workspace, or -1}
  @begin{short}
    Sets the workspace on which applications will be launched when using this
    context when running under a window manager that supports multiple
    workspaces, as described in the Extended Window Manager Hints.
  @end{short}

  When the workspace is not specified or the @arg{desktop} argument is set to
  -1, it is up to the window manager to pick one, typically it will be the
  current workspace.
  @see-class{gdk-app-launch-context}"
  (context (g-object gdk-app-launch-context))
  (desktop :int))

(export 'gdk-app-launch-context-set-desktop)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_timestamp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_timestamp"
           gdk-app-launch-context-set-timestamp) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[timestamp]{an unsigned integer with a timestamp}
  @begin{short}
    Sets the timestamp of @arg{context}.
  @end{short}
  The timestamp should ideally be taken from the event that triggered the
  launch.

  Window managers can use this information to avoid moving the focus to the
  newly launched application when the user is busy typing in another window.
  This is also known as 'focus stealing prevention'.
  @see-class{gdk-app-launch-context}"
  (context (g-object gdk-app-launch-context))
  (timestamp :uint32))

(export 'gdk-app-launch-context-set-timestamp)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon" gdk-app-launch-context-set-icon)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[icon]{a @class{g-icon} object, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with this context.
  @end{short}

  Window Managers can use this information when displaying startup
  notification. See also the @fun{gdk-app-launch-context-set-icon-name}
  function.
  @see-class{gdk-app-launch-context}
  @see-class{g-icon}
  @see-function{gdk-app-launch-context-set-icon-name}"
  (context (g-object gdk-app-launch-context))
  (icon (g-object g-icon)))

(export 'gdk-app-launch-context-set-icon)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon_name"
           gdk-app-launch-context-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[name]{a string with an icon name, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with this context.
  @end{short}
  The icon name will be interpreted in the same way as the icon field in
  desktop files. See also the @fun{gdk-app-launch-context-set-icon} function.

  If both an icon and an icon name are set, the @arg{name} argument takes
  priority. If neither an icon or an icon name is set, the icon is taken from
  either the file that is passed to launched application or from the
  @class{g-app-info} object for the launched application itself.
  @see-class{gdk-app-launch-context}
  @see-class{g-app-info}
  @see-function{gdk-app-launch-context-set-icon}"
  (context (g-object gdk-app-launch-context))
  (name :string))

(export 'gdk-app-launch-context-set-icon-name)

;;; --- End of file gdk.app.lisp -----------------------------------------------
