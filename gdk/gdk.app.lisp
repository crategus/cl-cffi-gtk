;;; ----------------------------------------------------------------------------
;;; gdk.app.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
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
;;;
;;; Application launching
;;;
;;; Startup notification for applications
;;;
;;; Synopsis
;;;
;;;     GdkAppLaunchContext
;;;
;;;     gdk_app_launch_context_new                         * deprecated *
;;;     gdk_app_launch_context_set_display                 * deprecated *
;;;     gdk_app_launch_context_set_screen
;;;     gdk_app_launch_context_set_desktop
;;;     gdk_app_launch_context_set_timestamp
;;;     gdk_app_launch_context_set_icon
;;;     gdk_app_launch_context_set_icon_name
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GAppLaunchContext
;;;          +----GdkAppLaunchContext
;;;
;;; Properties
;;;
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; Description
;;;
;;; GdkAppLaunchContext is an implementation of GAppLaunchContext that handles
;;; launching an application in a graphical context. It provides startup
;;; notification and allows to launch applications on a specific screen or
;;; workspace.
;;;
;;; Example 6. Launching an application
;;;
;;;   GdkAppLaunchContext *context;
;;;
;;;   context = gdk_display_get_app_launch_context (display);
;;;
;;;   gdk_app_launch_context_set_screen (screen);
;;;   gdk_app_launch_context_set_timestamp (event->time);
;;;
;;;   if (!g_app_info_launch_default_for_uri ("http://www.gtk.org",
;;;                                           context, &error))
;;;     g_warning ("Launching failed: %s\n", error->message);
;;;
;;;   g_object_unref (context);
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "display" property
;;;
;;;   "display"                  GdkDisplay*          : Read / Write / Construct
;;;
;;; Display.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAppLaunchContext
;;;
;;; typedef struct _GdkAppLaunchContext GdkAppLaunchContext;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkAppLaunchContext" gdk-app-launch-context
  (:superclass g-app-launch-context
   :export t
   :interfaces nil
   :type-initializer "gdk_app_launch_context_get_type")
  ((display
    gdk-app-launch-context-display
    "display" "GdkDisplay" t t)))

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_new ()
;;;
;;; GdkAppLaunchContext * gdk_app_launch_context_new (void);
;;;
;;; Warning
;;;
;;; gdk_app_launch_context_new has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gdk_display_get_app_launch_context()
;;; instead
;;;
;;; Creates a new GdkAppLaunchContext.
;;;
;;; Returns :
;;;     a new GdkAppLaunchContext
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_display ()
;;;
;;; void gdk_app_launch_context_set_display (GdkAppLaunchContext *context,
;;;                                          GdkDisplay *display);
;;;
;;; Warning
;;;
;;; gdk_app_launch_context_set_display has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gdk_display_get_app_launch_context() instead
;;;
;;; Sets the display on which applications will be launched when using this
;;; context. See also gdk_app_launch_context_set_screen().
;;;
;;; context :
;;;     a GdkAppLaunchContext
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-app-launch-context-set-display))

(defun gdk-app-launch-context-set-display (context display)
  (setf (gdk-app-launch-context-display context) display))

(export 'gdk-app-launch-context-set-display)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_screen ()
;;;
;;; void gdk_app_launch_context_set_screen (GdkAppLaunchContext *context,
;;;                                         GdkScreen *screen);
;;;
;;; Sets the screen on which applications will be launched when using this
;;; context. See also gdk_app_launch_context_set_display().
;;;
;;; If both screen and display are set, the screen takes priority. If neither
;;; screen or display are set, the default screen and display are used.
;;;
;;; context :
;;;     a GdkAppLaunchContext
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_screen" gdk-app-launch-context-set-screen)
    :void
  (context (g-object gdk-app-launch-context))
  (screen (g-object gdk-screen)))

(export 'gdk-app-launch-context-set-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_desktop ()
;;;
;;; void gdk_app_launch_context_set_desktop (GdkAppLaunchContext *context,
;;;                                          gint desktop);
;;;
;;; Sets the workspace on which applications will be launched when using this
;;; context when running under a window manager that supports multiple
;;; workspaces, as described in the Extended Window Manager Hints.
;;;
;;; When the workspace is not specified or desktop is set to -1, it is up to the
;;; window manager to pick one, typically it will be the current workspace.
;;;
;;; context :
;;;     a GdkAppLaunchContext
;;;
;;; desktop :
;;;     the number of a workspace, or -1
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_desktop"
           gdk-app-launch-context-set-desktop) :void
  (context (g-object gdk-app-launch-context))
  (desktop :int))

(export 'gdk-app-launch-context-set-desktop)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_timestamp ()
;;;
;;; void gdk_app_launch_context_set_timestamp (GdkAppLaunchContext *context,
;;;                                            guint32 timestamp);
;;;
;;; Sets the timestamp of context. The timestamp should ideally be taken from
;;; the event that triggered the launch.
;;;
;;; Window managers can use this information to avoid moving the focus to the
;;; newly launched application when the user is busy typing in another window.
;;; This is also known as 'focus stealing prevention'.
;;;
;;; context :
;;;     a GdkAppLaunchContext
;;;
;;; timestamp :
;;;     a timestamp
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_timestamp"
           gdk-app-launch-context-set-timestamp) :void
  (context (g-object gdk-app-launch-context))
  (timestamp :uint32))

(export 'gdk-app-launch-context-set-timestamp)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon ()
;;;
;;; void gdk_app_launch_context_set_icon (GdkAppLaunchContext *context,
;;;                                       GIcon *icon);
;;;
;;; Sets the icon for applications that are launched with this context.
;;;
;;; Window Managers can use this information when displaying startup
;;; notification.
;;;
;;; See also gdk_app_launch_context_set_icon_name().
;;;
;;; context :
;;;     a GdkAppLaunchContext
;;;
;;; icon :
;;;     a GIcon, or NULL
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon" gdk-app-launch-context-set-icon)
    :void
  (context (g-object gdk-app-launch-context))
  (icon :pointer)) ; TODO: Implement GIcon

(export 'gdk-app-launch-context-set-icon)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon_name ()
;;;
;;; void gdk_app_launch_context_set_icon_name (GdkAppLaunchContext *context,
;;;                                            const char *icon_name);
;;;
;;; Sets the icon for applications that are launched with this context. The
;;; icon_name will be interpreted in the same way as the Icon field in desktop
;;; files. See also gdk_app_launch_context_set_icon().
;;;
;;; If both icon and icon_name are set, the icon_name takes priority. If neither
;;; icon or icon_name is set, the icon is taken from either the file that is
;;; passed to launched application or from the GAppInfo for the launched
;;; application itself.
;;;
;;; context :
;;;     a GdkAppLaunchContext
;;;
;;; icon_name :
;;;     an icon name, or NULL
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon_name"
           gdk-app-launch-context-set-icon-name) :void
  (context (g-object gdk-app-launch-context))
  (icon-name :string))

(export 'gdk-app-launch-context-set-icon-name)

;;; --- End of file gdk.app.lisp -----------------------------------------------
