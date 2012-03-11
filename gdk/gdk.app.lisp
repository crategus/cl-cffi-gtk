;;; ----------------------------------------------------------------------------
;;; gdk.app.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
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
;;;     gdk_app_launch_context_new
;;;     gdk_app_launch_context_set_display
;;;     gdk_app_launch_context_set_screen
;;;     gdk_app_launch_context_set_desktop
;;;     gdk_app_launch_context_set_timestamp
;;;     gdk_app_launch_context_set_icon
;;;     gdk_app_launch_context_set_icon_name
;;; 
;;; Description
;;; 
;;; GdkAppLaunchContext is an implementation of GAppLaunchContext that handles
;;; launching an application in a graphical context. It provides startup
;;; notification and allows to launch applications on a specific screen or
;;; workspace.
;;; 
;;; Example 9. Launching an application
;;; 
;;; GdkAppLaunchContext *context;
;;; context = gdk_app_launch_context_new ();
;;; gdk_app_launch_context_set_screen (my_screen);
;;; gdk_app_launch_context_set_timestamp (event->time);
;;; if (!g_app_info_launch_default_for_uri("http://www.gtk.org",context,&error))
;;;   g_warning ("Launching failed: %s\n", error->message);
;;; g_object_unref (context);
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAppLaunchContext
;;; 
;;; typedef struct GdkAppLaunchContext GdkAppLaunchContext;
;;; 
;;; An opaque structure representing an application launch context.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkAppLaunchContext" gdk-app-launch-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_app_launch_context_get_type")
  ((:cffi display
          gdk-app-launch-context-display (g-object gdk-display)
          nil "gdk_app_launch_context_set_display")
   (:cffi screen
          gdk-app-launch-context-screen (g-object gdk-screen)
          nil "gdk_app_launch_context_set_screen")
   (:cffi desktop
          gdk-app-launch-context-desktop :int
          nil "gdk_app_launch_context_set_desktop")
   (:cffi timestamp
          gdk-app-launch-context-timestamp :uint32
          nil "gdk_app_launch_context_set_timestamp")
   (:cffi icon
          gdk-app-launch-context-icon g-object
          nil "gdk_app_launch_context_set_icon")
   (:cffi icon-name
          gdk-app-launch-context-icon-name :string
          nil "gdk_app_launch_context_set_icon_name"))) ;; TODO: GIcon

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_new ()
;;; 
;;; GdkAppLaunchContext * gdk_app_launch_context_new (void);
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
;;; When the workspace is not specified or desktop is set to -1, it is up to
;;; the window manager to pick one, typically it will be the current workspace.
;;; 
;;; context :
;;;     a GdkAppLaunchContext
;;; 
;;; desktop :
;;;     the number of a workspace, or -1
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

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
;;; If both icon and icon_name are set, the icon_name takes priority. If
;;; neither icon or icon_name is set, the icon is taken from either the file
;;; that is passed to launched application or from the GAppInfo for the
;;; launched application itself.
;;; 
;;; context :
;;;     a GdkAppLaunchContext
;;; 
;;; icon_name :
;;;     an icon name, or NULL
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gdk.app.lisp -----------------------------------------------
