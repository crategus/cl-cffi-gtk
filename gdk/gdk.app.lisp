;;; ----------------------------------------------------------------------------
;;; gdk.app.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-app-launch-context 'type)
 "@version{2013-4-6}
  @begin{short}
    @sym{gdk-app-launch-context} is an implementation of
    @class{g-app-launch-context} that handles launching an application in a
    graphical context. It provides startup notification and allows to launch
    applications on a specific screen or workspace.
  @end{short}

  @b{Example:} Launching an application
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
  @see-slot{gdk-app-launch-context-display}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display"
                                               'gdk-app-launch-context) 't)
 "The @code{\"display\"} property of type @class{gdk-display}
  (Read / Write / Construct)@br{}
  Display.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-app-launch-context-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-app-launch-context-display 'function)
 "@version{2013-4-6}
  Accessor of the slot @code{\"display\"} of the @class{gdk-app-launch-context}
  class.")

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_new" gdk-app-launch-context-new)
    (g-object gdk-app-launch-context)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @return{A new @class{gdk-app-launch-context} object.}
  @subheading{Warning}
    @sym{gdk-app-launch-context-new} has been deprecated since version 3.0 and
    should not be used in newly-written code.
    Use @fun{gdk-display-get-app-launch-context} instead.

  @begin{short}
    Creates a new @class{gdk-app-launch-context} object.
  @end{short}

  Since 2.14
  @see-function{gdk-display-get-app-launch-context}")

(export 'gdk-app-launch-context-new)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_display ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-app-launch-context-set-display))

(defun gdk-app-launch-context-set-display (context display)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[display]{a @class{gdk-display} object}
  @subheading{Warning}
    @sym{gdk-app-launch-context-set-display} has been deprecated since version
    3.0 and should not be used in newly-written code. Use
    @fun{gdk-display-get-app-launch-context} instead.

  @begin{short}
    Sets the display on which applications will be launched when using this
    context. See also @fun{gdk-app-launch-context-set-screen}.
  @end{short}

  Since 2.14
  @see-function{gdk-display-get-app-launch-context}
  @see-function{gdk-app-launch-context-set-screen}"
  (setf (gdk-app-launch-context-display context) display))

(export 'gdk-app-launch-context-set-display)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_screen" gdk-app-launch-context-set-screen)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Sets the screen on which applications will be launched when using this
    context. See also @fun{gdk-app-launch-context-set-display}.
  @end{short}

  If both screen and display are set, the screen takes priority. If neither
  screen or display are set, the default screen and display are used.

  Since 2.14
  @see-function{gdk-app-launch-context-set-display}"
  (context (g-object gdk-app-launch-context))
  (screen (g-object gdk-screen)))

(export 'gdk-app-launch-context-set-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_desktop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_desktop"
           gdk-app-launch-context-set-desktop) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[desktop]{the number of a workspace, or -1}
  @begin{short}
    Sets the workspace on which applications will be launched when using this
    context when running under a window manager that supports multiple
    workspaces, as described in the Extended Window Manager Hints.
  @end{short}

  When the workspace is not specified or desktop is set to -1, it is up to the
  window manager to pick one, typically it will be the current workspace.

  Since 2.14"
  (context (g-object gdk-app-launch-context))
  (desktop :int))

(export 'gdk-app-launch-context-set-desktop)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_timestamp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_timestamp"
           gdk-app-launch-context-set-timestamp) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[timestamp]{a timestamp}
  @begin{short}
    Sets the @arg{timestamp} of @arg{context}. The @arg{timestamp} should
    ideally be taken from the event that triggered the launch.
  @end{short}

  Window managers can use this information to avoid moving the focus to the
  newly launched application when the user is busy typing in another window.
  This is also known as 'focus stealing prevention'.

  Since 2.14"
  (context (g-object gdk-app-launch-context))
  (timestamp :uint32))

(export 'gdk-app-launch-context-set-timestamp)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon" gdk-app-launch-context-set-icon)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[icon]{a @class{g-icon} object, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with this context.
  @end{short}

  Window Managers can use this information when displaying startup
  notification.

  See also @fun{gdk-app-launch-context-set-icon-name}.

  Since 2.14
  @see-function{gdk-app-launch-context-set-icon-name}"
  (context (g-object gdk-app-launch-context))
  (icon :pointer)) ; TODO: Implement GIcon

(export 'gdk-app-launch-context-set-icon)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon_name"
           gdk-app-launch-context-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-6}
  @argument[context]{a @class{gdk-app-launch-context} object}
  @argument[icon-name]{an icon name, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with this context. The
    @arg{icon-name} will be interpreted in the same way as the Icon field in
    desktop files. See also @fun{gdk-app-launch-context-set-icon}.
  @end{short}

  If both icon and @arg{icon-name} are set, the @arg{icon-name} takes priority.
  If neither icon or @arg{icon-name} is set, the icon is taken from either the
  file that is passed to launched application or from the @code{GAppInfo} for
  the launched application itself.

  Since 2.14
  @see-function{gdk-app-launch-context-set-icon}"
  (context (g-object gdk-app-launch-context))
  (icon-name :string))

(export 'gdk-app-launch-context-set-icon-name)

;;; --- End of file gdk.app.lisp -----------------------------------------------
