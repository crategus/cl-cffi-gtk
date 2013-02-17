;;; ----------------------------------------------------------------------------
;;; gtk.mount-operation.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See http://www.gtk.org The API documentation of the
;;; Lisp binding is available at http://www.crategus.com/books/cl-cffi-gtk/
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; Filesystem utilities
;;;
;;; Functions for working with GIO
;;;
;;; Synopsis
;;;
;;;     GtkMountOperation
;;;     gtk_mount_operation_new
;;;     gtk_mount_operation_is_showing
;;;     gtk_mount_operation_set_parent
;;;     gtk_mount_operation_get_parent
;;;     gtk_mount_operation_set_screen
;;;     gtk_mount_operation_get_screen
;;;     gtk_show_uri
;;;
;;; Properties
;;;
;;;   "is-showing"               gboolean              : Read
;;;   "parent"                   GtkWindow*            : Read / Write
;;;   "screen"                   GdkScreen*            : Read / Write
;;;
;;; Description
;;;
;;; The functions and objects described here make working with GTK+ and GIO more
;;; convenient.
;;;
;;; GtkMountOperation is needed when mounting volumes: It is an implementation
;;; of GMountOperation that can be used with GIO functions for mounting volumes
;;; such as g_file_mount_enclosing_volume(), g_file_mount_mountable(),
;;; g_volume_mount(), g_mount_unmount_with_operation() and others.
;;;
;;; When necessary, GtkMountOperation shows dialogs to ask for passwords,
;;; questions or show processes blocking unmount.
;;;
;;; gtk_show_uri() is a convenient way to launch applications for URIs.
;;;
;;; Another object that is worth mentioning in this context is
;;; GdkAppLaunchContext, which provides visual feedback when lauching
;;; applications.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-showing" property
;;;
;;;  "is-showing"               gboolean              : Read
;;;
;;; Are we showing a dialog.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "parent" property
;;;
;;;   "parent"                   GtkWindow*            : Read / Write
;;;
;;; The parent window.
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen" property
;;;
;;;   "screen"                   GdkScreen*            : Read / Write
;;;
;;; The screen where this window will be displayed.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMountOperation
;;;
;;; struct GtkMountOperation;
;;;
;;; This should not be accessed directly. Use the accessor functions below.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_new ()
;;;
;;; GMountOperation * gtk_mount_operation_new (GtkWindow *parent);
;;;
;;; Creates a new GtkMountOperation
;;;
;;; parent :
;;;    transient parent of the window, or NULL.
;;;
;;; Returns :
;;;    a new GtkMountOperation
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_is_showing ()
;;;
;;; gboolean gtk_mount_operation_is_showing (GtkMountOperation *op);
;;;
;;; Returns whether the GtkMountOperation is currently displaying a window.
;;;
;;; op :
;;;    a GtkMountOperation
;;;
;;; Returns :
;;;    TRUE if op is currently displaying a window
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_set_parent ()
;;;
;;; voidgtk_mount_operation_set_parent (GtkMountOperation *op,
;;;                                     GtkWindow *parent);
;;;
;;; Sets the transient parent for windows shown by the GtkMountOperation.
;;;
;;; op :
;;;    a GtkMountOperation
;;;
;;; parent :
;;;    transient parent of the window, or NULL.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_get_parent ()
;;;
;;; GtkWindow * gtk_mount_operation_get_parent (GtkMountOperation *op);
;;;
;;; Gets the transient parent used by the GtkMountOperation
;;;
;;; op :
;;;    a GtkMountOperation
;;;
;;; Returns :
;;;    the transient parent for windows shown by op.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_set_screen ()
;;;
;;; void gtk_mount_operation_set_screen (GtkMountOperation *op,
;;;                                      GdkScreen *screen);
;;;
;;; Sets the screen to show windows of the GtkMountOperation on.
;;;
;;; op :
;;;    a GtkMountOperation
;;;
;;; screen :
;;;    a GdkScreen
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_get_screen ()
;;;
;;; GdkScreen * gtk_mount_operation_get_screen (GtkMountOperation *op);
;;;
;;; Gets the screen on which windows of the GtkMountOperation will be shown.
;;;
;;; op :
;;;    a GtkMountOperation
;;;
;;; Returns :
;;;    the screen on which windows of op are shown.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_show_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_show_uri" %gtk-show-uri) :boolean
  (screen (g-object gdk-screen))
  (uri :string)
  (timestamp :uint32)
  (error :pointer))

(defun gtk-show-uri (screen uri timestamp)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-8}
  @argument[screen]{screen to show the uri on or @code{nil} for the default
    screen.}
  @argument[uri]{the uri to show}
  @argument[timestamp]{a timestamp to prevent focus stealing}
  @return{@arg{true} on success, @code{nil} on error}
  @begin{short}
    This is a convenience function for launching the default application to show
    the uri.
  @end{short}
  The @arg{uri} must be of a form understood by GIO (i.e. you need to install
  @code{gvfs} to get support for uri schemes such as @code{http://} or
  @code{ftp://}, as only local files are handled by GIO itself). Typical
  examples are @code{file:///home/gnome/pict.jpg},
  @code{http://www.gnome.org mailto:me@@gnome.org}.

  Ideally the @arg{timestamp} is taken from the event triggering the
  @sym{gtk-show-uri} call. If @arg{timestamp} is not known you can take
  @variable{+gdk-current-time+}.

  This function can be used as a replacement for @code{gnome_vfs_url_show()} and
  @code{gnome_url_show()}.

  Since 2.14"
  (with-g-error (err)
    (%gtk-show-uri screen uri timestamp err)))

(export 'gtk-show-uri)

;;; --- End of file gtk-mount-operation.lisp -----------------------------------
