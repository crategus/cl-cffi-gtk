;;; ----------------------------------------------------------------------------
;;; gtk.application.lisp
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.4. See http://www.gtk.org.
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
;;;
;;; GtkApplication
;;; 
;;; Application class
;;;     
;;; Synopsis
;;; 
;;;     GtkApplication
;;;
;;;     gtk_application_new
;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GApplication
;;;          +----GtkApplication
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkApplication implements GActionGroup and GActionMap.
;;;
;;; Signals
;;; 
;;;   "window-added"                                   : Run First
;;;   "window-removed"                                 : Run First
;;; 
;;; Description
;;; 
;;; GtkApplication is a class that handles many important aspects of a GTK+
;;; application in a convenient fashion, without enforcing a one-size-fits-all
;;; application model.
;;; 
;;; Currently, GtkApplication handles GTK+ initialization, application
;;; uniqueness, provides some basic scriptability by exporting 'actions', and
;;; manages a list of toplevel windows whose life-cycle is automatically tied
;;; to the life-cycle of your application.
;;; 
;;; Example 110. A simple application
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; static void
;;; new_window (GApplication *app,
;;;             GFile        *file)
;;; {
;;;   GtkWidget *window, *scrolled, *view;
;;; 
;;;   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;   gtk_window_set_application (GTK_WINDOW (window), GTK_APPLICATION (app));
;;;   gtk_window_set_title (GTK_WINDOW (window), "Bloatpad");
;;;   scrolled = gtk_scrolled_window_new (NULL, NULL);
;;;   view = gtk_text_view_new ();
;;;   gtk_container_add (GTK_CONTAINER (scrolled), view);
;;;   gtk_container_add (GTK_CONTAINER (window), scrolled);
;;; 
;;;   if (file != NULL)
;;;     {
;;;       gchar *contents;
;;;       gsize length;
;;; 
;;;       if (g_file_load_contents (file, NULL, &contents, &length, NULL, NULL))
;;;         {
;;;           GtkTextBuffer *buffer;
;;; 
;;;           buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
;;;           gtk_text_buffer_set_text (buffer, contents, length);
;;;           g_free (contents);
;;;         }
;;;     }
;;; 
;;;   gtk_widget_show_all (GTK_WIDGET (window));
;;; }
;;; 
;;; static void
;;; bloat_pad_activate (GApplication *application)
;;; {
;;;   new_window (application, NULL);
;;; }
;;; 
;;; static void
;;; bloat_pad_open (GApplication  *application,
;;;                 GFile        **files,
;;;                 gint           n_files,
;;;                 const gchar   *hint)
;;; {
;;;   gint i;
;;; 
;;;   for (i = 0; i < n_files; i++)
;;;     new_window (application, files[i]);
;;; }
;;; 
;;; typedef GtkApplication BloatPad;
;;; typedef GtkApplicationClass BloatPadClass;
;;; 
;;; G_DEFINE_TYPE (BloatPad, bloat_pad, GTK_TYPE_APPLICATION)
;;; 
;;; static void
;;; bloat_pad_finalize (GObject *object)
;;; {
;;;   G_OBJECT_CLASS (bloat_pad_parent_class)->finalize (object);
;;; }
;;; 
;;; static void
;;; bloat_pad_init (BloatPad *app)
;;; {
;;; }
;;; 
;;; static void
;;; bloat_pad_class_init (BloatPadClass *class)
;;; {
;;;   G_OBJECT_CLASS (class)->finalize= bloat_pad_finalize;
;;; 
;;;   G_APPLICATION_CLASS (class)->activate = bloat_pad_activate;
;;;   G_APPLICATION_CLASS (class)->open = bloat_pad_open;
;;; }
;;; 
;;; BloatPad *
;;; bloat_pad_new (void)
;;; {
;;;   g_type_init ();
;;; 
;;;   return g_object_new (bloat_pad_get_type (),
;;;                        "application-id", "org.gtk.Test.bloatpad",
;;;                        "flags", G_APPLICATION_HANDLES_OPEN,
;;;                        NULL);
;;; }
;;; 
;;; int
;;; main (int argc, char **argv)
;;; {
;;;   BloatPad *bloat_pad;
;;;   int status;
;;; 
;;;   bloat_pad = bloat_pad_new ();
;;;   status = g_application_run (G_APPLICATION (bloat_pad), argc, argv);
;;;   g_object_unref (bloat_pad);
;;; 
;;;   return status;
;;; }
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-added" signal
;;; 
;;; void user_function (GtkApplication *application,
;;;                     GtkWindow      *window,
;;;                     gpointer        user_data)        : Run First
;;; 
;;; Emitted when a GtkWindow is added to application through
;;; gtk_application_add_wi!ndow().
;;; 
;;; application :
;;;     the GtkApplication which emitted the signal
;;; 
;;; window :
;;;     the newly-added GtkWindow
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-removed" signal
;;; 
;;; void user_function (GtkApplication *application,
;;;                     GtkWindow      *window,
;;;                     gpointer        user_data)        : Run First
;;; 
;;; Emitted when a GtkWindow is removed from application, either as a
;;; side-effect of being destroyed or explicitly through
;;; gtk_application_remove_window().
;;; 
;;; application :
;;;     the GtkApplication which emitted the signal
;;; 
;;; window :
;;;     the GtkWindow that is being removed
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkApplication
;;; 
;;; struct GtkApplication;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplication" gtk-application
  (:superclass g-application
   :export t
   :interfaces ("GActionGroup" "GActionMap")
   :type-initializer "gtk_application_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_application_new ()
;;; 
;;; GtkApplication * gtk_application_new (const gchar *application_id,
;;;                                       GApplicationFlags flags);
;;; 
;;; Creates a new GtkApplication instance.
;;; 
;;; This function calls g_type_init() for you. gtk_init() is called as soon as
;;; the application gets registered as the primary instance.
;;; 
;;; The application id must be valid. See g_application_id_is_valid().
;;; 
;;; application_id :
;;;     the application id
;;; 
;;; flags :
;;;     the application flags
;;; 
;;; Returns :
;;;     a new GtkApplication instance
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-application-new))

(defun gtk-application-new (application-id flags)
  (make-instance 'gtk-application
                 :application-id application-id
                 :flags flags))

(export 'gtk-application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window ()
;;; 
;;; void gtk_application_add_window (GtkApplication *application,
;;;                                  GtkWindow *window);
;;; 
;;; Adds a window from application.
;;; 
;;; This call is equivalent to setting the "application" property of window to
;;; application.
;;; 
;;; Normally, the connection between the application and the window will remain
;;; until the window is destroyed, but you can explicitly remove it with
;;; gtk_application_remove_window().
;;; 
;;; GTK+ will keep the application running as long as it has any windows.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_add_window" gtk-application-add-window) :void
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_window ()
;;; 
;;; void gtk_application_remove_window (GtkApplication *application,
;;;                                     GtkWindow *window);
;;; 
;;; Remove a window from application.
;;; 
;;; If window belongs to application then this call is equivalent to setting
;;; the "application" property of window to NULL.
;;; 
;;; The application may stop running as a result of a call to this function.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; window :
;;;     a GtkWindow
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_remove_window" gtk-application-remove-window) :void
  (application (g-object gtk-application))
  (window (g-object gtk-window)))

(export 'gtk-application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows ()
;;; 
;;; GList * gtk_application_get_windows (GtkApplication *application);
;;; 
;;; Gets a list of the GtkWindows associated with application.
;;; 
;;; The list is sorted by most recently focused window, such that the first
;;; element is the currently focused window. (Useful for choosing a parent for
;;; a transient window.)
;;; 
;;; The list that is returned should not be modified in any way. It will only
;;; remain valid until the next focus change or window creation or deletion.
;;; 
;;; application :
;;;     a GtkApplication
;;; 
;;; Returns :
;;;     a GList of GtkWindow. [element-type GtkWindow][transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_get_windows" gtk-application-get-windows)
    (g-list (g-object gtk-window))
  (application (g-object gtk-application)))

(export 'gtk-application-get-windows)

;;; --- End of file gtk.application.lisp ---------------------------------------
