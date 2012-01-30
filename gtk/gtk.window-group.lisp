;;; ----------------------------------------------------------------------------
;;; gtk.window-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkWindowGroup
;;; 
;;; Limit the effect of grabs
;;; 
;;; Synopsis
;;; 
;;;     GtkWindowGroup
;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkWindowGroup
;;; 
;;; Description
;;; 
;;; GtkWindowGroup objects are referenced by each window in the group, so once
;;; you have added all windows to a GtkWindowGroup, you can drop the initial
;;; reference to the window group with g_object_unref(). If the windows in the
;;; window group are subsequently destroyed, then they will be removed from the
;;; window group and drop their references on the window group; when all window
;;; have been removed, the window group will be freed.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkWindowGroup
;;; 
;;; struct GtkWindowGroup;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkWindowGroup" gtk-window-group
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_window_group_get_type")
  ((:cffi windows gtk-window-group-windows (g-list (g-object gtk-window))
          "gtk_window_group_list_windows" nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_new ()
;;; 
;;; GtkWindowGroup * gtk_window_group_new (void)
;;; 
;;; Creates a new GtkWindowGroup object. Grabs added with gtk_grab_add() only
;;; affect windows within the same GtkWindowGroup.
;;; 
;;; Returns :
;;;     a new GtkWindowGroup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_add_window ()
;;; 
;;; void gtk_window_group_add_window (GtkWindowGroup *window_group,
;;;                                   GtkWindow *window)
;;; 
;;; Adds a window to a GtkWindowGroup.
;;; 
;;; window_group :
;;;     a GtkWindowGroup
;;; 
;;; window :
;;;     the GtkWindow to add
;;; ----------------------------------------------------------------------------

(defcfun (gtk-window-group-add-window "gtk_window_group_add_window") :void
  (window-group (g-object gtk-window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_remove_window ()
;;; 
;;; void gtk_window_group_remove_window (GtkWindowGroup *window_group,
;;;                                      GtkWindow *window)
;;; 
;;; Removes a window from a GtkWindowGroup.
;;; 
;;; window_group :
;;;     a GtkWindowGroup
;;; 
;;; window :
;;;     the GtkWindow to remove
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_remove_window" gtk-window-group-remove-window) :void
  (window-group (g-object gtk-window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_list_windows ()
;;; 
;;; GList * gtk_window_group_list_windows (GtkWindowGroup *window_group)
;;; 
;;; Returns a list of the GtkWindows that belong to window_group.
;;; 
;;; window_group :
;;;     a GtkWindowGroup
;;; 
;;; Returns :
;;;     A newly-allocated list of windows inside the group.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_list_windows" gtk-window-group-list-windows)
    (g-list gtk-window)
  (window-group (g-object gtk-window-group)))

(export 'gtk-window-group-list-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_grab ()
;;; 
;;; GtkWidget* gtk_window_group_get_current_grab (GtkWindowGroup *window_group)
;;; 
;;; Gets the current grab widget of the given group, see gtk_grab_add().
;;; 
;;; window_group :
;;;     a GtkWindowGroup
;;; 
;;; Returns :
;;;     the current grab widget of the group. [transfer none]
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_device_grab ()
;;; 
;;; GtkWidget * gtk_window_group_get_current_device_grab
;;;                                               (GtkWindowGroup *window_group,
;;;                                                GdkDevice *device)
;;; 
;;; Returns the current grab widget for device, or NULL if none.
;;; 
;;; window_group :
;;;     a GtkWindowGroup
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; Returns :
;;;     The grab widget, or NULL. [transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; -- End of file gtk.window-group.lisp ---------------------------------------
