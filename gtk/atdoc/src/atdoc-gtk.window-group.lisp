;;; ----------------------------------------------------------------------------
;;; gtk.window-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Documentation strings for the library GTK+.
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

(in-package :gtk)

;;; --- gtk-window-group -------------------------------------------------------

(setf (documentation 'gtk-window-group 'type)
 "@version{2013-1-31}
  @begin{short}
    GtkWindowGroup objects are referenced by each window in the group, so once
    you have added all windows to a GtkWindowGroup, you can drop the initial
    reference to the window group with g_object_unref().
  @end{short}
  If the windows in the
  window group are subsequently destroyed, then they will be removed from the
  window group and drop their references on the window group; when all window
  have been removed, the window group will be freed.")

;;; --- gtk-window-group-new ---------------------------------------------------

(setf (documentation 'gtk-window-group-new 'function)
 "@version{2013-1-31}
  @argument{A new GtkWindowGroup.}
  @begin{short}
    Creates a new GtkWindowGroup object. Grabs added with gtk_grab_add() only
    affect windows within the same GtkWindowGroup.
  @end{short}")

;;; --- gtk-window-group-add-window --------------------------------------------

(setf (documentation 'gtk-window-group-add-window 'function)
 "@version{2013-1-31}
  @argument[window_group]{a GtkWindowGroup}
  @argument[window]{the GtkWindow to add}
  @begin{short}
    Adds a window to a GtkWindowGroup.
  @end{short}")

;;; --- gtk-window-group-remove-window -----------------------------------------

(setf (documentation 'gtk-window-group-remove-window 'function)
 "@version{2013-1-31}
  @argument[window_group]{a GtkWindowGroup}
  @argument[window]{the GtkWindow to remove}
  @begin{short}
    Removes a window from a GtkWindowGroup.
  @end{short}")

;;; --- gtk-window-group-list-windows ------------------------------------------

(setf (documentation 'gtk-window-group-list-windows 'function)
 "@version{2013-1-31}
  @argument[window_group]{a GtkWindowGroup}
  @return{A newly-allocated list of windows inside the group.}
  @begin{short}
    Returns a list of the GtkWindows that belong to window_group.
  @end{short}

  Since 2.14")

;;; --- gtk-window-group-get-current-grab --------------------------------------

(setf (documentation 'gtk-window-group-get-current-grab 'function)
 "@version{2013-1-31}
  @argument[window_group]{a GtkWindowGroup}
  @return{the current grab widget of the group}
  @begin{short}
    Gets the current grab widget of the given group, see gtk_grab_add().
  @end{short}

  Since 2.22")

;;; --- gtk-window-group-get-current-device-grab -------------------------------

(setf (documentation 'gtk-window-group-get-current-device-grab 'function)
 "@version{2013-1-31}
  @argument[window_group]{a GtkWindowGroup}
  @argument[device]{a GdkDevice}
  @return{The grab widget, or NULL.}
  @begin{short}
    Returns the current grab widget for device, or NULL if none.
  @end{short}

  Since 3.0")

;;; --- End of file gtk.window-group.lisp --------------------------------------
