;;; ----------------------------------------------------------------------------
;;; gtk.window-group.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Limit the effect of grabs
;;;
;;; Types and Values
;;;
;;;     GtkWindowGroup
;;;
;;; Functions
;;;
;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkWindowGroup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkWindowGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkWindowGroup" gtk-window-group
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_window_group_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-window-group 'type)
 "@version{*2020-5-29}
  @begin{short}
    A @sym{gtk-window-group} restricts the effect of grabs to windows in the
    same group, thereby making window groups almost behave like separate
    applications.
  @end{short}

  A window can be a member in at most one window group at a time. Windows that
  have not been explicitly assigned to a group are implicitly treated like
  windows of the default window group.

  @sym{gtk-window-group} objects are referenced by each window in the group, so
  once you have added all windows to a @sym{gtk-window-group}, you can drop the
  initial reference to the window group with the @fun{g-object-unref} function.
  If the windows in the window group are subsequently destroyed, then they will
  be removed from the window group and drop their references on the window
  group. When all window have been removed, the window group will be freed.")

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-group-new))

(defun gtk-window-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-29}
  @return{A new @class{gtk-window-group} object.}
  @begin{short}
    Creates a new window group.
  @end{short}
  Grabs added with the function @fun{gtk-grab-add} only affect windows within
  the same window group.
  @see-class{gtk-window-group}
  @see-function{gtk-grap-add}"
  (make-instance 'gtk-window-group))

(export 'gtk-window-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_add_window ()
;;; ----------------------------------------------------------------------------

(defcfun (gtk-window-group-add-window "gtk_window_group_add_window") :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @argument[window]{the @class{gtk-window} widget to add}
  @begin{short}
    Adds a window to a window group.
  @end{short}
  @see-class{gtk-window-group}
  @see-class{gtk-window}"
  (window-group (g-object gtk-window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_remove_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_remove_window" gtk-window-group-remove-window) :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @argument[window]{the @class{gtk-window} widget to remove}
  @begin{short}
    Removes a window from a window group.
  @end{short}
  @see-class{gtk-window-group}
  @see-class{gtk-window}"
  (window-group (g-object gtk-window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_list_windows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_list_windows" gtk-window-group-list-windows)
    (g-list gtk-window)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @return{A newly allocated list of windows inside the group.}
  @begin{short}
    Returns a list of windows that belong to the window group.
  @end{short}
  @see-class{gtk-window-group}
  @see-class{gtk-window}"
  (window-group (g-object gtk-window-group)))

(export 'gtk-window-group-list-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_grab () -> gtk-window-group-current-grab
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_get_current_grab" gtk-window-group-current-grab)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @return{The current @class{gtk-widget} grab widget of the group.}
  @begin{short}
    Gets the current grab widget of the given group.
  @end{short}
  See the function @fun{gtk-grab-add}.
  @see-class{gtk-window-group}
  @see-class{gtk-widget}
  @see-function{gtk-grab-add}"
  (window-group (g-object gtk-window-group)))

(export 'gtk-window-group-current-grab)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_device_grab ()
;;; -> gtk-window-group-current-device-grab
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_get_current_device_grab"
           gtk-window-group-current-device-grab)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @argument[device]{a @class{gdk-device} object}
  @return{The @class{gtk-widget} grab widget, or @code{nil}.}
  @begin{short}
    Returns the current grab widget for @arg{device}, or @code{nil} if none.
  @end{short}
  @see-class{gtk-window-group}
  @see-class{gdk-device}
  @see-class{gtk-widget}"
  (window-group (g-object gtk-window-group))
  (device (g-object gdk-device)))

(export 'gtk-window-group-current-device-grab)

;;; --- End of file gtk.window-group.lisp --------------------------------------
