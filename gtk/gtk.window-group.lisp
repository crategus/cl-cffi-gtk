;;; ----------------------------------------------------------------------------
;;; gtk.window-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;;
;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab
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
 "@version{2013-3-28}
  @begin{short}
    @sym{gtk-window-group} objects are referenced by each window in the group,
    so once you have added all windows to a @sym{gtk-window-group}, you can drop
    the initial reference to the window group with @fun{g-object-unref}.
  @end{short}
  If the windows in the window group are subsequently destroyed, then they will
  be removed from the window group and drop their references on the window
  group; when all window have been removed, the window group will be freed.")

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-window-group-new))

(defun gtk-window-group-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @return{A new @class{gtk-window-group} object.}
  Creates a new @class{gtk-window-group} object. Grabs added with
  @fun{gtk-grab-add} only affect windows within the same
  @class{gtk-window-group}."
  (make-instance 'gtk-window-group))

(export 'gtk-window-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_add_window ()
;;; ----------------------------------------------------------------------------

(defcfun (gtk-window-group-add-window "gtk_window_group_add_window") :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @argument[window]{the @class{gtk-window} to add}
  @begin{short}
    Adds a window to a @class{gtk-window-group} object.
  @end{short}"
  (window-group (g-object gtk-window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_remove_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_remove_window" gtk-window-group-remove-window) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @argument[window]{the @class{gtk-window} to remove}
  Removes a window from a @class{gtk-window-group} object."
  (window-group (g-object gtk-window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_list_windows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_list_windows" gtk-window-group-list-windows)
    (g-list gtk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @return{A newly allocated list of windows inside the group.}
  @begin{short}
    Returns a list of the @class{gtk-window} windows that belong to
    @arg{window-group}.
  @end{short}

  Since 2.14"
  (window-group (g-object gtk-window-group)))

(export 'gtk-window-group-list-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_get_current_grab" gtk-window-group-get-current-grab)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @return{The current grab widget of the group.}
  @begin{short}
    Gets the current grab widget of the given group, see @fun{gtk-grab-add}.
  @end{short}

  Since 2.22"
  (window-group (g-object gtk-window-group)))

(export 'gtk-window-group-get-current-grab)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_device_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_get_current_device_grab"
           gtk-window-group-get-current-device-grab)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-29}
  @argument[window-group]{a @class{gtk-window-group} object}
  @argument[device]{a @class{gdk-device} object}
  @return{The grab widget, or @code{nil}.}
  @begin{short}
    Returns the current grab widget for @arg{device}, or @code{nil} if none.
  @end{short}

  Since 3.0"
  (window-group (g-object gtk-window-group))
  (device (g-object gdk-device)))

(export 'gtk-window-group-get-current-device-grab)

;;; --- End of file gtk.window-group.lisp --------------------------------------
