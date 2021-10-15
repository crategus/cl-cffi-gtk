;;; ----------------------------------------------------------------------------
;;; gio.loadable-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2021 Dieter Kaiser
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
;;; GLoadableIcon
;;;
;;;     Loadable Icons
;;;
;;; Types and Values
;;;
;;;     GLoadableIcon
;;;
;;; Functions
;;;
;;;     g_loadable_icon_load
;;;     g_loadable_icon_load_async
;;;     g_loadable_icon_load_finish
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GLoadableIcon
;;;
;;; Prerequisites
;;;
;;;     GLoadableIcon requires GIcon and GObject.
;;;
;;; Known Implementations
;;;
;;;     GLoadableIcon is implemented by GBytesIcon and GFileIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GLoadableIcon
;;; ----------------------------------------------------------------------------

(define-g-interface "GLoadableIcon" g-loadable-icon
  (:export t
   :type-initializer "g_loadable_icon_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-loadable-icon atdoc:*class-name-alias*)
      "Interface"
      (documentation 'g-loadable-icon 'type)
 "@version{2021-10-8}
  @begin{short}
    Extends the @class{g-icon} interface and adds the ability to load icons
    from streams.
  @end{short}
  @see-class{g-icon}
  @see-class{g-themed-icon}")

;;; ----------------------------------------------------------------------------
;;; g_loadable_icon_load ()
;;;
;;; GInputStream *
;;; g_loadable_icon_load (GLoadableIcon *icon,
;;;                       int size,
;;;                       char **type,
;;;                       GCancellable *cancellable,
;;;                       GError **error);
;;;
;;; Loads a loadable icon. For the asynchronous version of this function, see
;;; g_loadable_icon_load_async().
;;;
;;; icon :
;;;     a GLoadableIcon
;;;
;;; size :
;;;     an integer
;;;
;;; type :
;;;     a location to store the type of the loaded icon, NULL to ignore
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore
;;;
;;; Returns :
;;;     A GInputStream to read the icon from.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_loadable_icon_load_async ()
;;;
;;; void
;;; g_loadable_icon_load_async (GLoadableIcon *icon,
;;;                             int size,
;;;                             GCancellable *cancellable,
;;;                             GAsyncReadyCallback callback,
;;;                             gpointer user_data);
;;;
;;; Loads an icon asynchronously. To finish this function, see
;;; g_loadable_icon_load_finish(). For the synchronous, blocking version of this
;;; function, see g_loadable_icon_load().
;;;
;;; icon :
;;;     a GLoadableIcon
;;;
;;; size :
;;;     an integer
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; callback .
;;;     a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to callback function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_loadable_icon_load_finish ()
;;;
;;; GInputStream *
;;; g_loadable_icon_load_finish (GLoadableIcon *icon,
;;;                              GAsyncResult *res,
;;;                              char **type,
;;;                              GError **error);
;;;
;;; Finishes an asynchronous icon load started in g_loadable_icon_load_async().
;;;
;;; icon :
;;;     a GLoadableIcon
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; type :
;;;     a location to store the type of the loaded icon, NULL to ignore
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore
;;;
;;; Returns :
;;;     A GInputStream to read the icon from.
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.loadable-icon.lisp -------------------------------------
