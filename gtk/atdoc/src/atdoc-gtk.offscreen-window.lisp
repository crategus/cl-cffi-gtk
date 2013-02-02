;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.offscreen-window.lisp
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

;;; --- gtk-offscreen-window ---------------------------------------------------

(setf (documentation 'gtk-offscreen-window 'type)
 "@version{2013-2-1}
  @begin{short}
    GtkOffscreenWindow is strictly intended to be used for obtaining snapshots
    of widgets that are not part of a normal widget hierarchy.
  @end{short}
  Since GtkOffscreenWindow is a toplevel widget you cannot obtain snapshots of a
  full window with it since you cannot pack a toplevel widget in another
  toplevel.

  The idea is to take a widget and manually set the state of it, add it to a
  GtkOffscreenWindow and then retrieve the snapshot as a cairo_surface_t or
  GdkPixbuf.

  GtkOffscreenWindow derives from GtkWindow only as an implementation detail.
  Applications should not use any API specific to GtkWindow to operate on this
  object. It should be treated as a GtkBin that has no parent widget.

  When contained offscreen widgets are redrawn, GtkOffscreenWindow will emit a
  \"damage-event\" signal.")

;;; --- gtk-offscreen-window-new -----------------------------------------------

(setf (documentation 'gtk-offscreen-window-new 'function)
 "@version{2013-2-1}
  @return{a GtkWidget}
  @begin{short}
    Creates a toplevel container widget that is used to retrieve snapshots of
    widgets without showing them on the screen.
  @end{short}

  Since 2.20")

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_get_surface ()
;;; 
;;; cairo_surface_t * gtk_offscreen_window_get_surface
;;;                                             (GtkOffscreenWindow *offscreen);
;;; 
;;; Retrieves a snapshot of the contained widget in the form of a
;;; cairo_surface_t. If you need to keep this around over window resizes then
;;; you should add a reference to it.
;;; 
;;; offscreen :
;;;     the GtkOffscreenWindow contained widget.
;;; 
;;; Returns :
;;;     A cairo_surface_t pointer to the offscreen surface, or NULL.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_get_pixbuf ()
;;; 
;;; GdkPixbuf * gtk_offscreen_window_get_pixbuf (GtkOffscreenWindow *offscreen);
;;; 
;;; Retrieves a snapshot of the contained widget in the form of a GdkPixbuf.
;;; This is a new pixbuf with a reference count of 1, and the application should
;;; unreference it once it is no longer needed.
;;; 
;;; offscreen :
;;;     the GtkOffscreenWindow contained widget.
;;; 
;;; Returns :
;;;     A GdkPixbuf pointer, or NULL.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- End of file atdoc-gtk.offscreen-window.lisp ----------------------------
