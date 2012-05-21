;;; ----------------------------------------------------------------------------
;;; gtk.offscreen-window.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkOffscreenWindow
;;; 
;;; A toplevel to manage offscreen rendering of child widgets
;;;     
;;; Synopsis
;;; 
;;;     GtkOffscreenWindow
;;;     
;;;     gtk_offscreen_window_new
;;;     gtk_offscreen_window_get_surface
;;;     gtk_offscreen_window_get_pixbuf
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkOffscreenWindow
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkOffscreenWindow implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Description
;;; 
;;; GtkOffscreenWindow is strictly intended to be used for obtaining snapshots
;;; of widgets that are not part of a normal widget hierarchy. Since
;;; GtkOffscreenWindow is a toplevel widget you cannot obtain snapshots of a
;;; full window with it since you cannot pack a toplevel widget in another
;;; toplevel.
;;; 
;;; The idea is to take a widget and manually set the state of it, add it to a
;;; GtkOffscreenWindow and then retrieve the snapshot as a cairo_surface_t or
;;; GdkPixbuf.
;;; 
;;; GtkOffscreenWindow derives from GtkWindow only as an implementation detail.
;;; Applications should not use any API specific to GtkWindow to operate on this
;;; object. It should be treated as a GtkBin that has no parent widget.
;;; 
;;; When contained offscreen widgets are redrawn, GtkOffscreenWindow will emit a
;;; "damage-event" signal.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkOffscreenWindow
;;; 
;;; struct GtkOffscreenWindow;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkOffscreenWindow" gtk-offscreen-window
  (:superclass g-object
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_offscreen_window_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_new ()
;;; 
;;; GtkWidget * gtk_offscreen_window_new (void);
;;; 
;;; Creates a toplevel container widget that is used to retrieve snapshots of
;;; widgets without showing them on the screen.
;;; 
;;; Returns :
;;;     A pointer to a GtkWidget
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-offscreen-window-new))

(defun gtk-offscreen-window-new ()
  (make-instance 'gtk-offscreen-window))

(export 'gtk-offscreen-window)

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

;;; --- End of file gtk.offscreen-window.lisp ----------------------------------
