;;; ----------------------------------------------------------------------------
;;; gtk.viewport.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkViewport
;;; 
;;; An adapter which makes widgets scrollable
;;; 
;;; Synopsis
;;; 
;;;     GtkViewport
;;;
;;;     gtk_viewport_new
;;;     gtk_viewport_get_hadjustment
;;;     gtk_viewport_get_vadjustment
;;;     gtk_viewport_set_hadjustment
;;;     gtk_viewport_set_vadjustment
;;;     gtk_viewport_set_shadow_type
;;;     gtk_viewport_get_shadow_type
;;;     gtk_viewport_get_bin_window
;;;     gtk_viewport_get_view_window
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkViewport
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkViewport implements AtkImplementorIface, GtkBuildable and GtkScrollable.
;;;
;;; Properties
;;; 
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkViewport widget acts as an adaptor class, implementing scrollability
;;; for child widgets that lack their own scrolling capabilities. Use
;;; GtkViewport to scroll child widgets such as GtkTable, GtkBox, and so on.
;;; 
;;; If a widget has native scrolling abilities, such as GtkTextView, GtkTreeView
;;; or GtkIconview, it can be added to a GtkScrolledWindow with
;;; gtk_container_add(). If a widget does not, you must first add the widget to
;;; a GtkViewport, then add the viewport to the scrolled window. The convenience
;;; function gtk_scrolled_window_add_with_viewport() does exactly this, so you
;;; can ignore the presence of the viewport.
;;; 
;;; The GtkViewport will start scrolling content only if allocated less than the
;;; child widget's minimum size in a given orientation.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Determines how the shadowed box around the viewport is drawn.
;;; 
;;; Default value: GTK_SHADOW_IN
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkViewport
;;; 
;;; struct GtkViewport;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkViewport" gtk-viewport
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_viewport_get_type")
  ((hadjustment
    gtk-viewport-hadjustment
    "hadjustment" "GtkAdjustment" t t)
   (shadow-type
    gtk-viewport-shadow-type
    "shadow-type" "GtkShadowType" t t)
   (vadjustment
    gtk-viewport-vadjustment
    "vadjustment" "GtkAdjustment" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new ()
;;; 
;;; GtkWidget * gtk_viewport_new (GtkAdjustment *hadjustment,
;;;                               GtkAdjustment *vadjustment);
;;; 
;;; Creates a new GtkViewport with the given adjustments.
;;; 
;;; hadjustment :
;;;     horizontal adjustment
;;; 
;;; vadjustment :
;;;     vertical adjustment
;;; 
;;; Returns :
;;;     a new GtkViewport
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-new (&optional (hadjustment nil) (vadjustment nil))
  (make-instance 'gtk-viewport
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'gtk-viewport-new)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_hadjustment ()
;;; 
;;; GtkAdjustment * gtk_viewport_get_hadjustment (GtkViewport *viewport);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code.
;;; Use gtk_scrollable_get_hadjustment()
;;; 
;;; Returns the horizontal adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; Returns :
;;;     the horizontal adjustment of viewport
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-get-hadjustment (viewport)
  (gtk-viewport-hadjustment viewport))

(export 'gtk-viewport-get-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_vadjustment ()
;;; 
;;; GtkAdjustment * gtk_viewport_get_vadjustment (GtkViewport *viewport);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code.
;;; Use gtk_scrollable_get_vadjustment()
;;; 
;;; Returns the vertical adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; Returns :
;;;     the vertical adjustment of viewport
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-get-vadjustment (viewport)
  (gtk-viewport-vadjustment viewport))

(export 'gtk-viewport-get-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_hadjustment ()
;;; 
;;; void gtk_viewport_set_hadjustment (GtkViewport   *viewport,
;;;                                    GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_set_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code.
;;; Use gtk_scrollable_set_hadjustment()
;;; 
;;; Sets the horizontal adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-set-hadjustment (viewport adjustment)
  (setf (gtk-viewport-hadjustment viewport) adjustment))

(export 'gtk-viewport-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_vadjustment ()
;;; 
;;; void gtk_viewport_set_vadjustment (GtkViewport   *viewport,
;;;                                    GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_set_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code.
;;; Use gtk_scrollable_set_vadjustment()
;;; 
;;; Sets the vertical adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-set-vadjustment (viewport adjustment)
  (setf (gtk-viewport-vadjustment viewport) adjustment))

(export 'gtk-viewport-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_shadow_type ()
;;; 
;;; void gtk_viewport_set_shadow_type (GtkViewport   *viewport,
;;;                                    GtkShadowType  type);
;;; 
;;; Sets the shadow type of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; type :
;;;     the new shadow type
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-set-shadow-type (viewport type)
  (setf (gtk-viewport-shadow-type viewport) type))

(export 'gtk-viewport-set-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_shadow_type ()
;;; 
;;; GtkShadowType gtk_viewport_get_shadow_type (GtkViewport *viewport);
;;; 
;;; Gets the shadow type of the GtkViewport.
;;; See gtk_viewport_set_shadow_type().
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; Returns :
;;;     the shadow type
;;; ----------------------------------------------------------------------------

(defun gtk-viewport-get-shadow-type (viewport)
  (gtk-viewport-shadow-type viewport))

(export 'gtk-viewport-get-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_bin_window ()
;;; 
;;; GdkWindow * gtk_viewport_get_bin_window (GtkViewport *viewport);
;;; 
;;; Gets the bin window of the GtkViewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; Returns :
;;;     a GdkWindow
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_viewport_get_bin_window" gtk-viewport-get-bin-window)
    (g-object gdk-window)
  (viewport (g-object gtk-viewport)))

(export 'gtk-viewport-get-bin-window)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_view_window ()
;;; 
;;; GdkWindow * gtk_viewport_get_view_window (GtkViewport *viewport);
;;; 
;;; Gets the view window of the GtkViewport.
;;; 
;;; viewport :
;;;     a GtkViewport
;;; 
;;; Returns :
;;;     a GdkWindow
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_viewport_get_view_window" gtk-viewport-get-view-window)
    (g-object gdk-window)
  (viewport (g-object gtk-viewport)))

(export 'gtk-viewport-get-view-window)

;;; --- End of file gtk.viewport.lisp ------------------------------------------
