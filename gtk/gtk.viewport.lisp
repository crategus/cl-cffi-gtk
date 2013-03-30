;;; ----------------------------------------------------------------------------
;;; gtk.viewport.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkViewport
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkViewport" gtk-viewport
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_viewport_get_type")
  ((shadow-type
    gtk-viewport-shadow-type
    "shadow-type" "GtkShadowType" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-viewport 'type)
 "@version{2013-3-28}
  @begin{short}
    The GtkViewport widget acts as an adaptor class, implementing scrollability
    for child widgets that lack their own scrolling capabilities. Use
    GtkViewport to scroll child widgets such as GtkGrid, GtkBox, and so on.
  @end{short}

  If a widget has native scrolling abilities, such as GtkTextView, GtkTreeView
  or GtkIconview, it can be added to a GtkScrolledWindow with
  gtk_container_add(). If a widget does not, you must first add the widget to
  a GtkViewport, then add the viewport to the scrolled window. The convenience
  function gtk_scrolled_window_add_with_viewport() does exactly this, so you
  can ignore the presence of the viewport.

  The GtkViewport will start scrolling content only if allocated less than the
  child widget's minimum size in a given orientation.
  @see-slot{gtk-viewport-shadow-type}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-viewport) 't)
 "The @code{\"shadow-type\"} property of type @symbol{gtk-shadow-type}
  (Read / Write)@br{}
  Determines how the shadowed box around the viewport is drawn. @br{}
  Default value: @code{:in}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-viewport-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-viewport-shadow-type 'function)
 "@version{2013-3-28}
  Accessor of the slot @code{\"shadow-type\"} of the @class{gtk-viewport}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-viewport-new))

(defun gtk-viewport-new (&optional (hadjustment nil) (vadjustment nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[hadjustment]{horizontal adjustment}
  @argument[vadjustment]{vertical adjustment}
  @return{A new @class{gtk-viewport} widget.}
  Creates a new @class{gtk-viewport} widget with the given adjustments."
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
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_hadjustment()
;;; 
;;; Returns the horizontal adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport.
;;; 
;;; Returns :
;;;     the horizontal adjustment of viewport
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_vadjustment ()
;;; 
;;; GtkAdjustment * gtk_viewport_get_vadjustment (GtkViewport *viewport);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_vadjustment()
;;; 
;;; Returns the vertical adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport.
;;; 
;;; Returns :
;;;     the vertical adjustment of viewport
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_hadjustment ()
;;; 
;;; void gtk_viewport_set_hadjustment (GtkViewport *viewport,
;;;                                    GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_set_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_set_hadjustment()
;;; 
;;; Sets the horizontal adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_vadjustment ()
;;; 
;;; void gtk_viewport_set_vadjustment (GtkViewport *viewport,
;;;                                    GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_viewport_set_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_set_vadjustment()
;;; 
;;; Sets the vertical adjustment of the viewport.
;;; 
;;; viewport :
;;;     a GtkViewport.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_shadow_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-viewport-set-shadow-type))

(defun gtk-viewport-set-shadow-type (viewport type)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[viewport]{a GtkViewport}
  @argument[type]{the new shadow type}
  Sets the shadow type of the viewport."
  (setf (gtk-viewport-shadow-type viewport) type))

(export 'gtk-viewport-set-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_shadow_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-viewport-get-shadow-type))

(defun gtk-viewport-get-shadow-type (viewport)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[viewport]{a GtkViewport}
  @return{The shadow type.}
  Gets the shadow type of the GtkViewport. See gtk_viewport_set_shadow_type()."
  (gtk-viewport-shadow-type viewport))

(export 'gtk-viewport-get-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_bin_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_viewport_get_bin_window" gtk-viewport-get-bin-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[viewport]{a GtkViewport}
  @return{a GdkWindow}
  @short{Gets the bin window of the GtkViewport.}

  Since 2.20"
  (viewport (g-object gtk-viewport)))

(export 'gtk-viewport-get-bin-window)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_view_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_viewport_get_view_window" gtk-viewport-get-view-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[viewport]{a GtkViewport}
  @return{a GdkWindow}
  @short{Gets the view window of the GtkViewport.}

  Since 2.22"
  (viewport (g-object gtk-viewport)))

(export 'gtk-viewport-get-view-window)

;;; --- End of file gtk.viewport.lisp ------------------------------------------
