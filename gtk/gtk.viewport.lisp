;;; ----------------------------------------------------------------------------
;;; gtk.viewport.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     An adapter which makes widgets scrollable
;;;
;;; Types and Values
;;;
;;;     GtkViewport
;;;     GtkViewportClass
;;;
;;; Functions
;;;
;;;     gtk_viewport_new
;;;     gtk_viewport_get_hadjustment                       deprecated
;;;     gtk_viewport_get_vadjustment                       deprecated
;;;     gtk_viewport_set_hadjustment                       deprecated
;;;     gtk_viewport_set_vadjustment                       deprecated
;;;     gtk_viewport_set_shadow_type                       Accessor
;;;     gtk_viewport_get_shadow_type                       Accessor
;;;     gtk_viewport_get_bin_window
;;;     gtk_viewport_get_view_window
;;;
;;; Properties
;;;
;;;     GtkShadowType    shadow-type    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkViewport
;;;
;;; Implemented Interfaces
;;;
;;;     GtkViewport implements AtkImplementorIface, GtkBuildable and
;;;     GtkScrollable.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-viewport 'type)
 "@version{2021-3-19}
  @begin{short}
    The @sym{gtk-viewport} widget acts as an adaptor class, implementing
    scrollability for child widgets that lack their own scrolling capabilities.
  @end{short}
  Use the @sym{gtk-viewport} widget to scroll child widgets such as the widgets
  @class{gtk-grid}, @class{gtk-box}, and so on.

  If a widget has native scrolling abilities, such as the widgets
  @class{gtk-text-view}, @class{gtk-tree-view} or @class{gtk-icon-view},
  it can be added to a @class{gtk-scrolled-window} widget with the function
  @fun{gtk-container-add}. If a widget does not, you must first add the widget
  to a @sym{gtk-viewport} widget, then add the viewport to the scrolled window.
  The function @fun{gtk-container-add} does this automatically if a child that
  does not implement the @class{gtk-scrollable} interface is added to a
  @class{gtk-scrolled-window} widget, so you can ignore the presence of the
  viewport.

  The @sym{gtk-viewport} widget will start scrolling content only if allocated
  less than the child widget's minimum size in a given orientation.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-viewport} widget has a single CSS node with name
    @code{viewport}.
  @end{dictionary}
  @see-slot{gtk-viewport-shadow-type}
  @see-class{gtk-scrolled-window}
  @see-class{gtk-scrollable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-viewport-shadow-type -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-viewport) 't)
 "The @code{shadow-type} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Determines how the shadowed box around the viewport is drawn. @br{}
  Default value: @code{:in}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-viewport-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-viewport-shadow-type 'function)
 "@version{2021-3-18}
  @syntax[]{(gtk-viewport-shadow-type object) => type}
  @syntax[]{(setf (gtk-viewport-shadow-type object) type)}
  @argument[viewport]{a @class{gtk-viewport} widget}
  @argument[type]{a @symbol{gtk-shadow-type} value for the shadow type}
  @begin{short}
    Accessor of the @slot[gtk-viewport]{shadow-type} slot of the
    @class{gtk-viewport} class.
  @end{short}

  The slot access function @sym{gtk-viewport-shadow-type} gets the shadow type
  of the viewport. The slot access function
  @sym{(setf gtk-viewport-shadow-type)} sets the shadow type.
  @see-class{gtk-viewport}
  @see-symbol{gtk-shadow-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-viewport-new))

(defun gtk-viewport-new (&optional (hadjustment nil) (vadjustment nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-3-18}
  @argument[hadjustment]{horizontal @class{gtk-adjustment} object}
  @argument[vadjustment]{vertical @class{gtk-adjustment} object}
  @return{A new @class{gtk-viewport} widget.}
  @begin{short}
    Creates a new viewport with the given adjustments.
  @end{short}
  @see-class{gtk-viewport}
  @see-class{gtk-adjustment}"
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
;;; gtk_viewport_get_bin_window () -> gtk-viewport-bin-window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_viewport_get_bin_window" gtk-viewport-bin-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-18}
  @argument[viewport]{a @class{gtk-viewport} widget}
  @return{A @class{gdk-window} object.}
  @short{Gets the bin window of the viewport.}
  @see-class{gtk-viewport}
  @see-class{gdk-window}"
  (viewport (g-object gtk-viewport)))

(export 'gtk-viewport-bin-window)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_view_window () -> gtk-viewport-view-window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_viewport_get_view_window" gtk-viewport-view-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-18}
  @argument[viewport]{a @class{gtk-viewport} widget}
  @return{A @class{gdk-window} object.}
  @short{Gets the view window of the viewport.}
  @see-class{gtk-viewport}
  @see-class{gdk-window}"
  (viewport (g-object gtk-viewport)))

(export 'gtk-viewport-view-window)

;;; --- End of file gtk.viewport.lisp ------------------------------------------
