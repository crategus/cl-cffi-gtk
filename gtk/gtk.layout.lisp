;;; ----------------------------------------------------------------------------
;;; gtk.layout.lisp
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
;;; GtkLayout
;;;
;;;     Infinite scrollable area containing child widgets and/or custom drawing
;;;
;;; Types and Values
;;;
;;;     GtkLayout
;;;
;;; Functions
;;;
;;;     gtk_layout_new
;;;     gtk_layout_put
;;;     gtk_layout_move
;;;     gtk_layout_set_size
;;;     gtk_layout_get_size
;;;     gtk_layout_get_hadjustment                         deprecated
;;;     gtk_layout_get_vadjustment                         deprecated
;;;     gtk_layout_set_hadjustment                         deprecated
;;;     gtk_layout_set_vadjustment                         deprecated
;;;     gtk_layout_get_bin_window
;;;
;;; Properties
;;;
;;;     guint  height  Read / Write
;;;     guint  width   Read / Write
;;;
;;; Child Properties
;;;
;;;     gint  x  Read / Write
;;;     gint  y  Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkLayout
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLayout implements AtkImplementorIface, GtkBuildable and GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLayout
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkLayout" 'gtk-layout))

(define-g-object-class "GtkLayout" gtk-layout
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_layout_get_type")
  ((height
    gtk-layout-height
    "height" "guint" t t)
   (width
    gtk-layout-width
    "width" "guint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-layout 'type)
 "@version{2020-5-3}
  @begin{short}
    @sym{gtk-layout} is similar to @class{gtk-drawing-area} in that it is a
    \"blank slate\" and does not do anything but paint a blank background by
    default.
  @end{short}
  It is different in that it supports scrolling natively, you can add it to a
  @class{gtk-scrolled-window}, and it can contain child widgets, since it is a
  @class{gtk-container}. However if you are just going to draw, a
  @class{gtk-drawing-area} is a better choice since it has lower overhead.

  When handling expose events on a @sym{gtk-layout}, you must draw to the
  @class{gdk-window} returned by the funcion @fun{gtk-layout-bin-window}, rather
  than to the one returned by the funcion @fun{gtk-widget-window} as you would
  for a drawing area.
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[x]{entry}
        The @code{x} child property of type @code{:int} (Read / Write) @br{}
        x position of the child widget. @br{}
        Default value: 0
      @end{entry}
      @begin[y]{entry}
        The @code{y} child property of type @code{:int} (Read / Write) @br{}
        y position of the child widget. @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-layout-height}
  @see-slot{gtk-layout-width}
  @see-class{gtk-drawing-area}
  @see-class{gtk-container}
  @see-class{gtk-scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-layout-height ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height" 'gtk-layout) 't)
 "The @code{height} property of type @code{:uint} (Read / Write) @br{}
  The height of the layout. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 100")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-height 'function)
 "@version{2020-5-3}
  @syntax[]{(gtk-layout-height object) => height}
  @syntax[]{(setf (gtk-layout-height object) height)}
  @argument[object]{a @class{gtk-layout} container}
  @argument[height]{an unsigned integer with the height of the layout}
  @begin{short}
    Accessor of the @slot[gtk-layout]{height} slot of the @class{gtk-layout}
    class.
  @end{short}

  The slot access function @sym{gtk-layout-height} gets the height of the
  layout. The slot access function @sym{(setf gtk-layout-height)} sets the
  height of the layout.
  @see-class{gtk-layout}")

;;; --- gtk-layout-width -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width" 'gtk-layout) 't)
 "The @code{width} property of type @code{:uint} (Read / Write) @br{}
  The width of the layout. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 100")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-width 'function)
 "@version{2020-5-3}
  @syntax[]{(gtk-layout-width object) => width}
  @syntax[]{(setf (gtk-layout-width object) width)}
  @argument[object]{a @class{gtk-layout} container}
  @argument[width]{an unsigned integer with the width of the layout}
  @begin{short}
    Accessor of the slot @slot[gtk-layout]{width} of the @class{gtk-layout}
    class.
  @end{short}

  The slot access function @sym{gtk-layout-width} gets the width of the
  layout. The slot access function @sym{(setf gtk-layout-width)} sets the
  width of the layout.
  @see-class{gtk-layout}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-layout-child-x -----------------------------------------------------

(define-child-property "GtkLayout"
                       gtk-layout-child-x
                       "x" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-child-x atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-child-x 'function)
 "@version{2020-5-3}
  @syntax[]{(gtk-layout-child-x container cild) => x}
  @syntax[]{(setf (gtk-layout-child-x container child) x)}
  @argument[container]{a @class{gtk-layout} container}
  @argument[child]{a @class{gtk-widget} child}
  @argument[x]{an integer with the x position of the child widget}
  @begin{short}
    Accessor of the @code{x} child property of the @class{gtk-layout} class.
  @end{short}

  The x position of the child widget in the layout.
  @see-class{gtk-layout}")

;;; --- gtk-layout-child-y -----------------------------------------------------

(define-child-property "GtkLayout"
                       gtk-layout-child-y
                       "y" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-child-y atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-child-y 'function)
 "@version{2020-5-3}
  @syntax[]{(gtk-layout-child-y container cild) => y}
  @syntax[]{(setf (gtk-layout-child-y container child) y)}
  @argument[container]{a @class{gtk-layout} container}
  @argument[child]{a @class{gtk-widget} child}
  @argument[y]{an integer with the y position of the child widget}
  @begin{short}
    Accessor of the child property @code{y} of the @class{gtk-layout} class.
  @end{short}

  The y position of the child widget in the layout.
  @see-class{gtk-layout}")

;;; ----------------------------------------------------------------------------
;;; gtk_layout_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-new))

(defun gtk-layout-new (&optional (hadjustment nil) (vadjustment nil))
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @argument[hadjustment]{horizontal scroll @class{gtk-adjustment},
    or @code{nil}}
  @argument[vadjustment]{vertical scroll  @class{gtk-adjustment}, or @code{nil}}
  @return{A new @class{gtk-layout} container.}
  @begin{short}
    Creates a new @class{gtk-layout} container.
  @end{short}
  Unless you have a specific adjustment you would like the layout to use for
  scrolling, pass @code{nil} for @arg{hadjustment} and @arg{vadjustment}.
  @see-class{gtk-layout}"
  (make-instance 'gtk-layout
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'gtk-layout-new)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_put ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_put" gtk-layout-put) :void
 #+cl-cffi-gtk-documentation
 "@version{2015-5-3}
  @argument[layout]{a @class{gtk-layout} container}
  @argument[child]{a @class{gtk-widget} child}
  @argument[x]{an integer with the x position of the child widget}
  @argument[y]{an integer with the y position of the child widget}
  @begin{short}
    Adds an child widget to the layout, at the given position in pixels.
  @end{short}
  The layout becomes the new parent container of the child widget.
  @see-class{gtk-layout}
  @see-function{gtk-layout-move}"
  (layout (g-object gtk-layout))
  (widget (g-object gtk-widget))
  (x :int)
  (y :int))

(export 'gtk-layout-put)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_move" gtk-layout-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @argument[layout]{a @class{gtk-layout} container}
  @argument[child]{a current @class{gtk-widget} child of @arg{layout}}
  @argument[x]{an integer with the x position to move to}
  @argument[y]{an integer with the y position to move to}
  @begin{short}
    Moves a current child of the layout to a new position in pixels.
  @end{short}
  @see-class{gtk-layout}
  @see-function{gtk-layout-put}"
  (layout (g-object gtk-layout))
  (widget (g-object gtk-widget))
  (x :int)
  (y :int))

(export 'gtk-layout-move)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-set-size))

(defun gtk-layout-set-size (layout width height)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @argument[layout]{a @class{gtk-layout} container}
  @argument[width]{an integer with the width of entire scrollable area}
  @argument[height]{an integer with the height of entire scrollable area}
  @begin{short}
    Sets the size in pixels of the scrollable area of the layout.
  @end{short}
  @see-class{gtk-layout}
  @see-function{gtk-layout-get-size}
  @see-function{gtk-layout-height}
  @see-function{gtk-layout-width}"
  (setf (gtk-layout-width layout) width
        (gtk-layout-height layout) height))

(export 'gtk-layout-set-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-get-size))

(defun gtk-layout-get-size (layout)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @argument[layout]{a @class{gtk-layout} container}
  @begin{return}
    @code{width} -- an integer with the width set on @arg{layout} @br{}
    @code{height} -- an integer with the height set on @arg{layout} @br{}
  @end{return}
  @begin{short}
    Gets the size in pixels that has been set on the layout, and that
    determines the total extents of the layout's scrollbar area.
  @end{short}
  @see-class{gtk-layout}
  @see-function{gtk-layout-set-size}
  @see-function{gtk-layout-height}
  @see-function{gtk-layout-width}"
  (values (gtk-layout-width layout)
          (gtk-layout-height layout)))

(export 'gtk-layout-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_layout_get_hadjustment (GtkLayout *layout);
;;;
;;; Warning
;;;
;;; gtk_layout_get_hadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_get_hadjustment()
;;;
;;; This function should only be called after the layout has been placed in a
;;; GtkScrolledWindow or otherwise configured for scrolling. It returns the
;;; GtkAdjustment used for communication between the horizontal scrollbar and
;;; layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; Returns :
;;;     horizontal scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_layout_get_vadjustment (GtkLayout *layout);
;;;
;;; Warning
;;;
;;; gtk_layout_get_vadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_get_vadjustment()
;;;
;;; This function should only be called after the layout has been placed in a
;;; GtkScrolledWindow or otherwise configured for scrolling. It returns the
;;; GtkAdjustment used for communication between the vertical scrollbar and
;;; layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; Returns :
;;;     vertical scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_hadjustment ()
;;;
;;; void gtk_layout_set_hadjustment (GtkLayout *layout,
;;;                                  GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_layout_set_hadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_set_hadjustment()
;;;
;;; Sets the horizontal scroll adjustment for the layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; adjustment :
;;;     new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_vadjustment ()
;;;
;;; void gtk_layout_set_vadjustment (GtkLayout *layout,
;;;                                  GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_layout_set_vadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_set_vadjustment()
;;;
;;; Sets the vertical scroll adjustment for the layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; adjustment :
;;;     new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_bin_window () -> gtk-layout-bin-window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_get_bin_window" gtk-layout-bin-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-3}
  @argument[layout]{a @class{gtk-layout} container}
  @return{A @class{gdk-window} object.}
  @begin{short}
    Retrieve the bin window of the layout used for drawing operations.
  @end{short}
  @see-class{gtk-layout}"
  (layout (g-object gtk-layout)))

(export 'gtk-layout-bin-window)

;;; --- End of file gtk.layout.lisp --------------------------------------------
