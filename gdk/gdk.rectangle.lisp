;;; ----------------------------------------------------------------------------
;;; gdk.rectangle.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; Points and Rectangles
;;;
;;;     Simple graphical data types
;;;
;;; Types and Values
;;;
;;;     GdkRectangle
;;;
;;; Functions
;;;
;;;     gdk_rectangle_intersect
;;;     gdk_rectangle_union
;;;     gdk_rectangle_equal
;;;
;;; Description
;;;
;;; GDK provides the GdkPoint and GdkRectangle data types for representing
;;; pixels and sets of pixels on the screen. Together with Cairo's
;;; cairo_region_t data type, they make up the central types for representing
;;; graphical data.
;;;
;;; GdkPoint is a simple structure containing an x and y coordinate of a point.
;;;
;;; GdkRectangle is a structure holding the position and size of a rectangle.
;;; The intersection of two rectangles can be computed with
;;; gdk_rectangle_intersect(). To find the union of two rectangles use
;;; gdk_rectangle_union().
;;;
;;; cairo_region_t is usually used for managing clipping of graphical
;;; operations.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRectangle
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-rectangle "GdkRectangle"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-rectangle 'type)
 "@version{2020-9-6}
  @begin{short}
    Defines the position and size of a rectangle.
  @end{short}
  It is identical to @symbol{cairo-rectangle-int-t}.

  @sym{gdk-rectangle} is a structure holding the position and size of a
  rectangle. The intersection of two rectangles can be computed with the
  function @fun{gdk-rectangle-intersect}. To find the union of two rectangles
  use the function @fun{gdk-rectangle-union}.

  @symbol{cairo-region-t} is usually used for managing clipping of graphical
  operations.
  @begin{pre}
(define-g-boxed-cstruct gdk-rectangle \"GdkRectangle\"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @see-constructor{make-gdk-rectangle}
  @see-constructor{copy-gdk-rectangle}
  @see-slot{gdk-rectangle-x}
  @see-slot{gdk-rectangle-y}
  @see-slot{gdk-rectangle-width}
  @see-slot{gdk-rectangle-height}
  @see-symbol{cairo-region-t}")

(export (boxed-related-symbols 'gdk-rectangle))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-x 'function)
 "@version{2020-9-6}
  @syntax[]{(gdk-rectangle-x instance) => x}
  @syntax[]{(setf (gdk-rectangle-x instance) x)}
  @argument[instance]{a @class{gdk-rectangle} structure}
  @argument[x]{a @code{:int} with the x coordinate of the rectangle}
  @begin{short}
    Accessor of the slot @code{x} of the @class{gdk-rectangle} structure.
  @end{short}
  @see-class{gdk-rectangle}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-y 'function)
 "@version{2020-9-6}
  @syntax[]{(gdk-rectangle-y instance) => y}
  @syntax[]{(setf (gdk-rectangle-y instance) y)}
  @argument[instance]{a @class{gdk-rectangle} structure}
  @argument[x]{a @code{:int} with the y coordinate of the rectangle}
  @begin{short}
    Accessor of the slot @code{y} of the @class{gdk-rectangle} structure.
  @end{short}
  @see-class{gdk-rectangle}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-width atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-width 'function)
 "@version{2020-9-6}
  @syntax[]{(gdk-rectangle-width instance) => width}
  @syntax[]{(setf (gdk-rectangle-width instance) width)}
  @argument[instance]{a @class{gdk-rectangle} structure}
  @argument[x]{a @code{:int} with the width of the rectangle}
  @begin{short}
    Accessor of the slot @code{width} of the @class{gdk-rectangle} structure.
  @end{short}
  @see-class{gdk-rectangle}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-height atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-height 'function)
 "@version{2020-9-6}
  @syntax[]{(gdk-rectangle-height instance) => height}
  @syntax[]{(setf (gdk-rectangle-height instance) height)}
  @argument[instance]{a @class{gdk-rectangle} structure}
  @argument[x]{a @code{:int} with the height of the rectangle}
  @begin{short}
    Accessor of the slot @code{height} of the @class{gdk-rectangle} structure.
  @end{short}
  @see-class{gdk-rectangle}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-rectangle 'function)
 "@version{2020-9-6}
  @argument[x]{a @code{:int} with the value for the slot @code{x}}
  @argument[y]{a @code{:int} with the value for the slot @code{y}}
  @argument[width]{a @code{:int} with the value for the slot @code{width}}
  @argument[height]{a @code{:int} with the value for the slot @code{height}}
  @begin{short}
    Returns a @class{gdk-rectangle} structure with the initial values given to
    @arg{x}, @arg{y}, @arg{width}, and @arg{height}.
  @end{short}
  @see-class{gdk-rectangle}
  @see-function{copy-gdk-rectangle}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-rectangle 'function)
 "@version{2020-9-6}
  @argument[instance]{a @class{gdk-rectangle} instance}
  @begin{short}
    Copy constructor of a @class{gdk-rectangle} structure.
  @end{short}
  @see-class{gdk-rectangle}
  @see-function{make-gdk-rectangle}")

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_intersect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rectangle_intersect" %gdk-rectangle-intersect) :boolean
  (rect1 (g-boxed-foreign gdk-rectangle))
  (rect2 (g-boxed-foreign gdk-rectangle))
  (dest  (g-boxed-foreign gdk-rectangle)))

(defun gdk-rectangle-intersect (rect1 rect2)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[rect1]{a @class{gdk-rectangle} structure}
  @argument[rect2]{a @class{gdk-rectangle} structure}
  @return{A @class{gdk-rectangle} with the intersection of @arg{rect1} and
     @arg{rect2}, or @code{nil}.}
  @begin{short}
    Calculates the intersection of two rectangles.
  @end{short}
  If the rectangles do not intersect @code{nil} is returned.
  @see-class{gdk-rectangle}
  @see-function{gdk-rectangle-union}"
  (let ((dest (make-gdk-rectangle)))
    (when (%gdk-rectangle-intersect rect1 rect2 dest)
      dest)))

(export 'gdk-rectangle-intersect)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_union ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rectangle_union" %gdk-rectangle-union) :void
  (rect1 (g-boxed-foreign gdk-rectangle))
  (rect2 (g-boxed-foreign gdk-rectangle))
  (dest  (g-boxed-foreign gdk-rectangle)))

(defun gdk-rectangle-union (rect1 rect2)
 #+cl-cffi-gtk-documentation
 "@version{2029-9-6}
  @argument[rect1]{a @class{gdk-rectangle} structure}
  @argument[rect2]{a @class{gdk-rectangle} structure}
  @return{A @class{gdk-rectangle} with the union of @arg{rect1} and
    @arg{rect2}.}
  @begin{short}
    Calculates the union of two rectangles.
  @end{short}
  The union of rectangles @arg{rect1} and @arg{rect2} is the smallest rectangle
  which includes both rectangles within it."
  (let ((dest (make-gdk-rectangle)))
    (%gdk-rectangle-union rect1 rect2 dest)
    dest))

(export 'gdk-rectangle-union)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_equal ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_rectangle_equal" gdk-rectangle-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[rect1]{a @class{gdk-rectangle} structure}
  @argument[rect2]{a @class{gdk-rectangle} structure}
  @return{@em{True} if the rectangles are equal.}
  @begin{short}
    Checks if the two given rectangles are equal.
  @end{short}

  Since 3.20
  @see-class{gdk-rectangle}"
  (rect1 (g-boxed-foreign gdk-rectangle))
  (rect2 (g-boxed-foreign gdk-rectangle)))

#+gdk-3-20
(export 'gdk-rectangle-equal)

;;; --- End of file gdk.rectangle.lisp -----------------------------------------
