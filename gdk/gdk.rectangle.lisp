;;; ----------------------------------------------------------------------------
;;; gdk.rectangle.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
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
;;; Points and Rectangles
;;;
;;; Simple graphical data types
;;;
;;; Synopsis
;;;
;;;     GdkPoint
;;;     GdkRectangle
;;;
;;;     gdk_rectangle_intersect
;;;     gdk_rectangle_union
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
;;; struct GdkPoint
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-point "GdkPoint"
  (x :int :initform 0)
  (y :int :initform 0))

(export (boxed-related-symbols 'gdk-point))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-point atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-point 'type)
 "@version{2013-1-25}
  @begin{short}
    @sym{gdk-point} is a simple structure containing an x and y coordinate of a
    point.
  @end{short}

  Defines the x and y coordinates of a point.
  @begin{pre}
(define-g-boxed-cstruct gdk-point \"GdkPoint\"
  (x :int :initform 0)
  (y :int :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[x]{the x coordinate of the point.}
    @entry[y]{the y coordinate of the point.}
  @end{table}
  @see-constructor{make-gdk-point}
  @see-constructor{copy-gdk-point}
  @see-slot{gdk-point-x}
  @see-slot{gdk-point-y}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-point-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-point-x 'function)
 "@version{2013-1-25}
  @begin{short}
    Accessor of the slot @code{x} of the @class{gdk-point} CStruct.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-point-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-point-y 'function)
 "@version{2013-1-25}
  @begin{short}
    Accessor of the slot @code{y} of the @class{gdk-point} CStruct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-point 'function)
 "@version{2013-1-25}
  @argument[x]{value for the slot @code{x}}
  @argument[y]{value for the slot @code{y}}
  @begin{short}
    Returns a @class{gdk-point} CStruct with the initial values given to
    @arg{x} and @arg{y}.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-point 'function)
 "@version{2013-4-4}
  @argument[instance]{a @class{gdk-point} instance}
  @short{Copy constructor of a @class{gdk-point} object.}")

;;; ----------------------------------------------------------------------------
;;; GdkRectangle
;;;
;;; typedef cairo_rectangle_int_t GdkRectangle;
;;;
;;; Defines the position and size of a rectangle. It is identical to
;;; cairo_rectangle_int_t.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-rectangle "GdkRectangle"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-rectangle 'type)
 "@version{2013-1-25}
  @begin{short}
    Defines the position and size of a rectangle. It is identical to
    @symbol{cairo-rectangle-int-t}.
  @end{short}

  @sym{gdk-rectangle} is a structure holding the position and size of a
  rectangle. The intersection of two rectangles can be computed with
  @fun{gdk-rectangle-intersect}. To find the union of two rectangles use
  @fun{gdk-rectangle-union}.

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
  @see-slot{gdk-rectangle-height}")

(export (boxed-related-symbols 'gdk-rectangle))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-x atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-x 'function)
 "@version{2013-1-25}
  @begin{short}
    Accessor of the slot @code{x} of the @class{gdk-rectangle} CStruct.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-y atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-y 'function)
 "@version{2013-1-25}
  @begin{short}
    Accessor of the slot @code{y} of the @class{gdk-rectangle} CStruct.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-width atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-width 'function)
 "@version{2013-1-25}
  @begin{short}
    Accessor of the slot @code{width} of the @class{gdk-rectangle} CStruct.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rectangle-height atdoc:*function-name-alias*) "Accessor"
      (documentation 'gdk-rectangle-height 'function)
 "@version{2013-1-25}
  @begin{short}
    Accessor of the slot @code{height} of the @class{gdk-rectangle} CStruct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-rectangle 'function)
 "@version{2013-1-25}
  @argument[x]{value for the slot @code{x}}
  @argument[y]{value for the slot @code{y}}
  @argument[width]{value for the slot @code{width}}
  @argument[height]{value for the slot @code{height}}
  @begin{short}
    Returns a @class{gdk-rectangle} CStruct with the initial values give to
    @arg{x}, @arg{y}, @arg{width}, and @arg{height}.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-rectangle 'function)
 "@version{2013-4-4}
  @argument[instance]{a @class{gdk-rectangle} instance}
  @short{Copy constructor of a @class{gdk-rectangle} object.}")

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_intersect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rectangle_intersect" %gdk-rectangle-intersect) :boolean
  (src-1 (g-boxed-foreign gdk-rectangle))
  (src-2 (g-boxed-foreign gdk-rectangle))
  (dest  (g-boxed-foreign gdk-rectangle)))

(defun gdk-rectangle-intersect (src-1 src-2)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-25}
  @argument[src1]{a @class{gdk-rectangle} structure}
  @argument[src2]{a @class{gdk-rectangle} structure}
  @return{The intersection of @arg{src1} and @arg{src2}, or @code{nil}.}
  @begin{short}
    Calculates the intersection of two rectangles.
  @end{short}
  If the rectangles do not intersect, the intersection width and height is set
  to 0 and its x and y values are undefined."
  (let ((dest (make-gdk-rectangle)))
    (when (%gdk-rectangle-intersect src-1 src-2 dest)
      dest)))

(export 'gdk-rectangle-intersect)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_union ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rectangle_union" %gdk-rectangle-union) :void
  (src-1 (g-boxed-foreign gdk-rectangle))
  (src-2 (g-boxed-foreign gdk-rectangle))
  (dest  (g-boxed-foreign gdk-rectangle)))

(defun gdk-rectangle-union (src-1 src-2)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-25}
  @argument[src1]{a @class{gdk-rectangle}}
  @argument[src2]{a @class{gdk-rectangle}}
  @return{The union of @arg{src1} and @arg{src2}.}
  @begin{short}
    Calculates the union of two rectangles.
  @end{short}
  The union of rectangles @arg{src1} and @arg{src2} is the smallest rectangle
  which includes both @arg{src1} and @arg{src2} within it."
  (let ((dest (make-gdk-rectangle)))
    (%gdk-rectangle-union src-1 src-2 dest)
    dest))

(export 'gdk-rectangle-union)

;;; --- End of file gdk.rectangle.lisp -----------------------------------------
