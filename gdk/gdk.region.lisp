;;; ----------------------------------------------------------------------------
;;; gdk.region.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
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
;;; Points, Rectangles and Regions
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
;;; GDK provides the GdkPoint, and GdkRectangle data types for representing
;;; pixels and sets of pixels on the screen.
;;; 
;;; GdkPoint is a simple structure containing an x and y coordinate of a point.
;;; 
;;; GdkRectangle is a structure holding the position and size of a rectangle.
;;; The intersection of two rectangles can be computed with
;;; gdk_rectangle_intersect(). To find the union of two rectangles use
;;; gdk_rectangle_union().
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkPoint
;;; 
;;; struct GdkPoint {
;;;   gint x;
;;;   gint y;
;;; };
;;; 
;;; Defines the x and y coordinates of a point.
;;; 
;;; gint x;
;;;     the x coordinate of the point
;;; 
;;; gint y;
;;;     the y coordinate of the point
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-point "GdkPoint"
  (x :int :initform 0)
  (y :int :initform 0))

(export (boxed-related-symbols 'gdk-point))

;;; ----------------------------------------------------------------------------
;;; struct GdkRectangle
;;; 
;;; struct GdkRectangle {
;;;   gint x;
;;;   gint y;
;;;   gint width;
;;;   gint height;
;;; };
;;; 
;;; Defines the position and size of a rectangle.
;;; 
;;; gint x;
;;;     the x coordinate of the left edge of the rectangle
;;; 
;;; gint y;
;;;     the y coordinate of the top of the rectangle
;;; 
;;; gint width;
;;;     the width of the rectangle
;;; 
;;; gint height;
;;;     the height of the rectangle
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-rectangle "GdkRectangle"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'gdk-rectangle))

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_intersect ()
;;; 
;;; gboolean gdk_rectangle_intersect (const GdkRectangle *src1,
;;;                                   const GdkRectangle *src2,
;;;                                   GdkRectangle *dest);
;;; 
;;; Calculates the intersection of two rectangles. It is allowed for dest to be
;;; the same as either src1 or src2. If the rectangles do not intersect, dest's
;;; width and height is set to 0 and its x and y values are undefined. If you
;;; are only interested in whether the rectangles intersect, but not in the
;;; intersecting area itself, pass NULL for dest.
;;; 
;;; src1 :
;;;     a GdkRectangle
;;; 
;;; src2 :
;;;     a GdkRectangle
;;; 
;;; dest :
;;;     return location for the intersection of src1 and src2, or NULL
;;; 
;;; Returns :
;;;     TRUE if the rectangles intersect
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rectangle_intersect" %gdk-rectangle-intersect) :boolean
  (src-1 (g-boxed-foreign gdk-rectangle))
  (src-2 (g-boxed-foreign gdk-rectangle))
  (dest  (g-boxed-foreign gdk-rectangle)))

(defun gdk-rectangle-intersect (src-1 src-2)
  (let ((dest (make-gdk-rectangle)))
    (when (%gdk-rectangle-intersect src-1 src-2 dest)
      dest)))

(export 'gdk-rectangle-intersect)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_union ()
;;; 
;;; void gdk_rectangle_union (const GdkRectangle *src1,
;;;                           const GdkRectangle *src2,
;;;                           GdkRectangle *dest);
;;; 
;;; Calculates the union of two rectangles. The union of rectangles src1 and
;;; src2 is the smallest rectangle which includes both src1 and src2 within it.
;;; It is allowed for dest to be the same as either src1 or src2.
;;; 
;;; src1 :
;;;     a GdkRectangle
;;; 
;;; src2 :
;;;     a GdkRectangle
;;; 
;;; dest :
;;;     return location for the union of src1 and src2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rectangle_union" %gdk-rectangle-union) :void
  (src-1 (g-boxed-foreign gdk-rectangle))
  (src-2 (g-boxed-foreign gdk-rectangle))
  (dest  (g-boxed-foreign gdk-rectangle)))

(defun gdk-rectangle-union (src-1 src-2)
  (let ((dest (make-gdk-rectangle)))
    (%gdk-rectangle-union src-1 src-2 dest)
    dest))

(export 'gdk-rectangle-union)

;;; --- End of file gdk.region.lisp --------------------------------------------
