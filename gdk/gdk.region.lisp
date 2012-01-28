;;; ----------------------------------------------------------------------------
;;; gdk.region.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
;;; 
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;;     GdkRegion
;;;
;;;     gdk_region_new
;;;     gdk_region_polygon
;;;
;;;     GdkFillRule
;;;
;;;     gdk_region_copy
;;;     gdk_region_rectangle
;;;     gdk_region_destroy
;;;     gdk_region_get_clipbox
;;;     gdk_region_get_rectangles
;;;     gdk_region_empty
;;;     gdk_region_equal
;;;     gdk_region_rect_equal
;;;     gdk_region_point_in
;;;     gdk_region_rect_in
;;;
;;;     GdkOverlapType
;;;
;;;     gdk_region_offset
;;;     gdk_region_shrink
;;;     gdk_region_union_with_rect
;;;     gdk_region_intersect
;;;     gdk_region_union
;;;     gdk_region_subtract
;;;     gdk_region_xor
;;;     
;;;     GdkSpan
;;;     
;;;     gdk_region_spans_intersect_foreach
;;; 
;;; Description
;;; 
;;; GDK provides the GdkPoint, GdkRectangle, GdkRegion and GdkSpan data types
;;; for representing pixels and sets of pixels on the screen.
;;; 
;;; GdkPoint is a simple structure containing an x and y coordinate of a point.
;;; 
;;; GdkRectangle is a structure holding the position and size of a rectangle.
;;; The intersection of two rectangles can be computed with
;;; gdk_rectangle_intersect(). To find the union of two rectangles use
;;; gdk_rectangle_union().
;;; 
;;; GdkRegion is an opaque data type holding a set of arbitrary pixels, and is
;;; usually used for clipping graphical operations (see
;;; gdk_gc_set_clip_region()).
;;; 
;;; GdkSpan is a structure holding a spanline. A spanline is a horizontal line
;;; that is one pixel wide. It is mainly used when rasterizing other graphics
;;; primitives. It can be intersected to regions by using
;;; gdk_region_spans_intersect_foreach().
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
;;;     the x coordinate of the point.
;;; 
;;; gint y;
;;;     the y coordinate of the point.
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
;;;     the x coordinate of the left edge of the rectangle.
;;; 
;;; gint y;
;;;     the y coordinate of the top of the rectangle.
;;; 
;;; gint width;
;;;     the width of the rectangle.
;;; 
;;; gint height;
;;;     the height of the rectangle.
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
;;;     return location for the intersection of src1 and src2, or NULL.
;;; 
;;; Returns :
;;;     TRUE if the rectangles intersect.
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
;;;     return location for the union of src1 and src2.
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

;;; ----------------------------------------------------------------------------
;;; GdkRegion
;;; 
;;; typedef struct _GdkRegion GdkRegion;
;;; 
;;; A GdkRegion represents a set of pixels on the screen.
;;; 
;;; In GTK3, GdkRegion will be replaced by cairo_region_t. All the functions
;;; listed in this section will go away and you will have to use the relevant
;;; Cairo functions. The conversion can be done using simple search and replace.
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gdk-region "GdkRegion"
  :alloc (%gdk-region-new))

(export (boxed-related-symbols 'gdk-region))

;; Internal for allocating a region. See the variant gdk-region-new which
;; does not return a pointer but a gdk-region.

(defcfun ("gdk_region_new" %gdk-region-new) :pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_region_new ()
;;; 
;;; GdkRegion * gdk_region_new (void);
;;; 
;;; Creates a new empty GdkRegion.
;;; 
;;; Returns :
;;;     a new empty GdkRegion
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_new" gdk-region-new) (g-boxed-foreign gdk-region :return))

(export 'gdk-region-new)

;;; ----------------------------------------------------------------------------
;;; enum GdkFillRule
;;; 
;;; typedef enum
;;; {
;;;   GDK_EVEN_ODD_RULE,
;;;   GDK_WINDING_RULE
;;; } GdkFillRule;
;;; 
;;; Warning
;;; 
;;; GdkFillRule is deprecated and should not be used in newly-written code.
;;; 
;;; The method for determining which pixels are included in a region, when
;;; creating a GdkRegion from a polygon. The fill rule is only relevant for
;;; polygons which overlap themselves.
;;; 
;;; GDK_EVEN_ODD_RULE
;;;     areas which are overlapped an odd number of times are included in the
;;;     region, while areas overlapped an even number of times are not.
;;; 
;;; GDK_WINDING_RULE
;;;     overlapping areas are always included.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFillRule" gdk-fill-rule
  (:export t
   :type-initializer "gdk_fill_rule_get_type")
  (:even-odd-rule 0)
  (:winding-rule 1))

;;; ----------------------------------------------------------------------------
;;; gdk_region_polygon ()
;;; 
;;; GdkRegion * gdk_region_polygon (const GdkPoint *points,
;;;                                 gint n_points,
;;;                                 GdkFillRule fill_rule);
;;; 
;;; Warning
;;; 
;;; gdk_region_polygon has been deprecated since version 2.22 and should not be
;;; used in newly-written code. There is no replacement. For working with paths,
;;; please use Cairo.
;;; 
;;; Creates a new GdkRegion using the polygon defined by a number of points.
;;; 
;;; points :
;;;     an array of GdkPoint structs
;;; 
;;; n_points :
;;;     the number of elements in the points array
;;; 
;;; fill_rule :
;;;     specifies which pixels are included in the region when the polygon
;;;     overlaps itself.
;;; 
;;; Returns :
;;;     a new GdkRegion based on the given polygon
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_polygon" %gdk-region-polygon)
    (g-boxed-foreign gdk-region :return)
  (points :pointer)
  (n-points :int)
  (fill-rule gdk-fill-rule))

(defun gdk-region-polygon (points fill-rule)
  (with-foreign-boxed-array (n pts gdk-point points)
    (%gdk-region-polygon pts n fill-rule)))

(export 'gdk-region-polygon)

;;; ----------------------------------------------------------------------------
;;; gdk_region_copy ()
;;; 
;;; GdkRegion * gdk_region_copy (const GdkRegion *region);
;;; 
;;; Copies region, creating an identical new region.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; Returns :
;;;     a new region identical to region
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_region_rectangle ()
;;; 
;;; GdkRegion * gdk_region_rectangle (const GdkRectangle *rectangle);
;;; 
;;; Creates a new region containing the area rectangle.
;;; 
;;; rectangle :
;;;     a GdkRectangle
;;; 
;;; Returns :
;;;     a new region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_rectangle" gdk-region-rectangle)
    (g-boxed-foreign gdk-region :return)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gdk-region-rectangle)

;;; ----------------------------------------------------------------------------
;;; gdk_region_destroy ()
;;; 
;;; void gdk_region_destroy (GdkRegion *region);
;;; 
;;; Destroys a GdkRegion.
;;; 
;;; region :
;;;     a GdkRegion
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_region_get_clipbox ()
;;; 
;;; void gdk_region_get_clipbox (const GdkRegion *region,
;;;                              GdkRectangle *rectangle);
;;; 
;;; Obtains the smallest rectangle which includes the entire GdkRegion.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; rectangle :
;;;     return location for the clipbox
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_get_clipbox" %gdk-region-get-clipbox) :void
  (region (g-boxed-foreign gdk-region))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gdk-region-get-clipbox (region)
  (let ((clipbox (make-gdk-rectangle)))
    (%gdk-region-get-clipbox region clipbox)
    clipbox))

(export 'gdk-region-get-clipbox)

;;; ----------------------------------------------------------------------------
;;; gdk_region_get_rectangles ()
;;; 
;;; void gdk_region_get_rectangles (const GdkRegion *region,
;;;                                 GdkRectangle **rectangles,
;;;                                 gint *n_rectangles);
;;; 
;;; Obtains the area covered by the region as a list of rectangles. The array
;;; returned in rectangles must be freed with g_free().
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; rectangles :
;;;     return location for an array of rectangles.
;;; 
;;; n_rectangles :
;;;     length of returned array
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_get_rectangles" %gdk-region-get-rectangles) :void
  (region (g-boxed-foreign gdk-region))
  (rectangles :pointer)
  (n-rectangles :pointer))

(defun gdk-region-get-rectangles (region)
  (with-foreign-objects ((rectangles-ptr :pointer) (n-rectangles-ptr :int))
    (%gdk-region-get-rectangles region rectangles-ptr n-rectangles-ptr)
    (let ((n (mem-ref n-rectangles-ptr :int))
          (rectangles (mem-ref rectangles-ptr :pointer)))
      (prog1
        (iter (for i from 0 below n)
              (for rect = (convert-from-foreign
                            (inc-pointer rectangles
                                         (* (foreign-type-size 'gdk-rectangle-cstruct)
                                            i))
                            '(g-boxed-foreign gdk-rectangle)))
              (collect rect))
        (g-free rectangles)))))

(export 'gdk-region-get-rectangles)

;;; ----------------------------------------------------------------------------
;;; gdk_region_empty ()
;;; 
;;; gboolean gdk_region_empty (const GdkRegion *region);
;;; 
;;; Finds out if the GdkRegion is empty.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; Returns :
;;;     TRUE if region is empty.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_empty" gdk-region-is-empty) :boolean
  (region (g-boxed-foreign gdk-region)))

(export 'gdk-region-is-empty)

;;; ----------------------------------------------------------------------------
;;; gdk_region_equal ()
;;; 
;;; gboolean gdk_region_equal (const GdkRegion *region1,
;;;                            const GdkRegion *region2);
;;; 
;;; Finds out if the two regions are the same.
;;; 
;;; region1 :
;;;     a GdkRegion
;;; 
;;; region2 :
;;;     a GdkRegion
;;; 
;;; Returns :
;;;     TRUE if region1 and region2 are equal.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_equal" gdk-region-equal) :boolean
  (region-1 (g-boxed-foreign gdk-region))
  (region-2 (g-boxed-foreign gdk-region)))

(export 'gdk-region-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_region_rect_equal ()
;;; 
;;; gboolean gdk_region_rect_equal (const GdkRegion *region,
;;;                                 const GdkRectangle *rectangle);
;;; 
;;; Warning
;;; 
;;; gdk_region_rect_equal has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use gdk_region_new_rect() and
;;; gdk_region_equal() to achieve the same effect.
;;; 
;;; Finds out if a regions is the same as a rectangle.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; rectangle :
;;;     a GdkRectangle
;;; 
;;; Returns :
;;;     TRUE if region and rectangle are equal.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_region_point_in ()
;;; 
;;; gboolean gdk_region_point_in (const GdkRegion *region, int x, int y);
;;; 
;;; Finds out if a point is in a region.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; x :
;;;     the x coordinate of a point
;;; 
;;; y :
;;;     the y coordinate of a point
;;; 
;;; Returns :
;;;     TRUE if the point is in region.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_point_in" gdk-region-point-in) :boolean
  (region (g-boxed-foreign gdk-region))
  (x :int)
  (y :int))

(export 'gdk-region-point-in)

;;; ----------------------------------------------------------------------------
;;; enum GdkOverlapType
;;; 
;;; typedef enum
;;; {
;;;   GDK_OVERLAP_RECTANGLE_IN,
;;;   GDK_OVERLAP_RECTANGLE_OUT,
;;;   GDK_OVERLAP_RECTANGLE_PART
;;; } GdkOverlapType;
;;; 
;;; Specifies the possible values returned by gdk_region_rect_in().
;;; 
;;; GDK_OVERLAP_RECTANGLE_IN
;;;     if the rectangle is inside the GdkRegion.
;;; 
;;; GDK_OVERLAP_RECTANGLE_OUT
;;;     if the rectangle is outside the GdkRegion.
;;; 
;;; GDK_OVERLAP_RECTANGLE_PART
;;;     if the rectangle is partly inside the GdkRegion.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkOverlapType" gdk-overlap-type
  (:export t
   :type-initializer "gdk_overlap_type_get_type")
  (:in 0)
  (:out 1)
  (:part 2))

;;; ----------------------------------------------------------------------------
;;; gdk_region_rect_in ()
;;; 
;;; GdkOverlapType gdk_region_rect_in (const GdkRegion *region,
;;;                                    const GdkRectangle *rectangle);
;;; 
;;; Tests whether a rectangle is within a region.
;;; 
;;; region :
;;;     a GdkRegion.
;;; 
;;; rectangle :
;;;     a GdkRectangle.
;;; 
;;; Returns :
;;;     GDK_OVERLAP_RECTANGLE_IN, GDK_OVERLAP_RECTANGLE_OUT, or
;;;     GDK_OVERLAP_RECTANGLE_PART, depending on whether the rectangle is
;;;     inside, outside, or partly inside the GdkRegion, respectively.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_rect_in" gdk-region-rect-in) gdk-overlap-type
  (region (g-boxed-foreign gdk-region))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gdk-region-rect-in)

;;; ----------------------------------------------------------------------------
;;; gdk_region_offset ()
;;; 
;;; void gdk_region_offset (GdkRegion *region, gint dx, gint dy);
;;; 
;;; Moves a region the specified distance.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; dx :
;;;     the distance to move the region horizontally
;;; 
;;; dy :
;;;     the distance to move the region vertically
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_offset" gdk-region-offset) :void
  (region (g-boxed-foreign gdk-region))
  (dx :int)
  (dy :int))

(export 'gdk-region-offset)

;;; ----------------------------------------------------------------------------
;;; gdk_region_shrink ()
;;; 
;;; void gdk_region_shrink (GdkRegion *region, gint dx, gint dy);
;;; 
;;; Warning
;;; 
;;; gdk_region_shrink has been deprecated since version 2.22 and should not be
;;; used in newly-written code. There is no replacement for this function.
;;; 
;;; Resizes a region by the specified amount. Positive values shrink the region.
;;; Negative values expand it.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; dx :
;;;     the number of pixels to shrink the region horizontally
;;; 
;;; dy :
;;;     the number of pixels to shrink the region vertically
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_shrink" gdk-region-shrink) :void
  (region (g-boxed-foreign gdk-region))
  (dx :int)
  (dy :int))

(export 'gdk-region-shrink)

;;; ----------------------------------------------------------------------------
;;; gdk_region_union_with_rect ()
;;; 
;;; void gdk_region_union_with_rect (GdkRegion *region,
;;;                                  const GdkRectangle *rect);
;;; 
;;; Sets the area of region to the union of the areas of region and rect. The
;;; resulting area is the set of pixels contained in either region or rect.
;;; 
;;; region :
;;;     a GdkRegion.
;;; 
;;; rect :
;;;     a GdkRectangle.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_union_with_rect" gdk-region-union-with-rect) :void
  (region (g-boxed-foreign gdk-region))
  (rect (g-boxed-foreign gdk-rectangle)))

(export 'gdk-region-union-with-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_region_intersect ()
;;; 
;;; void gdk_region_intersect (GdkRegion *source1, const GdkRegion *source2);
;;; 
;;; Sets the area of source1 to the intersection of the areas of source1 and
;;; source2. The resulting area is the set of pixels contained in both source1
;;; and source2.
;;; 
;;; source1 :
;;;     a GdkRegion
;;; 
;;; source2 :
;;;     another GdkRegion
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_intersect" gdk-region-intersect) :void
  (target (g-boxed-foreign gdk-region))
  (source (g-boxed-foreign gdk-region)))

(export 'gdk-region-intersect)

;;; ----------------------------------------------------------------------------
;;; gdk_region_union ()
;;; 
;;; void gdk_region_union (GdkRegion *source1, const GdkRegion *source2);
;;; 
;;; Sets the area of source1 to the union of the areas of source1 and source2.
;;; The resulting area is the set of pixels contained in either source1 or
;;; source2.
;;; 
;;; source1 :
;;;     a GdkRegion
;;; 
;;; source2 :
;;;     a GdkRegion
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_union" gdk-region-union) :void
  (target (g-boxed-foreign gdk-region))
  (source (g-boxed-foreign gdk-region)))

(export 'gdk-region-union)

;;; ----------------------------------------------------------------------------
;;; gdk_region_subtract ()
;;; 
;;; void gdk_region_subtract (GdkRegion *source1, const GdkRegion *source2);
;;; 
;;; Subtracts the area of source2 from the area source1. The resulting area is
;;; the set of pixels contained in source1 but not in source2.
;;; 
;;; source1 :
;;;     a GdkRegion
;;; 
;;; source2 :
;;;     another GdkRegion
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_subtract" gdk-region-subtract) :void
  (source-1 (g-boxed-foreign gdk-region))
  (source-2 (g-boxed-foreign gdk-region)))

(export 'gdk-region-subtract)

;;; ----------------------------------------------------------------------------
;;; gdk_region_xor ()
;;; 
;;; void gdk_region_xor (GdkRegion *source1, const GdkRegion *source2);
;;; 
;;; Sets the area of source1 to the exclusive-OR of the areas of source1 and
;;; source2. The resulting area is the set of pixels contained in one or the
;;; other of the two sources but not in both.
;;; 
;;; source1 :
;;;     a GdkRegion
;;; 
;;; source2 :
;;;     another GdkRegion
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_xor" gdk-region-xor) :void
  (target (g-boxed-foreign gdk-region))
  (source (g-boxed-foreign gdk-region)))

(export 'gdk-region-xor)

;;; ----------------------------------------------------------------------------
;;; struct GdkSpan
;;; 
;;; struct GdkSpan {
;;;   gint x;
;;;   gint y;
;;;   gint width;
;;; };
;;; 
;;; A GdkSpan represents a horizontal line of pixels starting at the pixel
;;; with coordinates x, y and ending before x + width, y.
;;; 
;;; gint x;
;;;     x coordinate of the first pixel.
;;; 
;;; gint y;
;;;     y coordinate of the first pixel.
;;; 
;;; gint width;
;;;     number of pixels in the span.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-span "GdkSpan"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0))

(export (boxed-related-symbols 'gdk-span))

;;; ----------------------------------------------------------------------------
;;; GdkSpanFunc ()
;;; 
;;; void (*GdkSpanFunc) (GdkSpan *span, gpointer data);
;;; 
;;; Warning
;;; 
;;; GdkSpanFunc is deprecated and should not be used in newly-written code.
;;; 
;;; This defines the type of the function passed to
;;; gdk_region_spans_intersect_foreach().
;;; 
;;; span :
;;;     a GdkSpan.
;;; 
;;; data :
;;;     the user data passed to gdk_region_spans_intersect_foreach().
;;; ----------------------------------------------------------------------------

(defcallback gdk-span-func-callback :void
    ((span (g-boxed-foreign gdk-span)) (data :pointer))
  (let ((fn (stable-pointer-value data)))
    (funcall fn span)))

;;; ----------------------------------------------------------------------------
;;; gdk_region_spans_intersect_foreach ()
;;; 
;;; void gdk_region_spans_intersect_foreach  (GdkRegion *region,
;;;                                           const GdkSpan *spans,
;;;                                           int n_spans,
;;;                                           gboolean sorted,
;;;                                           GdkSpanFunc function,
;;;                                           gpointer data);
;;; 
;;; Warning
;;; 
;;; gdk_region_spans_intersect_foreach has been deprecated since version 2.22
;;; and should not be used in newly-written code. There is no replacement.
;;; 
;;; Calls a function on each span in the intersection of region and spans.
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; spans :
;;;     an array of GdkSpans
;;; 
;;; n_spans :
;;;     the length of spans
;;; 
;;; sorted :
;;;     TRUE if spans is sorted wrt. the y coordinate
;;; 
;;; function :
;;;     function to call on each span in the intersection
;;; 
;;; data :
;;;     data to pass to function
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_region_spans_intersect_foreach"
          %gdk-region-spans-intersect-foreach) :void
  (region (g-boxed-foreign gdk-region))
  (spans :pointer)
  (n-spans :int)
  (sorted :boolean)
  (func :pointer)
  (data :pointer))

(defun gdk-region-spans-intersect-foreach (region spans sorted fn)
  (with-stable-pointer (ptr fn)
    (with-foreign-boxed-array (n spans-ptr gdk-span spans)
      (%gdk-region-spans-intersect-foreach
                                  region
                                  spans-ptr
                                  n
                                  sorted
                                  (callback %gdk-region-spans-intersect-foreach)
                                  ptr))))

(export 'gdk-region-spans-intersect-foreach)

;;; --- End of file gdk.region.lisp --------------------------------------------
