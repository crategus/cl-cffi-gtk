;;; ----------------------------------------------------------------------------
;;; cairo.regions.lisp
;;;
;;; The documentation has been copied from the Cairo Reference Manual
;;; for Cairo 1.12.0 . See http://cairographics.org
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
;;; Regions
;;; 
;;; Representing a pixel-aligned area
;;;     
;;; Synopsis
;;; 
;;;     cairo_region_t
;;;     
;;;     cairo_region_create
;;;     cairo_region_create_rectangle
;;;     cairo_region_create_rectangles
;;;     cairo_region_copy
;;;     cairo_region_reference
;;;     cairo_region_destroy
;;;     cairo_region_status
;;;     cairo_region_get_extents
;;;     cairo_region_num_rectangles
;;;     cairo_region_get_rectangle
;;;     cairo_region_is_empty
;;;     cairo_region_contains_point
;;;
;;;     cairo_region_overlap_t
;;;
;;;     cairo_region_contains_rectangle
;;;     cairo_region_equal
;;;     cairo_region_translate
;;;     cairo_region_intersect
;;;     cairo_region_intersect_rectangle
;;;     cairo_region_subtract
;;;     cairo_region_subtract_rectangle
;;;     cairo_region_union
;;;     cairo_region_union_rectangle
;;;     cairo_region_xor
;;;     cairo_region_xor_rectangle
;;; 
;;; Description
;;; 
;;; Regions are a simple graphical data type representing an area of
;;; integer-aligned rectangles. They are often used on raster surfaces to track
;;; areas of interest, such as change or clip areas.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_region_t
;;; 
;;; typedef struct _cairo_region cairo_region_t;
;;; 
;;; A cairo_region_t represents a set of integer-aligned rectangles.
;;; 
;;; It allows set-theoretical operations like cairo_region_union() and
;;; cairo_region_intersect() to be performed on them.
;;; 
;;; Memory management of cairo_region_t is done with cairo_region_reference()
;;; and cairo_region_destroy().
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

(defctype cairo-region-t :pointer)

(export 'cairo-region-t)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create ()
;;; 
;;; cairo_region_t * cairo_region_create (void);
;;; 
;;; Allocates a new empty region object.
;;; 
;;; Returns :
;;;     A newly allocated cairo_region_t. Free with cairo_region_destroy().
;;;     This function always returns a valid pointer; if memory cannot be
;;;     allocated, then a special error object is returned where all operations
;;;     on the object do nothing. You can check for this with
;;;     cairo_region_status().
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_create" cairo-region-create) cairo-region-t)

(export 'cairo-region-create)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create_rectangle ()
;;; 
;;; cairo_region_t * cairo_region_create_rectangle
;;;                                     (const cairo_rectangle_int_t *rectangle)
;;; 
;;; Allocates a new region object containing rectangle.
;;; 
;;; rectangle :
;;;     a cairo_rectangle_int_t
;;; 
;;; Returns :
;;;     A newly allocated cairo_region_t. Free with cairo_region_destroy().
;;;     This function always returns a valid pointer; if memory cannot be
;;;     allocated, then a special error object is returned where all operations
;;;     on the object do nothing. You can check for this with
;;;     cairo_region_status().
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_create_rectangles ()
;;; 
;;; cairo_region_t * cairo_region_create_rectangles
;;;                                         (const cairo_rectangle_int_t *rects,
;;;                                          int count);
;;; 
;;; Allocates a new region object containing the union of all given rects.
;;; 
;;; rects :
;;;     an array of count rectangles
;;; 
;;; count :
;;;     number of rectangles
;;; 
;;; Returns :
;;;     A newly allocated cairo_region_t. Free with cairo_region_destroy().
;;;     This function always returns a valid pointer; if memory cannot be
;;;     allocated, then a special error object is returned where all operations
;;;     on the object do nothing. You can check for this with
;;;     cairo_region_status().
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_copy ()
;;; 
;;; cairo_region_t * cairo_region_copy (const cairo_region_t *original);
;;; 
;;; Allocates a new region object copying the area from original.
;;; 
;;; original :
;;;     a cairo_region_t
;;; 
;;; Returns :
;;;     A newly allocated cairo_region_t. Free with cairo_region_destroy().
;;;     This function always returns a valid pointer; if memory cannot be
;;;     allocated, then a special error object is returned where all operations
;;;     on the object do nothing. You can check for this with
;;;    cairo_region_status().
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_reference ()
;;; 
;;; cairo_region_t * cairo_region_reference (cairo_region_t *region);
;;; 
;;; Increases the reference count on region by one. This prevents region from
;;; being destroyed until a matching call to cairo_region_destroy() is made.
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; Returns :
;;;     the referenced cairo_region_t.
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_destroy ()
;;; 
;;; void cairo_region_destroy (cairo_region_t *region);
;;; 
;;; Destroys a cairo_region_t object created with cairo_region_create(),
;;; cairo_region_copy(), or or cairo_region_create_rectangle().
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_status ()
;;; 
;;; cairo_status_t cairo_region_status (const cairo_region_t *region);
;;; 
;;; Checks whether an error has previous occurred for this region object.
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_get_extents ()
;;; 
;;; void cairo_region_get_extents (const cairo_region_t *region,
;;;                                cairo_rectangle_int_t *extents);
;;; 
;;; Gets the bounding rectangle of region as a cairo_rectangle_int_t
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; extents :
;;;     rectangle into which to store the extents
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_num_rectangles ()
;;; 
;;; int cairo_region_num_rectangles (const cairo_region_t *region);
;;; 
;;; Returns the number of rectangles contained in region.
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; Returns :
;;;     The number of rectangles contained in region.
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_get_rectangle ()
;;; 
;;; void cairo_region_get_rectangle (const cairo_region_t *region,
;;;                                  int nth,
;;;                                  cairo_rectangle_int_t *rectangle);
;;; 
;;; Stores the nth rectangle from the region in rectangle.
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; nth :
;;;     a number indicating which rectangle should be returned
;;; 
;;; rectangle :
;;;     return location for a cairo_rectangle_int_t
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_is_empty ()
;;; 
;;; cairo_bool_t cairo_region_is_empty (const cairo_region_t *region);
;;; 
;;; Checks whether region is empty.
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; Returns :
;;;     TRUE if region is empty, FALSE if it isn't.
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_contains_point ()
;;; 
;;; cairo_bool_t cairo_region_contains_point (const cairo_region_t *region,
;;;                                           int x,
;;;                                           int y);
;;; 
;;; Checks whether (x, y) is contained in region.
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; x :
;;;     the x coordinate of a point
;;; 
;;; y :
;;;     the y coordinate of a point
;;; 
;;; Returns :
;;;     TRUE if (x, y) is contained in region, FALSE if it is not.
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_region_overlap_t
;;; 
;;; typedef enum {
;;;     CAIRO_REGION_OVERLAP_IN,        /* completely inside region */
;;;     CAIRO_REGION_OVERLAP_OUT,        /* completely outside region */
;;;     CAIRO_REGION_OVERLAP_PART        /* partly inside region */
;;; } cairo_region_overlap_t;
;;; 
;;; Used as the return value for cairo_region_contains_rectangle().
;;; 
;;; CAIRO_REGION_OVERLAP_IN
;;;     The contents are entirely inside the region
;;; 
;;; CAIRO_REGION_OVERLAP_OUT
;;;     The contents are entirely outside the region
;;; 
;;; CAIRO_REGION_OVERLAP_PART
;;;     The contents are partially inside and partially outside the region.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_contains_rectangle ()
;;; 
;;; cairo_region_overlap_t cairo_region_contains_rectangle 
;;;                                     (const cairo_region_t *region,
;;;                                      const cairo_rectangle_int_t *rectangle)
;;; 
;;; Checks whether rectangle is inside, outside or partially contained in region
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; rectangle :
;;;     a cairo_rectangle_int_t
;;; 
;;; Returns :
;;;     CAIRO_REGION_OVERLAP_IN if rectangle is entirely inside region,
;;;     CAIRO_REGION_OVERLAP_OUT if rectangle is entirely outside region, or
;;;     CAIRO_REGION_OVERLAP_PART if rectangle is partially inside and partially
;;;     outside region.
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_equal ()
;;; 
;;; cairo_bool_t cairo_region_equal (const cairo_region_t *a,
;;;                                  const cairo_region_t *b);
;;; 
;;; Compares whether region_a is equivalent to region_b. NULL as an argument is
;;; equal to itself, but not to any non-NULL region.
;;; 
;;; a :
;;;     a cairo_region_t or NULL
;;; 
;;; b :
;;;     a cairo_region_t or NULL
;;; 
;;; Returns :
;;;     TRUE if both regions contained the same coverage, FALSE if it is not or
;;;     any region is in an error status.
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_translate ()
;;; 
;;; void cairo_region_translate (cairo_region_t *region, int dx, int dy);
;;; 
;;; Translates region by (dx, dy).
;;; 
;;; region :
;;;     a cairo_region_t
;;; 
;;; dx :
;;;     Amount to translate in the x direction
;;; 
;;; dy :
;;;     Amount to translate in the y direction
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_intersect ()
;;; 
;;; cairo_status_t cairo_region_intersect (cairo_region_t *dst,
;;;                                        const cairo_region_t *other);
;;; 
;;; Computes the intersection of dst with other and places the result in dst
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; other :
;;;     another cairo_region_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_intersect_rectangle ()
;;; 
;;; cairo_status_t cairo_region_intersect_rectangle
;;;                                     (cairo_region_t *dst,
;;;                                      const cairo_rectangle_int_t *rectangle)
;;; 
;;; Computes the intersection of dst with rectangle and places the result in dst
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; rectangle :
;;;     a cairo_rectangle_int_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_subtract ()
;;; 
;;; cairo_status_t cairo_region_subtract (cairo_region_t *dst,
;;;                                       const cairo_region_t *other);
;;; 
;;; Subtracts other from dst and places the result in dst
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; other :
;;;     another cairo_region_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_subtract_rectangle ()
;;; 
;;; cairo_status_t cairo_region_subtract_rectangle
;;;                                     (cairo_region_t *dst,
;;;                                      const cairo_rectangle_int_t *rectangle)
;;; 
;;; Subtracts rectangle from dst and places the result in dst
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; rectangle :
;;;     a cairo_rectangle_int_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_union ()
;;; 
;;; cairo_status_t cairo_region_union (cairo_region_t *dst,
;;;                                    const cairo_region_t *other);
;;; 
;;; Computes the union of dst with other and places the result in dst
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; other :
;;;     another cairo_region_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_union_rectangle ()
;;; 
;;; cairo_status_t cairo_region_union_rectangle
;;;                                     (cairo_region_t *dst,
;;;                                      const cairo_rectangle_int_t *rectangle)
;;; 
;;; Computes the union of dst with rectangle and places the result in dst.
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; rectangle :
;;;     a cairo_rectangle_int_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_xor ()
;;; 
;;; cairo_status_t cairo_region_xor (cairo_region_t *dst,
;;;                                  const cairo_region_t *other);
;;; 
;;; Computes the exclusive difference of dst with other and places the result
;;; in dst. That is, dst will be set to contain all areas that are either in dst
;;; or in other, but not in both.
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; other :
;;;     another cairo_region_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_region_xor_rectangle ()
;;; 
;;; cairo_status_t cairo_region_xor_rectangle
;;;                                     (cairo_region_t *dst,
;;;                                      const cairo_rectangle_int_t *rectangle)
;;; 
;;; Computes the exclusive difference of dst with rectangle and places the
;;; result in dst. That is, dst will be set to contain all areas that are either
;;; in dst or in rectangle, but not in both.
;;; 
;;; dst :
;;;     a cairo_region_t
;;; 
;;; rectangle :
;;;     a cairo_rectangle_int_t
;;; 
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;; 
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.regions.lisp -----------------------------------------
