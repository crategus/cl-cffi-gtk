;;; ----------------------------------------------------------------------------
;;; cairo.regions.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.2 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(defcstruct cairo-region-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-region-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-region-t atdoc:*external-symbols*)
 "@version{2013-7-29}
  @begin{short}
    A @sym{cairo-region-t} structure represents a set of integer-aligned
    rectangles.
  @end{short}

  It allows set-theoretical operations like the functions
  @fun{cairo-region-union} and @fun{cairo-region-intersect} to be performed on
  them.

  Memory management of @sym{cairo-region-t} is done with the functions
  @fun{cairo-region-reference} and @fun{cairo-region-destroy}.

  Since 1.10
  @see-function{cairo-region-union}
  @see-function{cairo-region-intersect}
  @see-function{cairo-region-reference}
  @see-function{cairo-region-destroy}")

(export 'cairo-region-t)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_create" cairo-region-create)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @begin{return}
    A newly allocated @symbol{cairo-region-t}. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned where
    all operations on the object do nothing. You can check for this with
    the function @fun{cairo-region-status}.
  @end{return}
  @short{Allocates a new empty region object.}

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}
  @see-function{cairo-region-status}")

(export 'cairo-region-create)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_create_rectangle" cairo-region-create-rectangle)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t}}
  @begin{return}
    A newly allocated @symbol{cairo-region-t}. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned where
    all operations on the object do nothing. You can check for this with the
    function @fun{cairo-region-status}.
  @end{return}
  @begin{short}
    Allocates a new region object containing rectangle.
  @end{short}

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}
  @see-function{cairo-region-destroy}
  @see-function{cairo-region-status}"
  (rectangle :pointer)) ; pointer is of type cairo-rectangle-int-t

(export 'cairo-region-create-rectangle)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_copy" cairo-region-copy)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-14}
  @argument[original]{a @symbol{cairo-region-t}}
  @begin{return}
    A newly allocated @symbol{cairo-region-t}. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned where
    all operations on the object do nothing. You can check for this with the
    function @fun{cairo-region-status}.
  @end{return}
  @begin{short}
    Allocates a new region object copying the area from original.
  @end{short}

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}
  @see-function{cairo-region-status}"
  (original (:pointer (:struct cairo-region-t))))

(export 'cairo-region-copy)

;;; ----------------------------------------------------------------------------
;;; cairo_region_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_reference" cairo-region-reference)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[region]{a @symbol{cairo-region-t}}
  @return{The referenced @symbol{cairo-region-t}.}
  @begin{short}
    Increases the reference count on region by one.
  @end{short}
  This prevents region from being destroyed until a matching call to
  the function @fun{cairo-region-destroy} is made.

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_region_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_destroy" cairo-region-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[region]{a @symbol{cairo-region-t}}
  @begin{short}
    Destroys a @symbol{cairo-region-t} object created with the functions
    @fun{cairo-region-create}, @fun{cairo-region-copy}, or
    @fun{cairo-region-create-rectangle}.
  @end{short}

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-create}
  @see-function{cairo-region-copy}
  @see-function{cairo-region-create-rectangle}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_region_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_status" cairo-region-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2013-11-18}
  @argument[region]{a @symbol{cairo-region-t}}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Checks whether an error has previous occurred for this region object.
  @end{short}

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-status-t}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-status)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_intersect" cairo-region-intersect)
    cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[dst]{a @symbol{cairo-region-t}}
  @argument[other]{another @symbol{cairo-region-t}}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the intersection of @arg{dst} with @arg{other} and places the
    result in @arg{dst}.
  @end{short}

  Since 1.10
  @see-symbol{cairo-region-t}"
  (dst (:pointer (:struct cairo-region-t)))
  (other (:pointer (:struct cairo-region-t))))

(export 'cairo-region-intersect)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_union" cairo-region-union) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[dst]{a @symbol{cairo-region-t}}
  @argument[other]{another @symbol{cairo-region-t}}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the union of @arg{dst} with @arg{other} and places the result in
    @arg{dst}.
  @end{short}

  Since 1.10
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-status-t}"
  (dest (:pointer (:struct cairo-region-t)))
  (other (:pointer (:struct cairo-region-t))))

(export 'cairo-region-union)

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
