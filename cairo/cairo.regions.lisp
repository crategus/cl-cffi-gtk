;;; ----------------------------------------------------------------------------
;;; cairo.regions.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;;     Representing a pixel-aligned area
;;;
;;; Types and Values
;;;
;;;     cairo_region_t
;;;     cairo_region_overlap_t
;;;
;;; Functions
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
;;; enum cairo_region_overlap_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-region-overlap-t
  :in
  :out
  :part)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-region-overlap-t atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'cairo-region-overlap-t atdoc:*external-symbols*)
 "@version{2020-12-14}
  @begin{short}
    Used as the return value for the function
    @fun{cairo-region-contains-rectangle}.
  @end{short}
  @begin[code]{table}
    @entry[:in]{The contents are entirely inside the region.}
    @entry[:out]{The contents are entirely outside the region.}
    @entry[:part]{The contents are partially inside and partially outside the
      region.}
  @end{table}
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-contain-rectangle}")

(export 'cairo-region-overlap-t)

;;; ----------------------------------------------------------------------------
;;; cairo_region_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-region-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-region-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-region-t atdoc:*external-symbols*)
 "@version{2020-12-14}
  @begin{short}
    A @sym{cairo-region-t} structure represents a set of integer-aligned
    rectangles.
  @end{short}
  It allows operations like the functions @fun{cairo-region-union} and
  @fun{cairo-region-intersect} to be performed on them.

  Memory management of @sym{cairo-region-t} is done with the functions
  @fun{cairo-region-reference} and @fun{cairo-region-destroy}.
  @see-symbol{cairo-region-t}
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
 "@version{2020-12-14}
  @begin{return}
    A newly allocated @symbol{cairo-region-t} instance. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned where
    all operations on the object do nothing. You can check for this with
    the function @fun{cairo-region-status}.
  @end{return}
  @short{Allocates a new empty region object.}
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
 "@version{2020-12-14}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t} structure}
  @begin{return}
    A newly allocated @symbol{cairo-region-t} instance. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned where
    all operations on the object do nothing. You can check for this with the
    function @fun{cairo-region-status}.
  @end{return}
  @begin{short}
    Allocates a new region object containing rectangle.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}
  @see-function{cairo-region-destroy}
  @see-function{cairo-region-status}"
  (rectangle :pointer)) ; pointer to type cairo-rectangle-int-t

(export 'cairo-region-create-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_create_rectangles ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_create_rectangles" cairo-region-create-rectangles)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-14}
  @argument[rects]{an array of count @symbol{cairo-rectangle-int-t} structures}
  @argument[count]{an integer with the number of rectangles}
  @begin{return}
    A newly allocated @symbol{cairo-region-t} instance. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned
    where all operations on the object do nothing. You can check for this with
    the function @fun{cairo-region-status}.
  @end{return}
  @begin{short}
    Allocates a new region object containing the union of all given @arg{rects}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}
  @see-function{cairo-region-destroy}
  @see-function{cairo-status}"
  (rects (:pointer (:pointer (:struct cairo-rectangle-int-t))))
  (count :int))

(export 'cairo-region-create-rectangles)

;;; ----------------------------------------------------------------------------
;;; cairo_region_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_copy" cairo-region-copy)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-14}
  @argument[original]{a @symbol{cairo-region-t} instance}
  @begin{return}
    A newly allocated @symbol{cairo-region-t} instance. Free with the function
    @fun{cairo-region-destroy}. This function always returns a valid pointer;
    if memory cannot be allocated, then a special error object is returned
    where all operations on the object do nothing. You can check for this with
    the function @fun{cairo-region-status}.
  @end{return}
  @begin{short}
    Allocates a new region object copying the area from original.
  @end{short}
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
 "@version{2020-12-14}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @return{The referenced @symbol{cairo-region-t} instance.}
  @begin{short}
    Increases the reference count on region by one.
  @end{short}
  This prevents region from being destroyed until a matching call to
  the function @fun{cairo-region-destroy} is made.
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_region_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_destroy" cairo-region-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-14}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @begin{short}
    Destroys a @symbol{cairo-region-t} instance created with the functions
    @fun{cairo-region-create}, @fun{cairo-region-copy}, or
    @fun{cairo-region-create-rectangle}.
  @end{short}
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
 "@version{2020-12-14}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Checks whether an error has previous occurred for this region object.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-status-t}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-status)

;;; ----------------------------------------------------------------------------
;;; cairo_region_get_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_get_extents" cairo-region-get-extents) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-14}
  @argument[regon]{a @symbol{cairo-region-t} instance}
  @argument[extents]{a @symbol{cairo-rectangle-int-t} instance into which to
    store the extents}
  @begin{short}
    Gets the bounding rectangle of region as a @symbol{cairo-rectangle-int-t}
    instance.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangel-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (extents (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-get-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_region_num_rectangles ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_num_rectangles" cairo-region-num-rectangles) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @return{An integer with the number of rectangles contained in @arg{region}.}
  @begin{short}
    Returns the number of rectangles contained in @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-num-rectangles)

;;; ----------------------------------------------------------------------------
;;; cairo_region_get_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_get_rectangle" cairo-region-get-rectangle) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[nth]{an integer number indicating which rectangle should be
    returned}
  @argument[rectangle]{return location for a @symbol{cairo-rectangle-int-t}}
  @begin{short}
    Stores the nth rectangle from the region in @arg{rectangle}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (nth :int)
  (rectangle (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-get-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_is_empty ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_is_empty" cairo-region-is-empty) cairo-bool-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @return{@em{True} if @arg{region} is empty, @em{false} if it is not.}
  @short{Checks whether @arg{region} is empty.}
  @see-symbol{cairo-region-t}"
  (region (:pointer (:struct cairo-region-t))))

(export 'cairo-region-is-empty)

;;; ----------------------------------------------------------------------------
;;; cairo_region_contains_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_contains_point" cairo-region-contains-point)
    cairo-bool-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[x]{an integer with the x coordinate of a point}
  @argument[y]{an integer with the y coordinate of a point}
  @begin{return}
    @em{True} if (x, y) is contained in @arg{region}, @em{false} if it is not.
  @end{return}
  @short{Checks whether (x, y) is contained in @arg{region}.}
  @see-symbol{cairo-region-t}"
  (region (:pointer (:struct cairo-region-t)))
  (x :int)
  (y :int))

(export 'cairo-region-contains-point)

;;; ----------------------------------------------------------------------------
;;; cairo_region_contains_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_contains_rectangle" cairo-region-contains-rectangle)
    cairo-region-overlap-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t} instance}
  @begin{return}
    @code{:in} if @arg{rectangle} is entirely inside @arg{region}, @code{:out}
    if @arg{rectangle} is entirely outside @arg{region}, or @code{:part} if
    @arg{rectangle} is partially inside and partially outside @arg{region}.
  @end{return}
  @begin{short}
    Checks whether @arg{rectangle} is inside, outside or partially contained in
    @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (rectangle (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-contains-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_equal" cairo-region-equal) cairo-bool-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region-a]{a @symbol{cairo-region-t} instance}
  @argument[region-b]{a @symbol{cairo-region-t} instance}
  @begin{return}
    @em{True} if both regions contained the same coverage, @em{false} if it is
    not or any region is in an error status.
  @end{return}
  @begin{short}
    Compares whether @arg{region-a} is equivalent to @arg{region-b}. NULL as an
    argument is equal to itself, but not to any non-NULL region.
  @end{short}
  @see-symbol{cairo-region-t}"
  (region-a (:pointer (:struct cairo-region-t)))
  (region-b (:pointer (:struct cairo-region-t))))

(export 'cairo-region-equal)

;;; ----------------------------------------------------------------------------
;;; cairo_region_translate ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_translate" cairo-region-translate) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[dx]{an integer with the amount to translate in the x direction}
  @argument[dy]{an integer with the amount to translate in the y direction}
  @short{Translates region by (dx, dy).}
  @see-symbol{cairo-region-t}"
  (region (:pointer (:struct cairo-region-t)))
  (dx :int)
  (dy :int))

(export 'cairo-region-translate)

;;; ----------------------------------------------------------------------------
;;; cairo_region_intersect ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_intersect" cairo-region-intersect)
    cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[other]{another @symbol{cairo-region-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the intersection of @arg{region} with @arg{other} and places the
    result in @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-status-t}"
  (region (:pointer (:struct cairo-region-t)))
  (other (:pointer (:struct cairo-region-t))))

(export 'cairo-region-intersect)

;;; ----------------------------------------------------------------------------
;;; cairo_region_intersect_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_intersect_rectangle" cairo-region-intersect-rectangle)
    cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the intersection of @arg{region} with @arg{rectangle} and places
    the result in @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (rectangle (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-intersect-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_subtract ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_subtract" cairo-region-subtract) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[other]{another @symbol{cairo-region-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Subtracts @arg{other} from @arg{region} and places the result in
    @arg{region.}
  @end{short}
  @see-symbol{cairo-region-t}"
  (region (:pointer (:struct cairo-region-t)))
  (other (:pointer (:struct cairo-region-t))))

(export 'cairo-region-subtract)

;;; ----------------------------------------------------------------------------
;;; cairo_region_subtract_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_subtract_rectangle" cairo-region-subtract-rectangle)
    cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Subtracts @arg{rectangle} from @arg{region} and places the result in
    @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (rectangle (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-subtract-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_union ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_union" cairo-region-union) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[other]{another @symbol{cairo-region-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the union of @arg{region} with @arg{other} and places the result
    in @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-status-t}"
  (region (:pointer (:struct cairo-region-t)))
  (other (:pointer (:struct cairo-region-t))))

(export 'cairo-region-union)

;;; ----------------------------------------------------------------------------
;;; cairo_region_union_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_union_rectangle" cairo-region-union-rectangle)
    cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the union of @arg{region} with @arg{rectangle} and places the
    result in @arg{region}.
  @end{short}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (rectangle (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-union-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_region_xor ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_xor" cairo-region-xor) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[other]{another @symbol{cairo-region-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the exclusive difference of @arg{region} with @arg{other} and
    places the result in @arg{region}.
  @end{short}
  That is, @arg{region} will be set to contain all areas that are either in
  @arg{region} or in @arg{other}, but not in both.
  @see-symbol{cairo-region-t}"
  (region (:pointer (:struct cairo-region-t)))
  (other (:pointer (:struct cairo-region-t))))

(export 'cairo-region-xor)

;;; ----------------------------------------------------------------------------
;;; cairo_region_xor_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_region_xor_rectangle" cairo-region-xor-rectangle)
    cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[region]{a @symbol{cairo-region-t} instance}
  @argument[rectangle]{a @symbol{cairo-rectangle-int-t} instance}
  @return{@code{:success} or @code{:no-memory}}
  @begin{short}
    Computes the exclusive difference of @arg{region} with @arg{rectangle} and
    places the result in @arg{region}.
  @end{short}
  That is, @arg{region} will be set to contain all areas that are either in
  @arg{region} or in @arg{rectangle}, but not in both.
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-int-t}"
  (region (:pointer (:struct cairo-region-t)))
  (rectangle (:pointer (:struct cairo-rectangle-int-t))))

(export 'cairo-region-xor-rectangle)

;;; --- End of file cairo.regions.lisp -----------------------------------------
