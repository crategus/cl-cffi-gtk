;;; ----------------------------------------------------------------------------
;;; cairo.pattern.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;; cairo_pattern_t
;;;
;;;     Sources for drawing
;;;
;;; Types and Values
;;;
;;;     cairo_pattern_t
;;;     cairo_extend_t
;;;     cairo_filter_t
;;;     cairo_pattern_type_t
;;;
;;; Functions
;;;
;;;     cairo_pattern_add_color_stop_rgb
;;;     cairo_pattern_add_color_stop_rgba
;;;     cairo_pattern_get_color_stop_count
;;;     cairo_pattern_get_color_stop_rgba
;;;     cairo_pattern_create_rgb
;;;     cairo_pattern_create_rgba
;;;     cairo_pattern_get_rgba
;;;     cairo_pattern_create_for_surface
;;;     cairo_pattern_get_surface
;;;     cairo_pattern_create_linear
;;;     cairo_pattern_get_linear_points
;;;     cairo_pattern_create_radial
;;;     cairo_pattern_get_radial_circles
;;;     cairo_pattern_create_mesh
;;;
;;;     cairo_mesh_pattern_begin_patch
;;;     cairo_mesh_pattern_end_patch
;;;     cairo_mesh_pattern_move_to
;;;     cairo_mesh_pattern_line_to
;;;     cairo_mesh_pattern_curve_to
;;;     cairo_mesh_pattern_set_control_point
;;;     cairo_mesh_pattern_set_corner_color_rgb
;;;     cairo_mesh_pattern_set_corner_color_rgba
;;;     cairo_mesh_pattern_get_patch_count
;;;     cairo_mesh_pattern_get_path
;;;     cairo_mesh_pattern_get_control_point
;;;     cairo_mesh_pattern_get_corner_color_rgba
;;;
;;;     cairo_pattern_reference
;;;     cairo_pattern_destroy
;;;     cairo_pattern_status
;;;
;;;     cairo_pattern_set_extend
;;;     cairo_pattern_get_extend
;;;     cairo_pattern_set_filter
;;;     cairo_pattern_get_filter
;;;     cairo_pattern_set_matrix
;;;     cairo_pattern_get_matrix
;;;
;;;     cairo_pattern_get_type
;;;     cairo_pattern_get_reference_count
;;;     cairo_pattern_set_user_data
;;;     cairo_pattern_get_user_data
;;;
;;; Description
;;;
;;; cairo_pattern_t is the paint with which cairo draws. The primary use of
;;; patterns is as the source for all cairo drawing operations, although they
;;; can also be used as masks, that is, as the brush too.
;;;
;;; A cairo pattern is created by using one of the many constructors, of the
;;; form cairo_pattern_create_type() or implicitly through
;;; cairo_set_source_type() functions.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-pattern-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-pattern-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-pattern-t atdoc:*external-symbols*)
 "@version{2020-12-25}
  @begin{short}
    A @sym{cairo-pattern-t} structure represents a source when drawing onto a
    surface.
  @end{short}
  There are different subtypes of @sym{cairo-pattern-t} structures, for
  different types of sources; for example, the function
  @fun{cairo-pattern-create-rgb} creates a pattern for a solid opaque color.

  Other than various @code{cairo-pattern-create-type} functions, some of the
  pattern types can be implicitly created using various
  @code{cairo-set-source-type} functions; for example the function
  @fun{cairo-set-source-rgb}.

  The type of a pattern can be queried with the function
  @fun{cairo-pattern-get-type}.

  Memory management of the @sym{cairo-pattern-t} structure is done with the
  functions @fun{cairo-pattern-reference} and @fun{cairo-pattern-destroy}.
  @see-function{cairo-pattern-create-rgb}
  @see-function{cairo-set-source-rgb}
  @see-function{cairo-pattern-reference}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-get-type}")

(export 'cairo-pattern-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_extend_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-extend-t
  :none
  :repeat
  :reflect
  :pad)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-extend-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-extend-t atdoc:*external-symbols*)
 "@version{2020-12-12}
  @begin{short}
    The @sym{cairo-extend-t} enumeration is used to describe how pattern
    color/alpha will be determined for areas \"outside\" the pattern's natural
    area, for example, outside the surface bounds or outside the gradient
    geometry.
  @end{short}

  Mesh patterns are not affected by the extend mode.

  The default extend mode is @code{:none} for surface patterns and @code{:pad}
  for gradient patterns.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-extend-t
  :none
  :repeat
  :reflect
  :pad)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Pixels outside of the source pattern are fully transparent.}
    @entry[:repeat]{The pattern is tiled by repeating.}
    @entry[:reflect]{The pattern is tiled by reflecting at the edges.}
    @entry[:pad]{Pixels outside of the pattern copy the closest pixel from the
      source.}
  @end{table}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-set-extend}
  @see-function{cairo-pattern-get-extend}")

(export 'cairo-extend-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_filter_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-filter-t
  :fast
  :good
  :best
  :nearest
  :bilinear
  :gaussian)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-filter-t atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'cairo-filter-t atdoc:*external-symbols*)
 "@version{2020-12-12}
  @begin{short}
    The @sym{cairo-filter-t} enumeration is used to indicate what filtering
    should be applied when reading pixel values from patterns.
  @end{short}
  See the function @fun{cairo-pattern-set-filter} for indicating the desired
  filter to be used with a particular pattern.
  @begin{pre}
(defcenum cairo-filter-t
  :fast
  :good
  :best
  :nearest
  :bilinear
  :gaussian)
  @end{pre}
  @begin[code]{table}
    @entry[:fast]{A high-performance filter, with quality similar to
      @code{:nearest}.}
    @entry[:good]{A reasonable-performance filter, with quality similar to
      @code{:bilinear}.}
    @entry[:best]{The highest-quality available, performance may not be
      suitable for interactive use.}
    @entry[:nearest]{Nearest-neighbor filtering.}
    @entry[:bilinear]{Linear interpolation in two dimensions.}
    @entry[:gaussian]{This filter value is currently unimplemented, and should
      not be used in current code.}
  @end{table}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-set-filter}
  @see-function{cairo-pattern-get-filter}")

(export 'cairo-filter-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_pattern_type_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-pattern-type-t
  :solid
  :surface
  :linear
  :radial
  :mesh
  :raster-source)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-pattern-type-t atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'cairo-pattern-type-t atdoc:*external-symbols*)
 "@version{2020-12-12}
  @begin{short}
    The @sym{cairo-pattern-type-t} enumeration is used to describe the type of
    a given pattern.
  @end{short}

  The type of a pattern is determined by the function used to create it. The
  functions @fun{cairo-pattern-create-rgb} and @fun{cairo-pattern-create-rgba}
  create @code{:solid} patterns. The remaining @sym{cairo-pattern-create}
  functions map to pattern types in obvious ways.

  The pattern type can be queried with the function
  @fun{cairo-pattern-get-type}.

  Most Cairo pattern functions can be called with a pattern of any type,
  (though trying to change the extend or filter for a solid pattern will have
  no effect). A notable exception are the functions
  @fun{cairo-pattern-add-color-stop-rgb} and
  @fun{cairo-pattern-add-color-stop-rgba} which must only be called with
  gradient patterns (either @code{:linear} or @code{:radial}). Otherwise the
  pattern will be shutdown and put into an error state.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-pattern-type-t
  :solid
  :surface
  :linear
  :radial
  :mesh
  :raster-source)
  @end{pre}
  @begin[code]{table}
    @entry[:solid]{The pattern is a solid (uniform) color. It may be opaque or
      translucent.}
    @entry[:surface]{The pattern is a based on a surface (an image).}
    @entry[:linear]{The pattern is a linear gradient.}
    @entry[:radial]{The pattern is a radial gradient.}
    @entry[:mesh]{The pattern is a mesh.}
    @entry[:raster-source]{The pattern is a user pattern providing raster data.}
  @end{table}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-create-rgb}
  @see-function{cairo-pattern-create-rgba}
  @see-function{cairo-pattern-get-type}
  @see-function{cairo-pattern-add-color-stop-rgb}
  @see-function{cairo-pattern-add-color-stop-rgba}")

(export 'cairo-pattern-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_add_color_stop_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_add_color_stop_rgb" %cairo-pattern-add-color-stop-rgb)
    :void
  (pattern (:pointer (:struct cairo-pattern-t)))
  (offset :double)
  (red :double)
  (green :double)
  (blue :double))

(defun cairo-pattern-add-color-stop-rgb (pattern offset red green blue)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[offset]{a double float with an offset in the range [0.0 .. 1.0]}
  @argument[red]{a double float red component of color}
  @argument[green]{a double float green component of color}
  @argument[blue]{a double float blue component of color}
  @begin{short}
    Adds an opaque color stop to a gradient pattern.
  @end{short}
  The offset specifies the location along the gradient's control vector. For
  example, a linear gradient's control vector is from (x0,y0) to (x1,y1) while
  a radial gradient's control vector is from any point on the start circle to
  the corresponding point on the end circle.

  The color is specified in the same way as in the function
  @fun{cairo-set-source-rgb}.

  If two (or more) stops are specified with identical offset values, they will
  be sorted according to the order in which the stops are added, stops added
  earlier will compare less than stops added later. This can be useful for
  reliably making sharp color transitions instead of the typical blend.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a gradient pattern, e.g. a linear or radial pattern,
    then @arg{pattern} will be put into an error status with a status of
    @code{:pattern-type-mismatch}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-set-source-rgb}"
  (%cairo-pattern-add-color-stop-rgb pattern
                                     (coerce offset 'double-float)
                                     (coerce red 'double-float)
                                     (coerce green 'double-float)
                                     (coerce blue 'double-float)))

(export 'cairo-pattern-add-color-stop-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_add_color_stop_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_add_color_stop_rgba"
          %cairo-pattern-add-color-stop-rgba) :void
  (pattern (:pointer (:struct cairo-pattern-t)))
  (offset :double)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defun cairo-pattern-add-color-stop-rgba (pattern offset red green blue alpha)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-12}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[offset]{a double float with an offset in the range [0.0 .. 1.0]}
  @argument[red]{a double float red component of color}
  @argument[green]{a double float green component of color}
  @argument[blue]{a double float blue component of color}
  @argument[alpha]{a double float alpha component of color}
  @begin{short}
    Adds a translucent color stop to a gradient pattern.
  @end{short}
  The offset specifies the location along the gradient's control vector. For
  example, a linear gradient's control vector is from (x0,y0) to (x1,y1) while
  a radial gradient's control vector is from any point on the start circle to
  the corresponding point on the end circle.

  The color is specified in the same way as in the function
  @fun{cairo-set-source-rgba}.

  If two or more stops are specified with identical offset values, they will
  be sorted according to the order in which the stops are added, stops added
  earlier will compare less than stops added later. This can be useful for
  reliably making sharp color transitions instead of the typical blend.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a gradient pattern, e.g. a linear or radial pattern,
    then @arg{pattern} will be put into an error status with a status of
    @code{:pattern-type-mismatch}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-set-source-rgba}"
  (%cairo-pattern-add-color-stop-rgba pattern
                                      (coerce offset 'double-float)
                                      (coerce red 'double-float)
                                      (coerce green 'double-float)
                                      (coerce blue 'double-float)
                                      (coerce alpha 'double-float)))

(export 'cairo-pattern-add-color-stop-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_color_stop_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_color_stop_count"
           %cairo-pattern-get-color-stop-count) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (count (:pointer :int)))

(defun cairo-pattern-get-color-stop-count (cr)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{count} -- an integer with the number of color stops @br{}
    @code{status} -- @code{:sucess}, or @code{:pattern-type-mismatch} if
    @arg{pattern} is not a gradient pattern
  @end{return}
  @begin{short}
    Gets the number of color stops specified in the given gradient pattern.
  @end{short}
  @see-symbol{cairo-pattern-t}"
  (with-foreign-object (count :int)
    (let ((status (%cairo-pattern-get-color-stop-count cr count)))
      (values (mem-ref count :int))
              status)))

(export 'cairo-pattern-get-color-stop-count)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_color_stop_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_color_stop_rgba"
          %cairo-pattern-get-color-stop-rgba) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (index :int)
  (offset (:pointer :double))
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

(defun cairo-pattern-get-color-stop-rgba (pattern index)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[index]{an integer with the index of the stop to return data for}
  @begin{return}
    @code{offset} -- a double float with the offset of the stop @br{}
    @code{red} -- a double float red component of color @br{}
    @code{green} -- a double float green component of color @br{}
    @code{blue} -- a double float blue component of color @br{}
    @code{alpha} -- a double float alpha component of color @br{}
    @code{status} -- @code{:success}, or @code{:invalid-index} if @arg{index}
    is not valid for the given pattern, if the pattern is not a gradient
    pattern, @code{:pattern-type-mismatch} is returned
  @end{return}
  @begin{short}
    Gets the color and offset information at the given index for a gradient
    pattern.
  @end{short}
  Values of index are 0 to 1 less than the number returned by the function
  @fun{cairo-pattern-get-color-stop-count}.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-get-color-stop-count}"
  (with-foreign-objects ((offset :double)
                         (red :double)
                         (green :double)
                         (blue :double)
                         (alpha :double))
    (let ((status (%cairo-pattern-get-color-stop-rgba pattern
                                                      index
                                                      offset
                                                      red green blue alpha)))
      (values (mem-ref offset :double)
              (mem-ref red :double)
              (mem-ref green :double)
              (mem-ref blue :double)
              (mem-ref alpha :double)
              status))))

(export 'cairo-pattern-get-color-stop-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_rgb" %cairo-pattern-create-rgb)
    (:pointer (:struct cairo-pattern-t))
  (red :double)
  (green :double)
  (blue :double))

(defun cairo-pattern-create-rgb (red green blue)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[red]{a double float red component of the color}
  @argument[green]{a double float green component of the color}
  @argument[blue]{a double float blue component of the color}
  @begin{return}
    The newly created @sym{cairo-pattern-t} instance if successful, or an error
    pattern in case of no memory. The caller owns the returned object and should
    call the function @fun{cairo-pattern-destroy} when finished with it. This
    function will always return a valid pointer, but if an error occurred the
    pattern status will be set to an error. To inspect the status of a pattern
    use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Creates a new @sym{cairo-pattern-t} instance corresponding to an opaque
    color.
  @end{short}
  The color components are floating point numbers in the range 0 to 1. If the
  values passed in are outside that range, they will be clamped.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}"
  (%cairo-pattern-create-rgb (coerce red 'double-float)
                             (coerce green 'double-float)
                             (coerce blue 'double-float)))

(export 'cairo-pattern-create-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_rgba" %cairo-pattern-create-rgba)
    (:pointer (:struct cairo-pattern-t))
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defun cairo-pattern-create-rgba (red green blue alpha)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-26}
  @argument[red]{a double float red component of the color}
  @argument[green]{a double float green component of the color}
  @argument[blue]{a double float blue component of the color}
  @argument[alpha]{a double float alpha component of the color}
  @begin{return}
    The newly created @symbol{cairo-pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned object and
    should call the function @fun{cairo-pattern-destroy} when finished with it.
    This function will always return a valid pointer, but if an error occurred
    the pattern status will be set to an error. To inspect the status of a
    pattern use the function @fun{cairo-patern-status}.
  @end{return}
  @begin{short}
    Creates a new @symbol{cairo-pattern-t} instance corresponding to a
    translucent color.
  @end{short}
  The color components are floating point numbers in the range 0 to 1. If the
  values passed in are outside that range, they will be clamped.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}"
  (%cairo-pattern-create-rgba (coerce red 'double-float)
                              (coerce green 'double-float)
                              (coerce blue 'double-float)
                              (coerce alpha 'double-float)))

(export 'cairo-pattern-create-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_rgba" %cairo-pattern-get-rgba) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

(defun cairo-pattern-get-rgba (pattern)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{red} -- a double float red component of color @br{}
    @code{green} -- a double float green component of color @br{}
    @code{blue} -- a double float blue component of color @br{}
    @code{alpha} -- a double float alpha component of color @br{}
    @code{status} -- @code{:success}, or @code{:pattern-type-mismatch} if the
    pattern is not a solid color pattern
  @end{return}
  @begin{short}
    Gets the solid color for a solid color pattern.
  @end{short}
  @see-symbol{cairo-pattern-t}"
  (with-foreign-objects ((red :double)
                         (green :double)
                         (blue :double)
                         (alpha :double))
    (let ((status (%cairo-pattern-get-rgba pattern red green blue alpha)))
      (values (mem-ref red :double)
              (mem-ref green :double)
              (mem-ref blue :double)
              (mem-ref alpha :double)
              (mem-ref alpha :double)
              status))))

(export 'cairo-pattern-get-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_for_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_for_surface" cairo-pattern-create-for-surface)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-12}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @begin{return}
    The newly created @symbol{cairo-pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned object and
    should call the function @fun{cairo-pattern-destroy} when finished with it.
    This function will always return a valid pointer, but if an error occurred
    the pattern status will be set to an error. To inspect the status of a
    pattern use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Create a new @symbol{cairo-pattern-t} instance for the given surface.
  @end{short}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-pattern-create-for-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_surface" %cairo-pattern-get-surface) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (surface (:pointer (:pointer (:struct cairo-surface-t)))))

(defun cairo-pattern-get-surface (pattern)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{surface} -- a @symbol{cairo-surface-t} instance of the pattern @br{}
    @code{status} -- @code{:success}, or @code{:pattern-type-mismatch} if the
    pattern is not a surface pattern
  @end{return}
  @begin{short}
    Gets the surface of a surface pattern.
  @end{short}
  The reference returned in @arg{surface} is owned by the pattern. The caller
  should call the function @fun{cairo-surface-reference} if the surface is to
  be retained.
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-reference}"
  (with-foreign-object (surface '(:pointer (:struct cairo-surface-t)))
    (let ((status (%cairo-pattern-get-surface pattern surface)))
      (values surface status))))

(export 'cairo-pattern-get-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_linear ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_linear" %cairo-pattern-create-linear)
    (:pointer (:struct cairo-pattern-t))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun cairo-pattern-create-linear (x0 y0 x1 y1)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[x0]{a double float x coordinate of the start point}
  @argument[y0]{a double float y coordinate of the start point}
  @argument[x1]{a double float x coordinate of the end point}
  @argument[y1]{a double float y coordinate of the end point}
  @begin{return}
    The newly created @symbol{cairo-pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned instance
    and should call the function @fun{cairo-pattern-destroy} when finished with
    it. This function will always return a valid pointer, but if an error
    occurred the pattern status will be set to an error. To inspect the status
    of a pattern use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Create a new linear gradient @symbol{cairo-pattern-t} instance along the
    line defined by (@arg{x0}, @arg{y0}) and (@arg{x1}, @arg{y1}).
  @end{short}
  Before using the gradient pattern, a number of color stops should be defined
  using the functions @fun{cairo-pattern-add-color-stop-rgb} or
  @fun{cairo-pattern-add-color-stop-rgba}.
  @begin[Note]{dictionary}
    The coordinates here are in pattern space. For a new pattern, pattern
    space is identical to user space, but the relationship between the spaces
    can be changed with the function @fun{cairo-pattern-set-matrix}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}
  @see-function{cairo-pattern-add-color-stop-rgb}
  @see-function{cairo-pattern-add-color-stop-rgba}
  @see-function{cairo-pattern-set-matrix}"
  (%cairo-pattern-create-linear (coerce x0 'double-float)
                                (coerce y0 'double-float)
                                (coerce x1 'double-float)
                                (coerce y1 'double-float)))

(export 'cairo-pattern-create-linear)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_linear_points ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_linear_points" %cairo-pattern-get-linear-points)
    cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (x0 (:pointer :double))
  (y0 (:pointer :double))
  (x1 (:pointer :double))
  (y1 (:pointer :double)))

(defun cairo-pattern-get-linear-points (pattern)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{x0} -- a double float x coordinate of the first point @br{}
    @code{y0} -- a double float y coordinate of the first point @br{}
    @code{y0} -- a double float x coordinate of the second point @br{}
    @code{y1} -- a double float y coordinate of the second point @br{}
    @code{status} -- @code{:success}, or @code{:pattern-type-mismatch} if
    pattern is not a linear gradient pattern
  @end{return}
  @begin{short}
    Gets the gradient endpoints for a linear gradient.
  @end{short}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}"
  (with-foreign-objects ((x0 :double) (y0 :double) (x1 :double) (y1 :double))
    (let ((status (%cairo-pattern-get-linear-points pattern x0 y0 x1 y1)))
      (values (mem-ref x0 :double)
              (mem-ref y0 :double)
              (mem-ref x1 :double)
              (mem-ref y1 :double)
              status))))

(export 'cairo-pattern-get-linear-points)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_radial ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_radial" %cairo-pattern-create-radial)
    (:pointer (:struct cairo-pattern-t))
  (cx0 :double)
  (cy0 :double)
  (radius0 :double)
  (cx1 :double)
  (cy1 :double)
  (radius1 :double))

(defun cairo-pattern-create-radial (cx0 cy0 radius0 cx1 cy1 radius1)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-12}
  @argument[cx0]{a double float x coordinate for the center of the start circle}
  @argument[cy0]{a double float y coordinate for the center of the start circle}
  @argument[radius0]{a double float  with the radius of the start circle}
  @argument[cx1]{a double float x coordinate for the center of the end circle}
  @argument[cy1]{a double float y coordinate for the center of the end circle}
  @argument[radius1]{a double float radius of the end circle}
  @begin{return}
    The newly created @symbol{cairo-pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned object and
    should call the function @fun{cairo-pattern-destroy} when finished with it.
    This function will always return a valid pointer, but if an error occurred
    the pattern status will be set to an error. To inspect the status of a
    pattern use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Creates a new radial gradient @symbol{cairo-pattern-t} instance between the
    two circles defined by (@arg{cx0}, @arg{cy0}, @arg{radius0}) and (@arg{cx1},
    @arg{cy1}, @arg{radius1}).
  @end{short}
  Before using the gradient pattern, a number of color stops should be defined
  using the functions @fun{cairo-pattern-add-color-stop-rgb} or
  @fun{cairo-pattern-add-color-stop-rgba}.
  @begin[Note]{dictionary}
    The coordinates here are in pattern space. For a new pattern, pattern
    space is identical to user space, but the relationship between the spaces
    can be changed with the function @fun{cairo-pattern-set-matrix}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}
  @see-function{cairo-pattern-add-color-stop-rgb}
  @see-function{cairo-pattern-add-color-stop-rgba}
  @see-function{cairo-pattern-set-matrix}"
  (%cairo-pattern-create-radial (coerce cx0 'double-float)
                                (coerce cy0 'double-float)
                                (coerce radius0 'double-float)
                                (coerce cx1 'double-float)
                                (coerce cy1 'double-float)
                                (coerce radius1 'double-float)))

(export 'cairo-pattern-create-radial)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_radial_circles ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_radial_circles" %cairo-pattern-get-radial-circles)
    cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (x0 (:pointer :double))
  (y0 (:pointer :double))
  (r0 (:pointer :double))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (r1 (:pointer :double)))

(defun cairo-pattern-get-radial-circles (pattern)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{x0} -- a double float x coordinate of the center of the first circle
    @br{}
    @code{y0} -- a double float y coordinate of the center of the first circle
    @br{}
    @code{r0} -- a double float radius of the first circle  @br{}
    @code{x1} -- a double float x coordinate of the center of the second circle
    @br{}
    @code{y1} -- a double float y coordinate of the center of the second circle
    @br{}
    @code{r1} -- a double float radius of the second circle @br{}
    @code{status} -- @code{:success}, or @code{:pattern-type-mismatch} if
    pattern is not a radial gradient pattern
  @end{return}
  @begin{short}
    Gets the gradient endpoint circles for a radial gradient, each specified as
    a center coordinate and a radius.
  @end{short}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}"
  (with-foreign-objects ((x0 :double) (y0 :double) (r0 :double)
                         (x1 :double) (y1 :double) (r1 :double))
    (let ((status (%cairo-pattern-get-radial-circles pattern
                                                     x0 y0 r0 x1 y1 r1)))
      (values (mem-ref x0 :double)
              (mem-ref y0 :double)
              (mem-ref r0 :double)
              (mem-ref x1 :double)
              (mem-ref y1 :double)
              (mem-ref r1 :double)
              status))))

(export 'cairo-pattern-get-radial-circles)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_mesh ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_mesh" cairo-pattern-create-mesh)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-12}
  @begin{return}
   The newly created @symbol{cairo-pattern-t} instance if successful, or an
   error pattern in case of no memory. The caller owns the returned object and
   should call the @fun{cairo-pattern-destroy} when finished with it. This
   function will always return a valid pointer, but if an error occurred the
   pattern status will be set to an error. To inspect the status of a pattern
   use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Create a new mesh pattern.
  @end{short}

  Mesh patterns are tensor-product patch meshes (type 7 shadings in PDF). Mesh
  patterns may also be used to create other types of shadings that are special
  cases of tensor-product patch meshes such as Coons patch meshes (type 6
  shading in PDF) and Gouraud-shaded triangle meshes (type 4 and 5 shadings in
  PDF).

  Mesh patterns consist of one or more tensor-product patches, which should be
  defined before using the mesh pattern. Using a mesh pattern with a partially
  defined patch as source or mask will put the context in an error status with
  a status of @code{:invalid-mesh-construction}.

  A tensor-product patch is defined by 4 Bézier curves (side 0, 1, 2, 3) and
  by 4 additional control points (P0, P1, P2, P3) that provide further control
  over the patch and complete the definition of the tensor-product patch. The
  corner C0 is the first point of the patch.

  Degenerate sides are permitted so straight lines may be used. A zero length
  line on one side may be used to create 3 sided patches.
  @begin{pre}
      C1     Side 1       C2
       +---------------+
       |               |
       |  P1       P2  |
       |               |
Side 0 |               | Side 2
       |               |
       |               |
       |  P0       P3  |
       |               |
       +---------------+
     C0     Side 3        C3
  @end{pre}
  Each patch is constructed by first calling the function
  @fun{cairo-mesh-pattern-begin-patch}, then the function
  @fun{cairo-mesh-pattern-move-to} to specify the first point in the patch (C0).
  Then the sides are specified with calls to the functions
  @fun{cairo-mesh-pattern-curve-to} and @fun{cairo-mesh-pattern-line-to}.

  The four additional control points (P0, P1, P2, P3) in a patch can be
  specified with the function @fun{cairo-mesh-pattern-set-control-point}.

  At each corner of the patch (C0, C1, C2, C3) a color may be specified with
  the functions @fun{cairo-mesh-pattern-set-corner-color-rgb} or
  @fun{cairo-mesh-pattern-set-corner-color-rgba}. Any corner whose color is not
  explicitly specified defaults to transparent black.

  A Coons patch is a special case of the tensor-product patch where the
  control points are implicitly defined by the sides of the patch. The default
  value for any control point not specified is the implicit value for a Coons
  patch, i.e. if no control points are specified the patch is a Coons patch.

  A triangle is a special case of the tensor-product patch where the control
  points are implicitly defined by the sides of the patch, all the sides are
  lines and one of them has length 0, i.e. if the patch is specified using
  just 3 lines, it is a triangle. If the corners connected by the 0-length
  side have the same color, the patch is a Gouraud-shaded triangle.

  Patches may be oriented differently to the above diagram. For example the
  first point could be at the top left. The diagram only shows the
  relationship between the sides, corners and control points. Regardless of
  where the first point is located, when specifying colors, corner 0 will
  always be the first point, corner 1 the point between side 0 and side 1 etc.

  Calling the function @fun{cairo-mesh-pattern-end-patch} completes the current
  patch. If less than 4 sides have been defined, the first missing side is
  defined as a line from the current point to the first point of the patch (C0)
  and the other sides are degenerate lines from C0 to C0. The corners between
  the added sides will all be coincident with C0 of the patch and their color
  will be set to be the same as the color of C0.

  Additional patches may be added with additional calls to the functions
  @fun{cairo-mesh-pattern-begin-patch}/@fun{cairo-mesh-pattern-end-patch}.
  @begin{pre}
cairo_pattern_t *pattern = cairo_pattern_create_mesh ();

/* Add a Coons patch */
cairo_mesh_pattern_begin_patch (pattern);
cairo_mesh_pattern_move_to (pattern, 0, 0);
cairo_mesh_pattern_curve_to (pattern, 30, -30,  60,  30, 100, 0);
cairo_mesh_pattern_curve_to (pattern, 60,  30, 130,  60, 100, 100);
cairo_mesh_pattern_curve_to (pattern, 60,  70,  30, 130,   0, 100);
cairo_mesh_pattern_curve_to (pattern, 30,  70, -30,  30,   0, 0);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 0, 1, 0, 0);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 1, 0, 1, 0);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 2, 0, 0, 1);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 3, 1, 1, 0);
cairo_mesh_pattern_end_patch (pattern);

/* Add a Gouraud-shaded triangle */
cairo_mesh_pattern_begin_patch (pattern)
cairo_mesh_pattern_move_to (pattern, 100, 100);
cairo_mesh_pattern_line_to (pattern, 130, 130);
cairo_mesh_pattern_line_to (pattern, 130,  70);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 0, 1, 0, 0);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 1, 0, 1, 0);
cairo_mesh_pattern_set_corner_color_rgb (pattern, 2, 0, 0, 1);
cairo_mesh_pattern_end_patch (pattern)
  @end{pre}
  When two patches overlap, the last one that has been added is drawn over
  the first one.

  When a patch folds over itself, points are sorted depending on their
  parameter coordinates inside the patch. The v coordinate ranges from 0 to 1
  when moving from side 3 to side 1; the u coordinate ranges from 0 to 1 when
  going from side 0 to side 2. Points with higher v coordinate hide points
  with lower v coordinate. When two points have the same v coordinate, the one
  with higher u coordinate is above. This means that points nearer to side 1
  are above points nearer to side 3; when this is not sufficient to decide
  which point is above (for example when both points belong to side 1 or side
  3) points nearer to side 2 are above points nearer to side 0.

  For a complete definition of tensor-product patches, see the PDF
  specification (ISO32000), which describes the parametrization in detail.
  @begin[Note]{dictionary}
    The coordinates are always in pattern space. For a new pattern, pattern
    space is identical to user space, but the relationship between the spaces
    can be changed with the function @fun{cairo-pattern-set-matrix}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}
  @see-function{cairo-mesh-pattern-begin-patch}
  @see-function{cairo-mesh-pattern-end-patch}
  @see-function{cairo-mesh-pattern-move-to}
  @see-function{cairo-mesh-pattern-curve-to}
  @see-function{cairo-mesh-pattern-line-to}
  @see-function{cairo-mesh-pattern-set-control-point}
  @see-function{cairo-mesh-pattern-set-corner-color-rgb}
  @see-function{cairo-mesh-pattern-set-corner-color-rgba}
  @see-function{cairo-pattern-set-matrix}")

(export 'cairo-pattern-create-mesh)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_begin_patch ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_begin_patch" cairo-mesh-pattern-begin-patch) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-12}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{short}
    Begin a patch in a mesh pattern.
  @end{short}

  After calling this function, the patch shape should be defined with the
  functions @fun{cairo-mesh-pattern-move-to}, @fun{cairo-mesh-pattern-line-to}
  and @fun{cairo-mesh-pattern-curve-to}.

  After defining the patch, @fun{cairo-mesh-pattern-end-patch} must be called
  before using pattern as a source or mask.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{pattern} already has a current patch, it will be put into an error
    status with a status of @code{:invalid-mesh-contstruction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}
  @see-function{cairo-mesh-pattern-move-to}
  @see-function{cairo-mesh-pattern-line-to}
  @see-function{cairo-mesh-pattern-curve-to}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-mesh-pattern-begin-patch)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_end_patch ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_end_patch" cairo-mesh-pattern-end-patch) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-12}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{short}
    Indicates the end of the current patch in a mesh pattern.
  @end{short}

  If the current patch has less than 4 sides, it is closed with a straight
  line from the current point to the first point of the patch as if the function
  @fun{cairo-mesh-pattern-line-to} was used.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{pattern} has no current patch or the current patch has no current
    point, @arg{pattern} will be put into an error status with a status of
    @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}
  @see-function{cairo-mesh-pattern-line-to}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-mesh-pattern-end-patch)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_move_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_move_to" cairo-mesh-pattern-move-to) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[x]{the double float x coordinate of the new position}
  @argument[y]{the double float y coordinate of the new position}
  @begin{short}
    Define the first point of the current patch in a mesh pattern.
  @end{short}

  After this call the current point will be (x, y).
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{pattern} has no current patch or the current patch already has at least
    one side, @arg{pattern} will be put into an error status with a status of
    @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (x :double)
  (y :double))

(export 'cairo-mesh-pattern-move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_line_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_line_to" cairo-mesh-pattern-line-to) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[x]{a double float x coordinate of the end of the new line}
  @argument[y]{a double float y coordinate of the end of the new line}
  @begin{short}
    Adds a line to the current patch from the current point to position
    (@arg{x}, @arg{y}) in pattern-space coordinates.
  @end{short}

  If there is no current point before the call to the function
  @sym{cairo-mesh-pattern-line-to} this function will behave as
  @begin{pre}
(cairo-mesh-pattern-move-to pattern x y)
  @end{pre}
  After this call the current point will be (@arg{x}, @arg{y}).
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{pattern} has no current patch or the current patch already has 4 sides,
    @arg{pattern} will be put into an error status with a status of
    @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-mesh-pattern-move-to}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (x :double)
  (y :double))

(export 'cairo-mesh-pattern-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_curve_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_curve_to" cairo-mesh-pattern-curve-to) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[x1]{the double float x coordinate of the first control point}
  @argument[y1]{the double float y coordinate of the first control point}
  @argument[x2]{the double float y coordinate of the second control point}
  @argument[y2]{the double float y coordinate of the second control point}
  @argument[x3]{the double float x coordinate of the end of the curve}
  @argument[y3]{the double float y coordinate of the end of the curve}
  @begin{short}
    Adds a cubic Bézier spline to the current patch from the current point to
    position (x3, y3) in pattern-space coordinates, using (x1, y1) and (x2, y2)
    as the control points.
  @end{short}

  If the current patch has no current point before the call to the function
  @sym{cairo-mesh-pattern-curve-to}, this function will behave as if preceded
  by a call
  @begin{pre}
(cairo-mesh-pattern-move-to pattern x1 y1)
  @end{pre}
  After this call the current point will be (x3, y3).
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{pattern} has no current patch or the current patch already has 4 sides,
    @arg{pattern} will be put into an error status with a status of
    @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-mesh-pattern-move-to}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double)
  (x3 :double)
  (y3 :double))

(export 'cairo-mesh-pattern-curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_control_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_set_control_point"
           cairo-mesh-pattern-set-control-point) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[point-num]{an unsigned integer with the control point to set the
    position for}
  @argument[x]{the double float x coordinate of the control point}
  @argument[y]{the double float y coordinate of the control point}
  @begin{short}
    Set an internal control point of the current patch.
  @end{short}

  Valid values for @arg{point-num} are from 0 to 3 and identify the control
  points as explained for the function @fun{cairo-pattern-create-mesh}.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{point-num} is not valid, @arg{pattern} will be put into an error status
    with a status of @code{:invalid-index}. If @arg{pattern} has no current
    patch, @arg{pattern} will be put into an error status with a status of
    @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-create-mesh}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (point-num :uint)
  (x :double)
  (y :double))

(export 'cairo-mesh-pattern-set-control-point)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_corner_color_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_set_corner_color_rgb"
           cairo-mesh-pattern-set-corner-color-rgb) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[corner-num]{an unsigned integer with the corner to set the color
    for}
  @argument[red]{a double float red component of color}
  @argument[green]{a double float green component of color}
  @argument[blue]{a double float blue component of color}
  @begin{short}
    Sets the color of a corner of the current patch in a mesh pattern.
  @end{short}

  The color is specified in the same way as for the function
  @fun{cairo-set-source-rgb}.

  Valid values for @arg{corner-num} are from 0 to 3 and identify the corners as
  explained for the function @fun{cairo-pattern-create-mesh}.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{corner-num} is not valid, @arg{pattern} will be put into an error
    status with a status of @code{:invalid-index}. If @arg{pattern} has no
    current patch, @arg{pattern} will be put into an error status with a status
     of @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-set-source-rgb}
  @see-function{cairo-pattern-create-mesh}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (corner-num :uint)
  (red :double)
  (green :double)
  (blue :double))

(export 'cairo-mesh-pattern-set-corner-color-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_corner_color_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_set_corner_color_rgba"
           cairo-mesh-pattern-set-corner-color-rgba) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[corner-num]{an unsigned integer with the corner to set the color
    for}
  @argument[red]{a double float red component of color}
  @argument[green]{a double float green component of color}
  @argument[blue]{a double float blue component of color}
  @argument[alpha]{a double float alpha component of color}
  @begin{short}
    Sets the color of a corner of the current patch in a mesh pattern.
  @end{short}

  The color is specified in the same way as for the function
  @fun{cairo-set-source-rgba}.

  Valid values for @arg{corner-num} are from 0 to 3 and identify the corners as
  explained for the function @fun{cairo-pattern-create-mesh}.
  @begin[Note]{dictionary}
    If @arg{pattern} is not a mesh pattern then @arg{pattern} will be put into
    an error status with a status of @code{:pattern-type-mismatch}. If
    @arg{corner-num} is not valid, @arg{pattern} will be put into an error
    status with a status of @code{:invalid-index}. If @arg{pattern} has no
    current patch, @arg{pattern} will be put into an error status with a status
    of @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-set-source-rgba}
  @see-function{cairo-pattern-create-mesh}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (corner-num :uint)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(export 'cairo-mesh-pattern-set-corner-color-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_patch_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_get_patch_count"
          %cairo-mesh-pattern-get-patch-count) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (count (:pointer :uint)))

(defun cairo-mesh-pattern-get-patch-count (pattern)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{count} -- an unsigned integer with the number patches @br{}
    @code{status} -- @code{:success}, or @code{:pattern-tpye-mismatch} if
    @arg{pattern} is not a mesh pattern
  @end{return}
  @begin{short}
    Gets the number of patches specified in the given mesh pattern.
  @end{short}

  The number only includes patches which have been finished by calling the
  function @fun{cairo-mesh-pattern-end-patch}. For example it will be 0 during
  the definition of the first patch.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-mesh-pattern-end-patch}"
  (with-foreign-object (count :uint)
    (let ((status (%cairo-mesh-pattern-get-patch-count pattern count)))
      (values (mem-ref count :uint)
              status))))

(export 'cairo-mesh-pattern-get-patch-count)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_path ()
;;; ----------------------------------------------------------------------------

;; The return type is (:pointer (:struct cairo-path-t)), but at this point
;; we don't have the declaration cairo-path-t.

(defcfun ("cairo_mesh_pattern_get_path" cairo-mesh-pattern-get-path) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[patch-num]{an unsigned integer with the patch number to return data
    for}
  @begin{return}
    The path defining the patch, or a path with status @code{:invalid-index}
    if @arg{patch-num} or @arg{point-num} is not valid for @arg{pattern}. If
    @arg{pattern} is not a mesh pattern, a path with status
    @code{:pattern-type-mismatch} is returned.
  @end{return}
  @begin{short}
    Gets path defining the patch @arg{patch-num} for a mesh pattern.
  @end{short}

  @arg{patch-num} can range 0 to 1 less than the number returned by the
  function @fun{cairo-mesh-pattern-get-patch-count}.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-mesh-pattern-get-patch-count}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (patch-num :uint))

(export 'cairo-mesh-pattern-get-path)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_control_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_get_control_point"
          %cairo-mesh-pattern-get-control-point) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (patch-num :uint)
  (point-num :uint)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun cairo-mesh-pattern-get-control-point (pattern patch-num point-num)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[patch-num]{an unsigned integer with the patch number to return data
    for}
  @argument[point-num]{an unsigned integer with the control point number to
    return data for}
  @begin{return}
    @code{x} -- a double float x coordinate of the control point @br{}
    @code{y} -- a double float y coordinate of the control point @br{}
    @code{status} -- @code{:success}, or @code{:invalid-index} if
      @arg{patch-num} or @arg{point-num} is not valid for @arg{pattern}. If
      @arg{pattern} is not a mesh pattern, @code{:pattern-type-mismatch} is
      returned.
  @end{return}
  @begin{short}
    Gets the control point @arg{point-num} of patch @arg{patch-num} for a mesh
    pattern.
  @end{short}

  @arg{patch-num} can range 0 to 1 less than the number returned by the
  function @fun{cairo-mesh-pattern-get-patch-count}.

  Valid values for @arg{point-num} are from 0 to 3 and identify the control
  points as explained for the function @fun{cairo-pattern-create-mesh}.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-mesh-pattern-get-patch-count}
  @see-function{cairo-pattern-create-mesh}"
  (with-foreign-objects ((x :double) (y :double))
    (let ((status (%cairo-mesh-pattern-get-control-point pattern
                                                         patch-num
                                                         point-num
                                                         x y)))
      (values (mem-ref x :double)
              (mem-ref y :double)
              status))))

(export 'cairo-mesh-pattern-get-control-point)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_corner_color_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_get_corner_color_rgba"
          %cairo-mesh-pattern-get-corner-color-rgba) cairo-status-t
  (pattern (:pointer (:struct cairo-pattern-t)))
  (patch-num :uint)
  (corner-num :uint)
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

(defun cairo-mesh-pattern-get-corner-color-rgba (pattern patch-num corner-num)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[patch-num]{an unsigned integer with the patch number to return data
    for}
  @argument[corner-num]{an unsigned integer with the corner number to return
    data for}
  @begin{return}
    @code{red} -- a double float red component of color @br{}
    @code{green} -- a double float green component of color @br{}
    @code{blue} -- a double float blue component of color @br{}
    @code{alpha} -- a double float for alpha component of color @br{}
    @code{status} -- @code{:success}, or @code{:invalid-index} if
      @arg{patch-num} or @arg{corner-num} is not valid for @arg{pattern}. If
      @arg{pattern} is not a mesh pattern, @code{:pattern-type-mismatch} is
      returned.
  @end{return}
  @begin{short}
    Gets the color information in corner @arg{corner-num} of patch
    @arg{patch-num} for a mesh pattern.
  @end{short}

  @arg{patch-num} can range 0 to 1 less than the number returned by the
  function @fun{cairo-mesh-pattern-get-patch-count}.

  Valid values for @arg{corner-num} are from 0 to 3 and identify the corners as
  explained for the function @fun{cairo-pattern-create-mesh}.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-mesh-pattern-get-patch-count}
  @see-function{cairo-pattern-create-mesh}"
  (with-foreign-objects ((red :double)
                         (green :double)
                         (blue :double)
                         (alpha :double))
    (let ((status (%cairo-mesh-pattern-get-corner-color-rgba pattern
                                                             patch-num
                                                             corner-num
                                                             red
                                                             green
                                                             blue
                                                             alpha)))
      (values (mem-ref red :double)
              (mem-ref green :double)
              (mem-ref blue :double)
              (mem-ref alpha :double)
              status))))

(export 'cairo-mesh-pattern-get-corner-color-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_reference" cairo-pattern-reference)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @return{The referenced @symbol{cairo-pattern-t} instance.}
  @begin{short}
    Increases the reference count on @arg{pattern} by one.
  @end{short}
  This prevents @arg{pattern} from being destroyed until a matching call to
  the function @fun{cairo-pattern-destroy} is made.

  The number of references to a @symbol{cairo-pattern-t} can be get using the
  function @fun{cairo-pattern-get-reference-count}.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-get-reference-count}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_destroy" cairo-pattern-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{short}
    Decreases the reference count on @arg{pattern} by one.
  @end{short}
  If the result is zero, then @arg{pattern} and all associated resources are
  freed. See the function @fun{cairo-pattern-reference}.
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-reference}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_status" cairo-pattern-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    @code{:success}, @code{:no-memory}, @code{:invalid-matrix},
    @code{:pattern-type-mismatch}, or @code{:invalid-mesh-construction}
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this pattern.
  @end{short}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-status)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_extend ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_set_extend" cairo-pattern-set-extend) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[extend]{a value of the @symbol{cairo-extend-t} enumeration
    describing how the area outside of the pattern will be drawn}
  @begin{short}
    Sets the mode to be used for drawing outside the area of a pattern.
  @end{short}
  See the @symbol{cairo-extend-t} enumeration for details on the semantics of
  each extend strategy.

  The default extend mode is @code{:none} for surface patterns and @code{:pad}
  for gradient patterns.
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-extend-t}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (extend cairo-extend-t))

(export 'cairo-pattern-set-extend)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_extend ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_extend" cairo-pattern-get-extend) cairo-extend-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    The current extend strategy used for drawing the pattern.
  @end{return}
  @begin{short}
    Gets the current extend mode for a @arg{pattern}.
  @end{short}
  See the @symbol{cairo-extend-t} enumeration for details on the semantics of
  each extend strategy.
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-extend-t}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-get-extend)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_set_filter" cairo-pattern-set-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[filter]{a value of the @symbol{cairo-filter-t} enumeration
    describing the filter to use for resizing the pattern}
  @begin{short}
    Sets the filter to be used for resizing when using this pattern.
  @end{short}
  See the @symbol{cairo-filter-t} enumeration for details on each filter.

  Note that you might want to control filtering even when you do not have an
  explicit @symbol{cairo-pattern-t} instance, for example when using the
  function @fun{cairo-set-source-surface}. In these cases, it is convenient to
  use the function @fun{cairo-get-source} to get access to the pattern that
  Cairo creates implicitly. For example:
  @begin{pre}
(cairo-set-source-surface cr image x y)
(cairo-pattern-set-filter (cairo-get-source cr :nearest))
  @end{pre}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-filter-t}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (filter cairo-filter-t))

(export 'cairo-pattern-set-filter)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_filter" cairo-pattern-get-filter) cairo-filter-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    The current filter used for resizing the pattern.
  @end{return}
  @begin{short}
    Gets the current filter for a @arg{pattern}.
  @end{short}
  See the @symbol{cairo-filter-t} enumeration for details on each filter.
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-filter-t}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-get-filter)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_set_matrix" cairo-pattern-set-matrix) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[matrix]{a @symbol{cairo-matrix-t} instance}
  @begin{short}
    Sets the pattern's transformation matrix to @arg{matrix}.
  @end{short}
  This matrix is a transformation from user space to pattern space.

  When a pattern is first created it always has the identity matrix for its
  transformation matrix, which means that pattern space is initially identical
  to user space.

  Important: Please note that the direction of this transformation matrix is
  from user space to pattern space. This means that if you imagine the flow
  from a pattern to user space, and on to device space, then coordinates in
  that flow will be transformed by the inverse of the pattern matrix.

  For example, if you want to make a pattern appear twice as large as it does
  by default the correct code to use is:
  @begin{pre}
(cairo-matrix-init-scale matrix 0.5 0.5)
(cairo-pattern-set-matrix pattern matrix)
  @end{pre}
  Meanwhile, using values of 2.0 rather than 0.5 in the code above would cause
  the pattern to appear at half of its default size.

  Also, please note the discussion of the user-space locking semantics of the
  function @fun{cairo-set-source}.
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-matrix-t}
  @see-function{cairo-set-source}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (matrix (:pointer (:struct cairo-matrix-t))))

(export 'cairo-pattern-set-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_matrix" cairo-pattern-get-matrix) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @argument[matrix]{a @symbol{cairo-matrix-t} for the matrix}
  @begin{short}
    Stores the pattern's transformation matrix into @arg{matrix}.
  @end{short}
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-matrix-t}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (matrix (:pointer (:pointer (:struct cairo-matrix-t)))))

(export 'cairo-pattern-get-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_type" cairo-pattern-get-type) cairo-pattern-type-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-13}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @return{A value of the @symbol{cairo-pattern-type-t} enumeration.}
  @begin{short}
    This function returns the type of a pattern.
  @end{short}
  See the @symbol{cairo-pattern-type-t} enumeration for available types.
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-pattern-type-t}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-get-type)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_reference_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_get_reference_count" cairo-pattern-get-reference-count)
    :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-12-26}
  @argument[pattern]{a @symbol{cairo-pattern-t} instance}
  @begin{return}
    The current reference count of @arg{pattern}.
  @end{return}
  If the instance is a \"nil\" instance, 0 will be returned.
  @short{Returns the current reference count of @arg{pattern}.}
  @see-symbol{cairo-pattern-t}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-get-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_user_data ()
;;;
;;; cairo_status_t
;;; cairo_pattern_set_user_data (cairo_pattern_t *pattern,
;;;                              const cairo_user_data_key_t *key,
;;;                              void *user_data,
;;;                              cairo_destroy_func_t destroy);
;;;
;;; Attach user data to pattern. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_pattern_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_user_data ()
;;;
;;; void *
;;; cairo_pattern_get_user_data (cairo_pattern_t *pattern,
;;;                              const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to pattern using the specified key.
;;; If no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     The user data previously attached or NULL.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.pattern.lisp -----------------------------------------
